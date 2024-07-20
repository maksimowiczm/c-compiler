use crate::lexer::Lexer;
use crate::parser::type_qualifier::TypeQualifierList;
use crate::parser::Parse;
use crate::tokenizer::TokenKind;

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Pointer {
    Raw,
    Wrapper(Box<Pointer>),
    TypeQualifierList {
        type_qualifier_list: TypeQualifierList,
        wrapper: Option<Box<Pointer>>,
    },
}

impl Parse for Pointer {
    fn parse(lexer: &mut Lexer) -> Option<Pointer> {
        let token = lexer.borrow()?;

        if !matches!(token.kind, TokenKind::Asterisk) {
            lexer.release(1);
            return None;
        }

        let type_qualifier_list = TypeQualifierList::parse(lexer);
        let pointer = Pointer::parse(lexer);

        let out = match (type_qualifier_list, pointer) {
            (Some(type_qualifier_list), wrapper) => Pointer::TypeQualifierList {
                type_qualifier_list,
                wrapper: wrapper.map(Box::new),
            },
            (None, Some(pointer)) => Pointer::Wrapper(Box::new(pointer)),
            (None, None) => Pointer::Raw,
        };

        Some(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::type_qualifier::TypeQualifier;
    use crate::tokenizer::Token;
    use std::iter::repeat;

    #[test]
    fn parse_pointer_fail() {
        let tokens = vec![Token {
            kind: TokenKind::Identifier,
            value: "foo".to_string(),
        }];
        let mut lexer = Lexer::new(tokens.clone());
        let pointer = Pointer::parse(&mut lexer);
        assert_eq!(pointer, None);
        assert_eq!(lexer.borrow(), tokens.get(0));
    }

    macro_rules! test_pointer_parse {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let mut lexer = Lexer::new($input);
                let pointer = Pointer::parse(&mut lexer);
                assert_eq!(pointer, $expected);
                assert_eq!(lexer.borrow(), None);
            }
        };
    }

    test_pointer_parse!(
        parse_pointer,
        vec![Token {
            kind: TokenKind::Asterisk,
            value: "*".to_string(),
        }],
        Some(Pointer::Raw)
    );

    test_pointer_parse!(
        triple_pointer,
        repeat(Token {
            kind: TokenKind::Asterisk,
            value: "*".to_string(),
        })
        .take(3)
        .collect(),
        Some(Pointer::Wrapper(Box::new(Pointer::Wrapper(Box::new(
            Pointer::Raw
        )))))
    );

    test_pointer_parse!(
        parse_pointer_with_type_qualifier_list,
        vec![
            Token {
                kind: TokenKind::Asterisk,
                value: "*".to_string(),
            },
            Token {
                kind: TokenKind::Const,
                value: "const".to_string(),
            },
        ],
        Some(Pointer::TypeQualifierList {
            type_qualifier_list: TypeQualifierList {
                type_qualifier: TypeQualifier::Const,
                type_qualifier_list: None,
            },
            wrapper: None,
        })
    );

    test_pointer_parse!(
        parse_pointer_with_type_qualifier_list_and_wrapper,
        vec![
            Token {
                kind: TokenKind::Asterisk,
                value: "*".to_string(),
            },
            Token {
                kind: TokenKind::Const,
                value: "const".to_string(),
            },
            Token {
                kind: TokenKind::Asterisk,
                value: "*".to_string(),
            },
        ],
        Some(Pointer::TypeQualifierList {
            type_qualifier_list: TypeQualifierList {
                type_qualifier: TypeQualifier::Const,
                type_qualifier_list: None,
            },
            wrapper: Some(Box::new(Pointer::Raw)),
        })
    );
}
