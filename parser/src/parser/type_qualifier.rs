use crate::lexer::Lexer;
use crate::parser::Parse;
use crate::tokenizer::TokenKind;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TypeQualifier {
    Const,
    Volatile,
}

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct TypeQualifierList {
    pub type_qualifier: TypeQualifier,
    pub type_qualifier_list: Option<Box<TypeQualifierList>>,
}

impl Parse for TypeQualifier {
    fn parse(lexer: &mut Lexer) -> Option<Self> {
        let token = lexer.borrow()?;

        let out = match &token.kind {
            TokenKind::Const => TypeQualifier::Const,
            TokenKind::Volatile => TypeQualifier::Volatile,
            _ => {
                lexer.release(1);
                return None;
            }
        };

        Some(out)
    }
}

impl Parse for TypeQualifierList {
    fn parse(lexer: &mut Lexer) -> Option<TypeQualifierList> {
        let type_qualifier = TypeQualifier::parse(lexer)?;

        let type_qualifier_list = TypeQualifierList::parse(lexer).map(Box::new);

        Some(TypeQualifierList {
            type_qualifier,
            type_qualifier_list,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::tokenizer::Token;

    #[test]
    fn parse_const() {
        let mut lexer = Lexer::new(vec![Token {
            kind: TokenKind::Const,
            value: "const".to_string(),
        }]);
        let result = TypeQualifier::parse(&mut lexer);

        assert_eq!(result, Some(TypeQualifier::Const));
        assert_eq!(lexer.borrow(), None);
    }

    #[test]
    fn parse_volatile() {
        let mut lexer = Lexer::new(vec![Token {
            kind: TokenKind::Volatile,
            value: "volatile".to_string(),
        }]);

        let result = TypeQualifier::parse(&mut lexer);

        assert_eq!(result, Some(TypeQualifier::Volatile));
        assert_eq!(lexer.borrow(), None);
    }

    #[test]
    fn parse_list() {
        let mut lexer = Lexer::new(vec![
            Token {
                kind: TokenKind::Const,
                value: "const".to_string(),
            },
            Token {
                kind: TokenKind::Volatile,
                value: "volatile".to_string(),
            },
            Token {
                kind: TokenKind::Const,
                value: "const".to_string(),
            },
        ]);

        let result = TypeQualifierList::parse(&mut lexer);

        assert_eq!(
            result,
            Some(TypeQualifierList {
                type_qualifier: TypeQualifier::Const,
                type_qualifier_list: Some(Box::new(TypeQualifierList {
                    type_qualifier: TypeQualifier::Volatile,
                    type_qualifier_list: Some(Box::new(TypeQualifierList {
                        type_qualifier: TypeQualifier::Const,
                        type_qualifier_list: None,
                    })),
                })),
            })
        );
        assert_eq!(lexer.borrow(), None);
    }
}
