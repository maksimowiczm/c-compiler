use crate::lexer::Lexer;
use crate::parser::pointer::Pointer;
use crate::parser::Parse;
use crate::tokenizer::TokenKind;

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Declarator {
    pub pointer: Option<Pointer>,
    pub direct_declarator: DirectDeclarator,
}

impl Parse for Declarator {
    fn parse(lexer: &mut Lexer) -> Option<Declarator> {
        let pointer = Pointer::parse(lexer);
        let direct_declarator = DirectDeclarator::parse(lexer)?;

        Some(Declarator {
            pointer,
            direct_declarator,
        })
    }
}

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DirectDeclarator {
    Identifier(String),
    Declarator(Box<Declarator>),
}

impl Parse for DirectDeclarator {
    fn parse(lexer: &mut Lexer) -> Option<Self> {
        let token = lexer.borrow()?;

        let declarator = match token.kind {
            TokenKind::Identifier => {
                let identifier = token.value.to_string();
                lexer.consume();
                DirectDeclarator::Identifier(identifier)
            }
            TokenKind::OpenParen => {
                let declarator = Declarator::parse(lexer)?;
                let token = match lexer.borrow() {
                    Some(token) => token,
                    None => {
                        lexer.release(1);
                        return None;
                    }
                };
                if !matches!(token.kind, TokenKind::CloseParen) {
                    lexer.release(2);
                    return None;
                }

                DirectDeclarator::Declarator(Box::new(declarator))
            }
            _ => {
                lexer.release(1);
                return None;
            }
        };

        // todo [] {} ()

        Some(declarator)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::Token;

    macro_rules! test_declarator_parse {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let mut lexer = Lexer::new($input);
                let declarator = Declarator::parse(&mut lexer).unwrap();
                assert_eq!(declarator, $expected);
            }
        };
    }

    test_declarator_parse!(
        test_identifier,
        vec![Token {
            kind: TokenKind::Identifier,
            value: "a".to_string(),
        }],
        Declarator {
            pointer: None,
            direct_declarator: DirectDeclarator::Identifier("a".to_string())
        }
    );

    test_declarator_parse!(
        test_pointer_identifier,
        vec![
            Token {
                kind: TokenKind::Asterisk,
                value: "*".to_string(),
            },
            Token {
                kind: TokenKind::Identifier,
                value: "a".to_string(),
            }
        ],
        Declarator {
            pointer: Some(Pointer::Raw),
            direct_declarator: DirectDeclarator::Identifier("a".to_string())
        }
    );

    test_declarator_parse!(
        test_parenthesis,
        vec![
            Token {
                kind: TokenKind::OpenParen,
                value: "(".to_string(),
            },
            Token {
                kind: TokenKind::Identifier,
                value: "a".to_string(),
            },
            Token {
                kind: TokenKind::CloseParen,
                value: ")".to_string(),
            }
        ],
        Declarator {
            pointer: None,
            direct_declarator: DirectDeclarator::Declarator(Box::new(Declarator {
                pointer: None,
                direct_declarator: DirectDeclarator::Identifier("a".to_string())
            }))
        }
    );
}
