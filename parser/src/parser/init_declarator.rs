use crate::lexer::Lexer;
use crate::parser::declarator::Declarator;
use crate::parser::initializer::Initializer;
use crate::parser::Parse;
use crate::tokenizer::TokenKind;

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub initializer: Option<Initializer>,
}

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct InitDeclaratorList {
    pub init_declarator_list: Option<Box<InitDeclaratorList>>,
    pub init_declarator: InitDeclarator,
}

impl Parse for InitDeclarator {
    fn parse(lexer: &mut Lexer) -> Option<InitDeclarator> {
        let declarator = Declarator::parse(lexer)?;

        let token = match lexer.borrow() {
            Some(token) => token,
            None => {
                return Some(InitDeclarator {
                    declarator,
                    initializer: None,
                })
            }
        };
        let out = match token.kind {
            TokenKind::Assign => {
                let initializer = match Initializer::parse(lexer) {
                    Some(initializer) => initializer,
                    None => {
                        //todo
                        panic!("expected initializer");
                    }
                };

                InitDeclarator {
                    declarator,
                    initializer: Some(initializer),
                }
            }
            _ => {
                lexer.release(1);
                InitDeclarator {
                    declarator,
                    initializer: None,
                }
            }
        };

        Some(out)
    }
}

impl Parse for InitDeclaratorList {
    fn parse(lexer: &mut Lexer) -> Option<InitDeclaratorList> {
        let init_declarator = InitDeclarator::parse(lexer)?;
        let init_declarator_list = match lexer.borrow() {
            Some(token) => match token.kind {
                TokenKind::Comma => InitDeclaratorList::parse(lexer).map(Box::new),
                _ => None,
            },
            None => None,
        };

        Some(InitDeclaratorList {
            init_declarator_list,
            init_declarator,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::declarator::DirectDeclarator;
    use crate::tokenizer::{Token, TokenKind};

    macro_rules! test_init_declarator_parse {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let mut lexer = Lexer::new($input);
                let declarator = InitDeclarator::parse(&mut lexer).unwrap();
                assert_eq!(declarator, $expected);
            }
        };
    }

    test_init_declarator_parse!(
        parse_identifier,
        vec![Token {
            kind: TokenKind::Identifier,
            value: "a".to_string(),
        },],
        InitDeclarator {
            declarator: Declarator {
                pointer: None,
                direct_declarator: DirectDeclarator::Identifier("a".to_string())
            },
            initializer: None,
        }
    );

    test_init_declarator_parse!(
        parse_identifier_trailing_token,
        vec![
            Token {
                kind: TokenKind::Identifier,
                value: "a".to_string(),
            },
            Token {
                kind: TokenKind::Identifier,
                value: "a".to_string(),
            },
        ],
        InitDeclarator {
            declarator: Declarator {
                pointer: None,
                direct_declarator: DirectDeclarator::Identifier("a".to_string())
            },
            initializer: None,
        }
    );
}
