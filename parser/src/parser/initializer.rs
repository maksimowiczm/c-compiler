use crate::lexer::Lexer;
use crate::parser::expression::AssignmentExpression;
use crate::parser::Parse;
use crate::tokenizer::TokenKind;

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Initializer {
    AssignmentExpression(AssignmentExpression),
    InitializerList(Box<InitializerList>),
}

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct InitializerList {
    pub initializer: Initializer,
    pub initializer_list: Option<Box<InitializerList>>,
}

impl Parse for Initializer {
    fn parse(lexer: &mut Lexer) -> Option<Self> {
        let token = lexer.borrow()?;

        let out = match token.kind {
            TokenKind::OpenBrace => {
                lexer.consume();
                let list = match InitializerList::parse(lexer) {
                    Some(list) => list,
                    None => {
                        //todo
                        panic!("Expected initializer list")
                    }
                };

                let token = match lexer.borrow() {
                    Some(token) => token,
                    None => {
                        //todo
                        panic!("Expected close brace")
                    }
                };

                if let TokenKind::Comma = token.kind {
                    lexer.consume();
                }

                let token = match lexer.borrow() {
                    Some(token) => token,
                    None => {
                        //todo
                        panic!("Expected close brace")
                    }
                };

                match token.kind {
                    TokenKind::CloseBrace => {
                        lexer.consume();
                        Initializer::InitializerList(Box::new(list))
                    }
                    _ => {
                        //todo
                        panic!("Expected close brace")
                    }
                }
            }
            _ => {
                match AssignmentExpression::parse(lexer) {
                    Some(assignment_expression) => {
                        Initializer::AssignmentExpression(assignment_expression)
                    }
                    None => {
                        //todo
                        panic!("Expected assignment expression")
                    }
                }
            }
        };

        Some(out)
    }
}

impl Parse for InitializerList {
    fn parse(lexer: &mut Lexer) -> Option<Self> {
        let initializer = match Initializer::parse(lexer) {
            Some(initializer) => initializer,
            None => {
                //todo
                panic!("Expected initializer")
            }
        };

        let token = match lexer.borrow() {
            Some(token) => token,
            None => {
                lexer.release(1);
                return Some(InitializerList {
                    initializer,
                    initializer_list: None,
                });
            }
        };

        let initializer_list = match token.kind {
            TokenKind::Comma => {
                lexer.consume();
                match InitializerList::parse(lexer) {
                    Some(list) => Some(Box::new(list)),
                    None => {
                        //todo
                        panic!("Expected initializer list")
                    }
                }
            }
            _ => None,
        };

        Some(InitializerList {
            initializer,
            initializer_list,
        })
    }
}
