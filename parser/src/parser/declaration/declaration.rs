use crate::lexer::Token;
use crate::parser::declaration::declaration_specifier::DeclarationSpecifier;
use crate::parser::declaration::declarator::Declarator;
use crate::parser::expression::Expression;
use crate::parser::{Parse, ParserError, Result, TryParse};
use std::iter::Peekable;

#[derive(Debug)]
pub struct Declaration {
    pub declaration_specifiers: Vec<DeclarationSpecifier>,
    pub init_declarators: Vec<InitDeclarator>,
}

impl Parse for Declaration {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let mut declaration_specifiers = vec![];
        while let Some(specifier) = DeclarationSpecifier::try_parse(tokens)? {
            declaration_specifiers.push(specifier);
        }
        if declaration_specifiers.is_empty() {
            return Err(ParserError::ExpectedDeclarationSpecifier {
                near_tokens: tokens.take(6).collect(),
            });
        }

        let mut init_declarators = vec![];
        while let Some(token) = tokens.peek() {
            match token {
                Token::SemiColon => {
                    tokens.next();
                    break;
                }
                _ => {
                    init_declarators.push(InitDeclarator::parse(tokens)?);
                }
            }
        }

        Ok(Declaration {
            declaration_specifiers,
            init_declarators,
        })
    }
}

#[derive(Debug)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub initializer: Option<Initializer>,
}

impl Parse for InitDeclarator {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let declarator = Declarator::parse(tokens)?;

        let peek = match tokens.peek() {
            Some(peek) => peek,
            None => {
                return Ok(InitDeclarator {
                    declarator,
                    initializer: None,
                })
            }
        };

        let out = match peek {
            Token::Assignment(_) => {
                tokens.next();
                let initializer = Initializer::parse(tokens)?;
                InitDeclarator {
                    declarator,
                    initializer: Some(initializer),
                }
            }
            _ => InitDeclarator {
                declarator,
                initializer: None,
            },
        };

        Ok(out)
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Initializer {
    AssignmentExpression(Expression),
    List(Vec<Initializer>),
}

impl Parse for Initializer {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let peek = match tokens.peek() {
            Some(peek) => peek,
            None => {
                return Err(ParserError::ExpectedInitializer {
                    near_tokens: tokens.take(6).collect(),
                })
            }
        };

        let out = if let Token::OpenBrace = peek {
            tokens.next();

            let mut initializer_list = vec![];
            while let Some(token) = tokens.peek() {
                match token {
                    Token::CloseBrace => {
                        tokens.next();
                        break;
                    }
                    Token::Comma => {
                        return Err(ParserError::ExpectedInitializer {
                            near_tokens: tokens.take(6).collect(),
                        })
                    }
                    _ => {
                        initializer_list.push(Initializer::parse(tokens)?);
                    }
                }

                if let Some(Token::Comma) = tokens.peek() {
                    tokens.next();
                }
            }

            Initializer::List(initializer_list)
        } else {
            Expression::assignment_expression(tokens).map(Initializer::AssignmentExpression)?
        };

        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Constant as TokenConstant, Token};
    use crate::parser;
    use crate::parser::Parse;
    use parser::constant::Constant;
    use rstest::rstest;

    #[rstest]
    #[case::assignment(
        vec![
            Token::Constant(TokenConstant::SignedInteger(1)),
        ],
        Initializer::AssignmentExpression(Expression::Constant(Constant::Integer(1)))
    )]
    #[case::list(
        vec![
            Token::OpenBrace,
            Token::Constant(TokenConstant::SignedInteger(1)),
            Token::Comma,
            Token::Constant(TokenConstant::SignedInteger(2)),
            Token::CloseBrace,
        ],
        Initializer::List(vec![
            Initializer::AssignmentExpression(Expression::Constant(Constant::Integer(1))),
            Initializer::AssignmentExpression(Expression::Constant(Constant::Integer(2))),
        ])
    )]
    #[case::list_trailing_comma(
        vec![
            Token::OpenBrace,
            Token::Constant(TokenConstant::SignedInteger(1)),
            Token::Comma,
            Token::CloseBrace,
        ],
        Initializer::List(vec![
            Initializer::AssignmentExpression(Expression::Constant(Constant::Integer(1))),
        ])
    )]
    fn test_initalizer(#[case] input: Vec<Token>, #[case] expected: Initializer) {
        let mut tokens = input.into_iter().peekable();
        let result = Initializer::parse(&mut tokens).unwrap();
        assert_eq!(result, expected);
    }
}
