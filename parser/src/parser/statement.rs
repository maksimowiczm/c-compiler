use crate::lexer::{Keyword, Token};
use crate::parser::constant::Constant;
use crate::parser::expression::Expression;
use crate::parser::{Context, Parse, ParserError};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Statement {
    Expression(Expression),
    Conditional {
        condition: Expression,
        body: Box<Statement>,
        otherwise: Option<Box<Statement>>,
    },
    Switch {
        condition: Expression,
        statement: Box<Statement>,
    },
    Labeled {
        label: Label,
        statement: Box<Statement>,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Label {
    Label(String),
    Case(Constant),
    Default,
}

type Result = std::result::Result<Statement, ParserError>;

impl Parse for Statement {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>, context: &Context) -> Result
    where
        Self: Sized,
    {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            Token::Keyword(Keyword::If) | Token::Keyword(Keyword::Switch) => {
                Statement::conditional_statement(tokens, context)?
            }
            Token::Keyword(Keyword::Case) | Token::Keyword(Keyword::Default) => {
                Statement::labeled_statement(tokens, context)?
            }
            _ => Statement::expression_statement(tokens, context)?,
        };

        Ok(out)
    }
}

impl Statement {
    fn labeled_statement(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(Keyword::Case) => {
                let constant = Constant::parse(tokens, context)?;
                Self::expect_token(tokens, Token::Colon)?;
                let statement = Box::new(Statement::parse(tokens, context)?);
                Statement::Labeled {
                    label: Label::Case(constant),
                    statement,
                }
            }
            Token::Keyword(Keyword::Default) => {
                Self::expect_token(tokens, Token::Colon)?;
                let statement = Box::new(Statement::parse(tokens, context)?);
                Statement::Labeled {
                    label: Label::Default,
                    statement,
                }
            }
            _ => unreachable!(),
        };

        Ok(out)
    }

    fn expression_statement(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            Token::SemiColon => {
                tokens.next();
                Statement::Expression(Expression::Empty)
            }
            _ => {
                let expression = Expression::parse(tokens, context)?;

                // might be labeled statement
                if let Token::Colon = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)? {
                    tokens.next();
                    let statement = Box::new(Statement::parse(tokens, context)?);
                    return Ok(Statement::Labeled {
                        label: Label::Label(
                            expression
                                .get_identifier()
                                .ok_or(ParserError::ExpectedIdentifier)?,
                        ),
                        statement,
                    });
                }

                let statement = Statement::Expression(expression);
                Self::expect_token(tokens, Token::SemiColon)?;
                statement
            }
        };

        Ok(out)
    }

    fn conditional_statement(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(Keyword::If) => {
                Self::expect_token(tokens, Token::OpenParenthesis)?;
                let condition = Expression::parse(tokens, context)?;
                Self::expect_token(tokens, Token::CloseParenthesis)?;
                let body = Box::new(Statement::parse(tokens, context)?);
                let otherwise = match tokens.peek() {
                    Some(Token::Keyword(Keyword::Else)) => {
                        tokens.next();
                        Some(Box::new(Statement::parse(tokens, context)?))
                    }
                    _ => None,
                };

                Statement::Conditional {
                    condition,
                    body,
                    otherwise,
                }
            }
            Token::Keyword(Keyword::Switch) => {
                Self::expect_token(tokens, Token::OpenParenthesis)?;
                let condition = Expression::parse(tokens, context)?;
                Self::expect_token(tokens, Token::CloseParenthesis)?;
                let statement = Box::new(Statement::parse(tokens, context)?);

                Statement::Switch {
                    condition,
                    statement,
                }
            }
            _ => unreachable!(),
        };

        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::lexer::Constant as TokenConstant;
    use super::super::expression::Expression;
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::expression(
        vec![
            Token::Word("a".to_string()),
            Token::SemiColon,
        ],
        Statement::Expression(Expression::Identifier("a".to_string()))
    )]
    #[case::empty_expression(
        vec![
            Token::SemiColon,
        ],
        Statement::Expression(Expression::Empty)
    )]
    fn test_expression_statement(#[case] input: Vec<Token>, #[case] expected: Statement) {
        let context = Context::default();
        let result =
            Statement::expression_statement(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::if_statement(
        vec![
            Token::Keyword(Keyword::If),
            Token::OpenParenthesis,
            Token::Word("a".to_string()),
            Token::CloseParenthesis,
            Token::Word("b".to_string()),
            Token::SemiColon,
        ],
        Statement::Conditional {
            condition: Expression::Identifier("a".to_string()),
            body: Box::new(Statement::Expression(Expression::Identifier("b".to_string()))),
            otherwise: None,
        }
    )]
    #[case::if_else_statement(
        vec![
            Token::Keyword(Keyword::If),
            Token::OpenParenthesis,
            Token::Word("a".to_string()),
            Token::CloseParenthesis,
            Token::Word("b".to_string()),
            Token::SemiColon,
            Token::Keyword(Keyword::Else),
            Token::Word("c".to_string()),
            Token::SemiColon,
        ],
        Statement::Conditional {
            condition: Expression::Identifier("a".to_string()),
            body: Box::new(Statement::Expression(Expression::Identifier("b".to_string()))),
            otherwise: Some(Box::new(Statement::Expression(Expression::Identifier("c".to_string())))),
        }
    )]
    #[case::if_compound(
        vec![
            Token::Keyword(Keyword::If),
            Token::OpenParenthesis,
            Token::Word("a".to_string()),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Word("b".to_string()),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::Conditional {
            condition: Expression::Identifier("a".to_string()),
            body: Box::new(Statement::Expression(Expression::Identifier("b".to_string()))),
            otherwise: None,
        }
    )]
    #[case::if_else_compound(
        vec![
            Token::Keyword(Keyword::If),
            Token::OpenParenthesis,
            Token::Word("a".to_string()),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Word("b".to_string()),
            Token::SemiColon,
            Token::CloseBrace,
            Token::Keyword(Keyword::Else),
            Token::OpenBrace,
            Token::Word("c".to_string()),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::Conditional {
            condition: Expression::Identifier("a".to_string()),
            body: Box::new(Statement::Expression(Expression::Identifier("b".to_string()))),
            otherwise: Some(Box::new(Statement::Expression(Expression::Identifier("c".to_string())))),
        }
    )]
    #[case::switch(
        vec![
            Token::Keyword(Keyword::Switch),
            Token::OpenParenthesis,
            Token::Word("a".to_string()),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::CloseBrace,
        ],
        Statement::Switch {
            condition: Expression::Identifier("a".to_string()),
            statement: Box::new(Statement::Expression(Expression::Empty)),
        }
    )]
    #[case::switch_default(
        vec![
            Token::Keyword(Keyword::Switch),
            Token::OpenParenthesis,
            Token::Word("a".to_string()),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Keyword(Keyword::Default),
            Token::Colon,
            Token::CloseBrace,
        ],
        Statement::Switch {
            condition: Expression::Identifier("a".to_string()),
            statement: Box::new(Statement::Expression(Expression::Empty)),
        }
    )]
    #[case::switch_cased(
        vec![
            Token::Keyword(Keyword::Switch),
            Token::OpenParenthesis,
            Token::Word("a".to_string()),
            Token::CloseParenthesis,
            Token::OpenBrace,
            Token::Keyword(Keyword::Case),
            Token::Word("1".to_string()),
            Token::Colon,
            Token::Keyword(Keyword::Break),
            Token::SemiColon,
            Token::Keyword(Keyword::Default),
            Token::Colon,
            Token::Keyword(Keyword::Break),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::Switch {
            condition: Expression::Identifier("a".to_string()),
            statement: Box::new(Statement::Expression(Expression::Empty)),
        }
    )]
    fn test_conditional_statement(#[case] input: Vec<Token>, #[case] expected: Statement) {
        let context = Context::default();
        let result =
            Statement::conditional_statement(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::case(
        vec![
            Token::Keyword(Keyword::Case),
            Token::Constant(TokenConstant::SignedInteger(1)),
            Token::Colon,
            Token::Word("a".to_string()),
            Token::SemiColon,
        ],
        Statement::Labeled {
            label: Label::Case(Constant::Integer(1)),
            statement: Box::new(Statement::Expression(Expression::Identifier("a".to_string()))),
        }
    )]
    #[case::default(
        vec![
            Token::Keyword(Keyword::Default),
            Token::Colon,
            Token::Word("a".to_string()),
            Token::SemiColon,
        ],
        Statement::Labeled {
            label: Label::Default,
            statement: Box::new(Statement::Expression(Expression::Identifier("a".to_string()))),
        }
    )]
    #[case::label(
        vec![
            Token::Word("label".to_string()),
            Token::Colon,
            Token::Word("a".to_string()),
            Token::SemiColon,
        ],
        Statement::Labeled {
            label: Label::Label("label".to_string()),
            statement: Box::new(Statement::Expression(Expression::Identifier("a".to_string()))),
        }
    )]
    fn test_labeled_statement(#[case] input: Vec<Token>, #[case] expected: Statement) {
        let context = Context::default();
        let result = Statement::parse(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }
}
