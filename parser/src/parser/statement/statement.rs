use crate::lexer::{Keyword, Token};
use crate::parser::constant::Constant;
use crate::parser::expression::Expression;
use crate::parser::statement::compound_statement::CompoundStatement;
use crate::parser::{Parse, ParserError};
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
    Compound(CompoundStatement),
    Loop(Loop),
    Jump(Jump),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Jump {
    Goto(String),
    Continue,
    Break,
    Return(Option<Expression>),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Loop {
    While {
        condition: Expression,
        statement: Box<Statement>,
    },
    DoWhile {
        statement: Box<Statement>,
        condition: Expression,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Label {
    Label(String),
    Case(Constant),
    Default,
}

impl Parse for Statement {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> crate::parser::Result<Self>
    where
        Self: Sized,
    {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            Token::Keyword(Keyword::If) | Token::Keyword(Keyword::Switch) => {
                Statement::conditional_statement(tokens)?
            }
            Token::Keyword(Keyword::Case) | Token::Keyword(Keyword::Default) => {
                Statement::labeled_statement(tokens)?
            }
            Token::Keyword(Keyword::While) | Token::Keyword(Keyword::Do) => {
                Statement::iteration_statement(tokens)?
            }
            Token::OpenBrace => {
                let compound = CompoundStatement::parse(tokens)?;
                Statement::Compound(compound)
            }
            Token::Keyword(Keyword::Goto)
            | Token::Keyword(Keyword::Continue)
            | Token::Keyword(Keyword::Break)
            | Token::Keyword(Keyword::Return) => Statement::jump_statement(tokens)?,
            _ => Statement::expression_statement(tokens)?,
        };

        Ok(out)
    }
}

impl Statement {
    fn jump_statement(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> crate::parser::Result<Self> {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(Keyword::Goto) => {
                let label = match tokens.next().ok_or(ParserError::UnexpectedEndOfInput)? {
                    Token::Word(label) => label,
                    token => {
                        return Err(ParserError::ExpectedIdentifier {
                            unexpected: token,
                            near_tokens: tokens.take(6).collect(),
                        })
                    }
                };
                Self::expect_token(tokens, Token::SemiColon)?;
                Statement::Jump(Jump::Goto(label))
            }
            Token::Keyword(Keyword::Continue) => {
                Self::expect_token(tokens, Token::SemiColon)?;
                Statement::Jump(Jump::Continue)
            }
            Token::Keyword(Keyword::Break) => {
                Self::expect_token(tokens, Token::SemiColon)?;
                Statement::Jump(Jump::Break)
            }
            Token::Keyword(Keyword::Return) => {
                let expression = match tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)? {
                    Token::SemiColon => None,
                    _ => Some(Expression::parse(tokens)?),
                };
                Self::expect_token(tokens, Token::SemiColon)?;
                Statement::Jump(Jump::Return(expression))
            }
            _ => unreachable!(),
        };

        Ok(out)
    }

    fn iteration_statement(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> crate::parser::Result<Self> {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(Keyword::While) => {
                Self::expect_token(tokens, Token::OpenParenthesis)?;
                let condition = Expression::parse(tokens)?;
                Self::expect_token(tokens, Token::CloseParenthesis)?;
                let statement = Box::new(Statement::parse(tokens)?);

                Statement::Loop(Loop::While {
                    condition,
                    statement,
                })
            }
            Token::Keyword(Keyword::Do) => {
                let statement = Box::new(Statement::parse(tokens)?);
                Self::expect_token(tokens, Token::Keyword(Keyword::While))?;
                Self::expect_token(tokens, Token::OpenParenthesis)?;
                let condition = Expression::parse(tokens)?;
                Self::expect_token(tokens, Token::CloseParenthesis)?;
                Self::expect_token(tokens, Token::SemiColon)?;

                Statement::Loop(Loop::DoWhile {
                    statement,
                    condition,
                })
            }
            _ => unreachable!(),
        };

        Ok(out)
    }

    fn labeled_statement(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> crate::parser::Result<Self> {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(Keyword::Case) => {
                let constant = Constant::parse(tokens)?;
                Self::expect_token(tokens, Token::Colon)?;
                let statement = Box::new(Statement::parse(tokens)?);
                Statement::Labeled {
                    label: Label::Case(constant),
                    statement,
                }
            }
            Token::Keyword(Keyword::Default) => {
                Self::expect_token(tokens, Token::Colon)?;
                let statement = Box::new(Statement::parse(tokens)?);
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
    ) -> crate::parser::Result<Self> {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            Token::SemiColon => {
                tokens.next();
                Statement::Expression(Expression::Empty)
            }
            _ => {
                let expression = Expression::parse(tokens)?;

                // might be labeled statement
                if let Token::Colon = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)? {
                    tokens.next();
                    let peek = tokens
                        .peek()
                        .ok_or(ParserError::UnexpectedEndOfInput)?
                        .clone();
                    let statement = Box::new(Statement::parse(tokens)?);
                    return Ok(Statement::Labeled {
                        label: Label::Label(expression.get_identifier().ok_or(
                            ParserError::ExpectedIdentifier {
                                unexpected: peek,
                                near_tokens: tokens.take(6).collect(),
                            },
                        )?),
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
    ) -> crate::parser::Result<Self> {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(Keyword::If) => {
                Self::expect_token(tokens, Token::OpenParenthesis)?;
                let condition = Expression::parse(tokens)?;
                Self::expect_token(tokens, Token::CloseParenthesis)?;
                let body = Box::new(Statement::parse(tokens)?);
                let otherwise = match tokens.peek() {
                    Some(Token::Keyword(Keyword::Else)) => {
                        tokens.next();
                        Some(Box::new(Statement::parse(tokens)?))
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
                let condition = Expression::parse(tokens)?;
                Self::expect_token(tokens, Token::CloseParenthesis)?;
                let statement = Box::new(Statement::parse(tokens)?);

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
    use super::*;
    use crate::lexer::{Constant as TokenConstant, Keyword};
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
        let result = Statement::expression_statement(&mut input.into_iter().peekable()).unwrap();
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
            body: Box::new(
                Statement::Compound(
                    CompoundStatement {
                        statements: vec![Statement::Expression(Expression::Identifier("b".to_string()))]
                    }
                )
            ),
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
            body: Box::new(
                Statement::Compound(
                    CompoundStatement {
                        statements: vec![Statement::Expression(Expression::Identifier("b".to_string()))]
                    }
                )
            ),
            otherwise: Some(Box::new(
                Statement::Compound(
                    CompoundStatement {
                        statements: vec![Statement::Expression(Expression::Identifier("c".to_string()))]
                    }
                )
            )),
        },
    )]
    #[case::switch(
        vec![
            Token::Keyword(Keyword::Switch),
            Token::OpenParenthesis,
            Token::Word("a".to_string()),
            Token::CloseParenthesis,
            Token::Word("b".to_string()),
            Token::SemiColon,
        ],
        Statement::Switch {
            condition: Expression::Identifier("a".to_string()),
            statement: Box::new(Statement::Expression(Expression::Identifier("b".to_string()))),
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
            Token::Keyword(Keyword::Break),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::Switch {
            condition: Expression::Identifier("a".to_string()),
            statement: Box::new(
                Statement::Compound(
                    CompoundStatement {
                        statements: vec![Statement::Labeled {
                            label: Label::Default,
                            statement: Box::new(Statement::Jump(Jump::Break)),
                        }]
                    }
                )
            ),
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
            Token::Constant(TokenConstant::SignedInteger(1)),
            Token::Colon,
            Token::Word("b".to_string()),
            Token::SemiColon,
            Token::CloseBrace,
        ],
        Statement::Switch {
            condition: Expression::Identifier("a".to_string()),
            statement: Box::new(
                Statement::Compound(
                    CompoundStatement {
                        statements: vec![Statement::Labeled {
                            label: Label::Case(Constant::Integer(1)),
                            statement: Box::new(Statement::Expression(Expression::Identifier("b".to_string()))),
                        }]
                    }
                )
            ),
        }
    )]
    fn test_conditional_statement(#[case] input: Vec<Token>, #[case] expected: Statement) {
        let result = Statement::conditional_statement(&mut input.into_iter().peekable()).unwrap();
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
        let result = Statement::parse(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::while_loop(
        vec![
        Token::Keyword(Keyword::While),
        Token::OpenParenthesis,
        Token::Word("a".to_string()),
        Token::CloseParenthesis,
        Token::Word("b".to_string()),
        Token::SemiColon,
        ],
        Statement::Loop(Loop::While {
        condition: Expression::Identifier("a".to_string()),
        statement: Box::new(Statement::Expression(Expression::Identifier("b".to_string()))),
        })
    )]
    #[case::do_while_loop(
        vec![
        Token::Keyword(Keyword::Do),
        Token::Word("a".to_string()),
        Token::SemiColon,
        Token::Keyword(Keyword::While),
        Token::OpenParenthesis,
        Token::Word("b".to_string()),
        Token::CloseParenthesis,
        Token::SemiColon,
        ],
        Statement::Loop(Loop::DoWhile {
        statement: Box::new(Statement::Expression(Expression::Identifier("a".to_string()))),
        condition: Expression::Identifier("b".to_string()),
        })
    )]
    fn test_iteration_statement(#[case] input: Vec<Token>, #[case] expected: Statement) {
        let result = Statement::iteration_statement(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::goto(
        vec![
        Token::Keyword(Keyword::Goto),
        Token::Word("label".to_string()),
        Token::SemiColon,
        ],
        Statement::Jump(Jump::Goto("label".to_string()))
    )]
    #[case::continue_(
        vec![
        Token::Keyword(Keyword::Continue),
        Token::SemiColon,
        ],
        Statement::Jump(Jump::Continue)
    )]
    #[case::break_(
        vec![
        Token::Keyword(Keyword::Break),
        Token::SemiColon,
        ],
        Statement::Jump(Jump::Break)
    )]
    #[case::return_(
        vec![
        Token::Keyword(Keyword::Return),
        Token::SemiColon,
        ],
        Statement::Jump(Jump::Return(None))
    )]
    #[case::return_expression(
        vec![
        Token::Keyword(Keyword::Return),
        Token::Word("a".to_string()),
        Token::SemiColon,
        ],
        Statement::Jump(Jump::Return(Some(Expression::Identifier("a".to_string()))))
    )]
    fn test_jump_statement(#[case] input: Vec<Token>, #[case] expected: Statement) {
        let result = Statement::jump_statement(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }
}
