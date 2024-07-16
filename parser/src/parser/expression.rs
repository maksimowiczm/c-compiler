use crate::lexer::{Constant as TokenConstant, StringLiteral, Token};
use crate::parser::constant::Constant;
use crate::parser::{Context, Parse, ParserError};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expression {
    Identifier(String),
    Constant(Constant),
    String(String),
    PreOperation {
        variable: String,
        operator: InstantOperator,
    },
    PostOperation {
        variable: String,
        operator: InstantOperator,
    },
    StructMember {
        variable: String,
        member: String,
    },
    SquareBracket {
        variable: String,
        expression: Box<Expression>,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum InstantOperator {
    Increment,
    Decrement,
}

impl Parse for Expression {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>, context: &Context) -> Result
    where
        Self: Sized,
    {
        Expression::primary_expression(tokens, context)
    }
}

type Result = std::result::Result<Expression, ParserError>;

impl Expression {
    fn postfix_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let primary = Self::primary_expression(tokens, context)?;
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            // <postfix-expression> [ <expression> ]
            Token::OpenSquareBracket => {
                tokens.next();
                let expression = Expression::parse(tokens, context)?;
                Self::expect_token(tokens, Token::CloseSquareBracket)?;
                Expression::SquareBracket {
                    variable: primary
                        .get_identifier()
                        .ok_or(ParserError::UnexpectedToken {
                            unexpected: Token::OpenSquareBracket,
                            expected: vec![Token::Word("<identifier>".to_string())],
                            near_tokens: tokens.take(6).collect(),
                        })?,
                    expression: Box::new(expression),
                }
            }
            // <postfix-expression> ( {<assignment-expression>}* )
            Token::OpenParenthesis => todo!(),
            // <postfix-expression> . <identifier>
            // <postfix-expression> -> <identifier>
            Token::Dot | Token::Arrow => {
                tokens.next();
                let variable =
                    primary
                        .get_identifier()
                        .ok_or_else(|| ParserError::UnexpectedToken {
                            unexpected: Token::Arrow,
                            expected: vec![],
                            near_tokens: tokens.take(6).collect(),
                        })?;

                let member = match tokens.next().ok_or(ParserError::UnexpectedEndOfInput)? {
                    Token::Word(identifier) => identifier,
                    unexpected => {
                        return Err(ParserError::UnexpectedToken {
                            unexpected,
                            expected: vec![Token::Word("<identifier>".to_string())],
                            near_tokens: tokens.take(6).collect(),
                        })
                    }
                };
                Expression::StructMember { variable, member }
            }
            // <postfix-expression> ++
            Token::Increment => {
                let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
                let variable = primary
                    .get_identifier()
                    .ok_or(ParserError::UnexpectedToken {
                        unexpected: token,
                        expected: vec![Token::Word("<identifier>".to_string())],
                        near_tokens: tokens.take(6).collect(),
                    })?;
                Expression::PostOperation {
                    variable,
                    operator: InstantOperator::Increment,
                }
            }
            // postfix-expression> --
            Token::Decrement => {
                let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
                let variable = primary
                    .get_identifier()
                    .ok_or(ParserError::UnexpectedToken {
                        unexpected: token,
                        expected: vec![Token::Word("<identifier>".to_string())],
                        near_tokens: tokens.take(6).collect(),
                    })?;
                Expression::PostOperation {
                    variable,
                    operator: InstantOperator::Decrement,
                }
            }
            _ => primary,
        };

        Ok(out)
    }

    fn primary_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let token = tokens
            .peek()
            .ok_or(ParserError::UnexpectedEndOfInput)?
            .clone();

        let out = match token {
            Token::Word(identifier) => {
                tokens.next();
                if context.contains_enumeration_constant(&identifier.to_string()) {
                    Expression::Constant(Constant::Enumeration(identifier.to_string()))
                } else {
                    Expression::Identifier(identifier.to_string())
                }
            }
            Token::Constant { .. } => Expression::Constant(Constant::parse(tokens, context)?),
            Token::StringLiteral(string_literal) => match string_literal {
                StringLiteral::ByteString(bytes) => {
                    let string = bytes.iter().collect();
                    Expression::String(string)
                }
            },
            Token::OpenParenthesis => {
                tokens.next();
                let expression = Expression::parse(tokens, context)?;
                Self::expect_token(tokens, Token::CloseParenthesis)?;
                expression
            }
            _ => Err(ParserError::UnexpectedToken {
                unexpected: token.clone(),
                expected: vec![
                    Token::Word("<identifier>".to_string()),
                    Token::Constant(TokenConstant::SignedInteger(0)),
                    Token::Constant(TokenConstant::UnsignedInteger(0)),
                    Token::Constant(TokenConstant::Decimal(0.0)),
                    Token::Constant(TokenConstant::Character('*')),
                    Token::StringLiteral(StringLiteral::ByteString(vec![])),
                    Token::OpenParenthesis,
                ],
                near_tokens: tokens.take(6).collect(),
            })?,
        };

        Ok(out)
    }
}

impl Expression {
    fn get_identifier(self) -> Option<String> {
        match self {
            Expression::Identifier(identifier) => Some(identifier),
            _ => None,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Constant as TokenConstant, StringLiteral, Token};
    use crate::parser::constant::Constant;
    use crate::parser::Context;
    use rstest::rstest;

    #[rstest]
    #[case::identifier(vec![Token::Word("foo".to_string())], Expression::Identifier("foo".to_string()))]
    #[case::enumeration_constant(vec![Token::Word("bar".to_string())], Expression::Constant(Constant::Enumeration("bar".to_string())))]
    #[case::signed_constant(vec![Token::Constant(TokenConstant::SignedInteger(42))], Expression::Constant(Constant::Integer(42)))]
    #[case::unsigned_constant(vec![Token::Constant(TokenConstant::UnsignedInteger(42))], Expression::Constant(Constant::Integer(42)))]
    #[case::character_constant(vec![Token::Constant(TokenConstant::Character('a'))], Expression::Constant(Constant::Character('a')))]
    #[case::decimal_constant(vec![Token::Constant(TokenConstant::Decimal(42.0))], Expression::Constant(Constant::Float(42.0)))]
    #[case::string_u8(vec![Token::StringLiteral(StringLiteral::ByteString(vec!['h', 'e', 'l', 'l', 'o']))], Expression::String("hello".to_string()))]
    #[case::parenthesis(vec![Token::OpenParenthesis, Token::Word("foo".to_string()), Token::CloseParenthesis], Expression::Identifier("foo".to_string()))]
    fn primary_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let mut context = Context {
            enums: Default::default(),
        };
        context
            .enums
            .insert("foo".to_string(), vec!["bar".to_string()]);
        let result =
            Expression::primary_expression(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::square_brackets(
        vec![
            Token::Word("foo".to_string()),
            Token::OpenSquareBracket,
            Token::Constant(TokenConstant::SignedInteger(42)),
            Token::CloseSquareBracket,
        ],
        Expression::SquareBracket {
            variable: "foo".to_string(),
            expression: Box::new(Expression::Constant(Constant::Integer(42))),
        },
    )]
    #[case::dot(
        vec![
            Token::Word("foo".to_string()),
            Token::Dot,
            Token::Word("bar".to_string()),
        ],
        Expression::StructMember {
            variable: "foo".to_string(),
            member: "bar".to_string(),
        },
    )]
    #[case::arrow(
        vec![
            Token::Word("foo".to_string()),
            Token::Arrow,
            Token::Word("bar".to_string()),
        ],
        Expression::StructMember {
            variable: "foo".to_string(),
            member: "bar".to_string(),
        },
    )]
    #[case::post_increment(
        vec![
            Token::Word("foo".to_string()),
            Token::Increment,
        ],
        Expression::PostOperation {
            variable: "foo".to_string(),
            operator: InstantOperator::Increment,
        },
    )]
    #[case::post_decrement(
        vec![
            Token::Word("foo".to_string()),
            Token::Decrement,
        ],
        Expression::PostOperation {
            variable: "foo".to_string(),
            operator: InstantOperator::Decrement,
        },
    )]
    fn test_postfix_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let context = Context {
            enums: Default::default(),
        };
        let result =
            Expression::postfix_expression(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }
}
