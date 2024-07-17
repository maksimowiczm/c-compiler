use crate::lexer::{Constant as TokenConstant, Keyword, StringLiteral, Token};
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
    Sizeof(SizeofExpression),
    Unary {
        operator: UnaryOperator,
        expression: Box<Expression>,
    },
    Cast {
        type_info: TypeInfo,
        expression: Box<Expression>,
    },
    Multiplicative {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: MultiplicativeOperator,
    },
    Additive {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: AdditiveOperator,
    },
    Shift {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: ShiftOperator,
    },
    Relation {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: RelationOperator,
    },
    Equality {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: EqualityOperator,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum EqualityOperator {
    Equal,
    NotEqual,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum RelationOperator {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ShiftOperator {
    Left,
    Right,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum AdditiveOperator {
    Addition,
    Subtraction,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum MultiplicativeOperator {
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TypeInfo {
    Keyword(String),
    Struct(String),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum UnaryOperator {
    AddressOf,
    Dereference,
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum SizeofExpression {
    Type(TypeInfo),
    Expression(Box<Expression>),
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
    fn equality_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let relation_expression = Self::relation_expression(tokens, context)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(relation_expression),
        };

        let operator = match peek {
            Token::Equal => EqualityOperator::Equal,
            Token::NotEqual => EqualityOperator::NotEqual,
            _ => return Ok(relation_expression),
        };

        tokens.next();
        let right = Self::relation_expression(tokens, context)?;
        Ok(Expression::Equality {
            left: Box::new(relation_expression),
            right: Box::new(right),
            operator,
        })
    }

    fn relation_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let shift_expression = Self::shift_expression(tokens, context)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(shift_expression),
        };

        let operator = match peek {
            Token::LessThan => RelationOperator::LessThan,
            Token::LessThanOrEqual => RelationOperator::LessThanOrEqual,
            Token::GreaterThan => RelationOperator::GreaterThan,
            Token::GreaterThanOrEqual => RelationOperator::GreaterThanOrEqual,
            _ => return Ok(shift_expression),
        };

        tokens.next();
        let right = Self::shift_expression(tokens, context)?;
        Ok(Expression::Relation {
            left: Box::new(shift_expression),
            right: Box::new(right),
            operator,
        })
    }

    fn shift_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let additive_expression = Self::additive_expression(tokens, context)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(additive_expression),
        };

        let out = match peek {
            Token::ShiftLeft => {
                tokens.next();
                let right = Self::additive_expression(tokens, context)?;
                Expression::Shift {
                    left: Box::new(additive_expression),
                    right: Box::new(right),
                    operator: ShiftOperator::Left,
                }
            }
            Token::ShiftRight => {
                tokens.next();
                let right = Self::additive_expression(tokens, context)?;
                Expression::Shift {
                    left: Box::new(additive_expression),
                    right: Box::new(right),
                    operator: ShiftOperator::Right,
                }
            }
            _ => additive_expression,
        };

        Ok(out)
    }

    fn additive_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let multiplicative_expression = Self::multiplicative_expression(tokens, context)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(multiplicative_expression),
        };

        let out = match peek {
            Token::Addition => {
                tokens.next();
                let right = Self::multiplicative_expression(tokens, context)?;
                Expression::Additive {
                    left: Box::new(multiplicative_expression),
                    right: Box::new(right),
                    operator: AdditiveOperator::Addition,
                }
            }
            Token::Negation => {
                tokens.next();
                let right = Self::multiplicative_expression(tokens, context)?;
                Expression::Additive {
                    left: Box::new(multiplicative_expression),
                    right: Box::new(right),
                    operator: AdditiveOperator::Subtraction,
                }
            }
            _ => multiplicative_expression,
        };

        Ok(out)
    }

    fn multiplicative_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let cast_expression = Self::cast_expression(tokens, context)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(cast_expression),
        };

        let out = match peek {
            Token::Star => {
                tokens.next();
                let right = Self::cast_expression(tokens, context)?;
                Expression::Multiplicative {
                    left: Box::new(cast_expression),
                    right: Box::new(right),
                    operator: MultiplicativeOperator::Multiply,
                }
            }
            Token::Division => {
                tokens.next();
                let right = Self::cast_expression(tokens, context)?;
                Expression::Multiplicative {
                    left: Box::new(cast_expression),
                    right: Box::new(right),
                    operator: MultiplicativeOperator::Divide,
                }
            }
            Token::Modulo => {
                tokens.next();
                let right = Self::cast_expression(tokens, context)?;
                Expression::Multiplicative {
                    left: Box::new(cast_expression),
                    right: Box::new(right),
                    operator: MultiplicativeOperator::Modulo,
                }
            }
            _ => cast_expression,
        };

        Ok(out)
    }

    fn cast_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        match peek {
            Token::OpenParenthesis => {
                tokens.next();
                let type_info = TypeInfo::parse(tokens, context)?;
                Self::expect_token(tokens, Token::CloseParenthesis)?;
                let expression = Self::cast_expression(tokens, context)?;
                Ok(Expression::Cast {
                    type_info,
                    expression: Box::new(expression),
                })
            }
            _ => Self::unary_expression(tokens, context),
        }
    }

    fn unary_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            Token::Increment => {
                tokens.next();
                let unary_expression = Self::unary_expression(tokens, context)?;
                Expression::PreOperation {
                    variable: unary_expression.get_identifier().ok_or_else(|| {
                        ParserError::UnexpectedToken {
                            unexpected: Token::Increment,
                            expected: vec![Token::Word("<identifier>".to_string())],
                            near_tokens: tokens.take(6).collect(),
                        }
                    })?,
                    operator: InstantOperator::Increment,
                }
            }
            Token::Decrement => {
                tokens.next();
                let unary_expression = Self::unary_expression(tokens, context)?;
                Expression::PreOperation {
                    variable: unary_expression.get_identifier().ok_or_else(|| {
                        ParserError::UnexpectedToken {
                            unexpected: Token::Decrement,
                            expected: vec![Token::Word("<identifier>".to_string())],
                            near_tokens: tokens.take(6).collect(),
                        }
                    })?,
                    operator: InstantOperator::Decrement,
                }
            }
            Token::Keyword(Keyword::Sizeof) => {
                tokens.next();
                Self::allow_token(tokens, Token::OpenParenthesis)?;
                let peek = tokens
                    .peek()
                    .ok_or(ParserError::UnexpectedEndOfInput)?
                    .clone();

                let out = match peek {
                    Token::Keyword(_) => Expression::Sizeof(SizeofExpression::Type(
                        TypeInfo::parse(tokens, context)?,
                    )),
                    _ => {
                        let expression = Expression::parse(tokens, context)?;
                        Expression::Sizeof(SizeofExpression::Expression(Box::new(expression)))
                    }
                };

                Self::allow_token(tokens, Token::CloseParenthesis)?;

                out
            }
            Token::Ampersand => {
                tokens.next();
                let expression = Self::cast_expression(tokens, context)?;
                Expression::Unary {
                    operator: UnaryOperator::AddressOf,
                    expression: Box::new(expression),
                }
            }
            Token::Star => {
                tokens.next();
                let expression = Self::cast_expression(tokens, context)?;
                Expression::Unary {
                    operator: UnaryOperator::Dereference,
                    expression: Box::new(expression),
                }
            }
            Token::Addition => {
                tokens.next();
                let expression = Self::cast_expression(tokens, context)?;
                Expression::Unary {
                    operator: UnaryOperator::Plus,
                    expression: Box::new(expression),
                }
            }
            Token::Negation => {
                tokens.next();
                let expression = Self::cast_expression(tokens, context)?;
                Expression::Unary {
                    operator: UnaryOperator::Minus,
                    expression: Box::new(expression),
                }
            }
            Token::BitwiseNot => {
                tokens.next();
                let expression = Self::cast_expression(tokens, context)?;
                Expression::Unary {
                    operator: UnaryOperator::BitwiseNot,
                    expression: Box::new(expression),
                }
            }
            Token::LogicalNot => {
                tokens.next();
                let expression = Self::cast_expression(tokens, context)?;
                Expression::Unary {
                    operator: UnaryOperator::LogicalNot,
                    expression: Box::new(expression),
                }
            }
            _ => Self::postfix_expression(tokens, context)?,
        };

        Ok(out)
    }

    fn postfix_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result {
        let primary = Self::primary_expression(tokens, context)?;
        let peek = tokens.peek();

        let peek = match peek {
            Some(token) => token,
            None => return Ok(primary),
        };

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
            Expression::StructMember { member, .. } => Some(member),
            _ => None,
        }
    }
}

impl Parse for TypeInfo {
    fn parse(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        _: &Context,
    ) -> std::result::Result<TypeInfo, ParserError>
    where
        Self: Sized,
    {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(Keyword::Struct) => {
                let identifier = match tokens.next().ok_or(ParserError::UnexpectedEndOfInput)? {
                    Token::Word(identifier) => identifier,
                    unexpected => {
                        return Err(ParserError::UnexpectedToken {
                            unexpected,
                            expected: vec![Token::Word("<identifier>".to_string())],
                            near_tokens: tokens.take(6).collect(),
                        })
                    }
                };

                TypeInfo::Struct(identifier)
            }
            Token::Keyword(keyword) => TypeInfo::Keyword(keyword.to_string().to_lowercase()),
            unexpected => {
                return Err(ParserError::UnexpectedToken {
                    unexpected,
                    expected: vec![Token::Keyword(Keyword::Int)],
                    near_tokens: tokens.take(6).collect(),
                })
            }
        };

        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Constant as TokenConstant, Keyword, StringLiteral, Token};
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
        let context = Context::default();
        let result =
            Expression::postfix_expression(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::pre_increment(
        vec![
            Token::Increment,
            Token::Word("foo".to_string()),
        ],
        Expression::PreOperation {
            variable: "foo".to_string(),
            operator: InstantOperator::Increment,
        },
    )]
    #[case::pre_decrement(
        vec![
            Token::Decrement,
            Token::Word("foo".to_string()),
        ],
        Expression::PreOperation {
            variable: "foo".to_string(),
            operator: InstantOperator::Decrement,
        },
    )]
    #[case::sizeof_type(
        vec![
            Token::Keyword(Keyword::Sizeof),
            Token::OpenParenthesis,
            Token::Keyword(Keyword::Int),
            Token::CloseParenthesis,
        ],
        Expression::Sizeof(SizeofExpression::Type(TypeInfo::Keyword("int".to_string()))),
    )]
    #[case::sizeof_expression(
        vec![
            Token::Keyword(Keyword::Sizeof),
            Token::OpenParenthesis,
            Token::Word("foo".to_string()),
            Token::CloseParenthesis,
        ],
        Expression::Sizeof(SizeofExpression::Expression(Box::new(Expression::Identifier("foo".to_string())))),
    )]
    #[case::address_of(
        vec![
            Token::Ampersand,
            Token::Word("foo".to_string()),
        ],
        Expression::Unary {
            operator: UnaryOperator::AddressOf,
            expression: Box::new(Expression::Identifier("foo".to_string())),
        },
    )]
    #[case::dereference(
        vec![
            Token::Star,
            Token::Word("foo".to_string()),
        ],
        Expression::Unary {
            operator: UnaryOperator::Dereference,
            expression: Box::new(Expression::Identifier("foo".to_string())),
        },
    )]
    #[case::plus(
        vec![
            Token::Addition,
            Token::Word("foo".to_string()),
        ],
        Expression::Unary {
            operator: UnaryOperator::Plus,
            expression: Box::new(Expression::Identifier("foo".to_string())),
        },
    )]
    #[case::minus(
        vec![
            Token::Negation,
            Token::Word("foo".to_string()),
        ],
        Expression::Unary {
            operator: UnaryOperator::Minus,
            expression: Box::new(Expression::Identifier("foo".to_string())),
        },
    )]
    #[case::bitwise_not(
        vec![
            Token::BitwiseNot,
            Token::Word("foo".to_string()),
        ],
        Expression::Unary {
            operator: UnaryOperator::BitwiseNot,
            expression: Box::new(Expression::Identifier("foo".to_string())),
        },
    )]
    #[case::logical_not(
        vec![
            Token::LogicalNot,
            Token::Word("foo".to_string()),
        ],
        Expression::Unary {
            operator: UnaryOperator::LogicalNot,
            expression: Box::new(Expression::Identifier("foo".to_string())),
        },
    )]
    fn test_unary_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let context = Context::default();
        let result =
            Expression::unary_expression(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::cast_type(
        vec![
            Token::OpenParenthesis,
            Token::Keyword(Keyword::Int),
            Token::CloseParenthesis,
            Token::Word("foo".to_string()),
        ],
        Expression::Cast {
            type_info: TypeInfo::Keyword("int".to_string()),
            expression: Box::new(Expression::Identifier("foo".to_string())),
        },
    )]
    fn test_cast_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let context = Context::default();
        let result =
            Expression::cast_expression(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::multiply(
        vec![
            Token::Word("foo".to_string()),
            Token::Star,
            Token::Word("bar".to_string()),
        ],
        Expression::Multiplicative {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: MultiplicativeOperator::Multiply,
        },
    )]
    #[case::divide(
        vec![
            Token::Word("foo".to_string()),
            Token::Division,
            Token::Word("bar".to_string()),
        ],
        Expression::Multiplicative {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: MultiplicativeOperator::Divide,
        },
    )]
    #[case::modulo(
        vec![
            Token::Word("foo".to_string()),
            Token::Modulo,
            Token::Word("bar".to_string()),
        ],
        Expression::Multiplicative {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: MultiplicativeOperator::Modulo,
        },
    )]
    fn test_multiplicative_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let context = Context::default();
        let result =
            Expression::multiplicative_expression(&mut input.into_iter().peekable(), &context)
                .unwrap();
    }

    #[rstest]
    #[case::addition(
        vec![
            Token::Word("foo".to_string()),
            Token::Addition,
            Token::Word("bar".to_string()),
        ],
        Expression::Additive {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: AdditiveOperator::Addition,
        },
    )]
    #[case::subtraction(
        vec![
            Token::Word("foo".to_string()),
            Token::Negation,
            Token::Word("bar".to_string()),
        ],
        Expression::Additive {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: AdditiveOperator::Subtraction,
        },
    )]
    fn test_additive_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let context = Context::default();
        let result =
            Expression::additive_expression(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::left_shift(
        vec![
            Token::Word("foo".to_string()),
            Token::ShiftLeft,
            Token::Word("bar".to_string()),
        ],
        Expression::Shift {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: ShiftOperator::Left,
        },
    )]
    #[case::right_shift(
        vec![
            Token::Word("foo".to_string()),
            Token::ShiftRight,
            Token::Word("bar".to_string()),
        ],
        Expression::Shift {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: ShiftOperator::Right,
        },
    )]
    fn test_shift_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let context = Context::default();
        let result =
            Expression::shift_expression(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::less_than(
        vec![
            Token::Word("foo".to_string()),
            Token::LessThan,
            Token::Word("bar".to_string()),
        ],
        Expression::Relation {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: RelationOperator::LessThan,
        },
    )]
    #[case::less_than_or_equal(
        vec![
            Token::Word("foo".to_string()),
            Token::LessThanOrEqual,
            Token::Word("bar".to_string()),
        ],
        Expression::Relation {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: RelationOperator::LessThanOrEqual,
        },
    )]
    #[case::greater_than(
        vec![
            Token::Word("foo".to_string()),
            Token::GreaterThan,
            Token::Word("bar".to_string()),
        ],
        Expression::Relation {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: RelationOperator::GreaterThan,
        },
    )]
    #[case::greater_than_or_equal(
        vec![
            Token::Word("foo".to_string()),
            Token::GreaterThanOrEqual,
            Token::Word("bar".to_string()),
        ],
        Expression::Relation {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: RelationOperator::GreaterThanOrEqual,
        },
    )]
    fn test_relation_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let context = Context::default();
        let result =
            Expression::relation_expression(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::equal(
        vec![
            Token::Word("foo".to_string()),
            Token::Equal,
            Token::Word("bar".to_string()),
        ],
        Expression::Equality {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: EqualityOperator::Equal,
        },
    )]
    #[case::not_equal(
        vec![
            Token::Word("foo".to_string()),
            Token::NotEqual,
            Token::Word("bar".to_string()),
        ],
        Expression::Equality {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: EqualityOperator::NotEqual,
        },
    )]
    fn test_equality_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let context = Context::default();
        let result =
            Expression::equality_expression(&mut input.into_iter().peekable(), &context).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::type_name(vec![Token::Keyword(Keyword::Int)], TypeInfo::Keyword("int".to_string()))]
    #[case::struct_(vec![Token::Keyword(Keyword::Struct), Token::Word("foo".to_string())], TypeInfo::Struct("foo".to_string()))]
    fn test_type_info(#[case] input: Vec<Token>, #[case] expected: TypeInfo) {
        let result =
            TypeInfo::parse(&mut input.into_iter().peekable(), &Context::default()).unwrap();
        assert_eq!(result, expected);
    }
}
