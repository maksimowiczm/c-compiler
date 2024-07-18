use crate::lexer::{
    Assignment as TokenAssignment, Constant as TokenConstant, Keyword, StringLiteral, Token,
};
use crate::parser::constant::Constant;
use crate::parser::declaration::specifier_qualifier::SpecifierQualifier;
use crate::parser::{Parse, ParserError, Result, TryParse};
use std::iter::Peekable;
use TokenAssignment::*;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum Expression {
    Empty,
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
        type_name: TypeName,
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
    Bitwise {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: BitwiseOperator,
    },
    Logical {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: LogicalOperator,
    },
    Ternary {
        condition: Box<Expression>,
        true_expression: Box<Expression>,
        false_expression: Box<Expression>,
    },
    Assignment {
        variable: Box<Expression>,
        expression: Box<Expression>,
        operator: AssignmentOperator,
    },
    Comma {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Dereference {
        expression: Box<Expression>,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum AssignmentOperator {
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    LeftShiftAssign,
    RightShiftAssign,
    BitwiseAndAssign,
    BitwiseXorAssign,
    BitwiseOrAssign,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum BitwiseOperator {
    And,
    Or,
    Xor,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum RelationOperator {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum ShiftOperator {
    Left,
    Right,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum AdditiveOperator {
    Addition,
    Subtraction,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum MultiplicativeOperator {
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum UnaryOperator {
    AddressOf,
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum SizeofExpression {
    Type(TypeName),
    Expression(Box<Expression>),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum InstantOperator {
    Increment,
    Decrement,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub struct TypeName {
    specifier_qualifier: Vec<SpecifierQualifier>,
    abstract_declarator: Option<()>,
}

impl TryParse for TypeName {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        let mut specifier_qualifier = vec![];
        while let Some(qualifier) = SpecifierQualifier::try_parse(tokens)? {
            specifier_qualifier.push(qualifier);
        }

        if specifier_qualifier.is_empty() {
            return Ok(None);
        }

        // abstract declarator todo
        let abstract_declarator = None;

        Ok(Some(Self {
            specifier_qualifier,
            abstract_declarator,
        }))
    }
}

impl Parse for Expression {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let expression = Expression::assignment_expression(tokens)?;
        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(expression),
        };

        let out = match peek {
            Token::Comma => {
                tokens.next();
                let right = Self::parse(tokens)?;
                Expression::Comma {
                    left: Box::new(expression),
                    right: Box::new(right),
                }
            }
            _ => expression,
        };

        Ok(out)
    }
}

impl Expression {
    pub fn assignment_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<Self> {
        let conditional_expression = Self::conditional_expression(tokens)?;

        // allow only lvalue
        if !matches!(
            conditional_expression,
            Expression::Identifier { .. }
                | Expression::SquareBracket { .. }
                | Expression::StructMember { .. }
                | Expression::Dereference { .. }
        ) {
            return Ok(conditional_expression);
        }

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(conditional_expression),
        };

        let operator = match peek {
            Token::Assignment(Equal) => AssignmentOperator::Assign,
            Token::Assignment(PlusEqual) => AssignmentOperator::AddAssign,
            Token::Assignment(MinusEqual) => AssignmentOperator::SubtractAssign,
            Token::Assignment(MultiplyEqual) => AssignmentOperator::MultiplyAssign,
            Token::Assignment(DivideEqual) => AssignmentOperator::DivideAssign,
            Token::Assignment(ModuloEqual) => AssignmentOperator::ModuloAssign,
            Token::Assignment(ShiftLeftEqual) => AssignmentOperator::LeftShiftAssign,
            Token::Assignment(ShiftRightEqual) => AssignmentOperator::RightShiftAssign,
            Token::Assignment(AndEqual) => AssignmentOperator::BitwiseAndAssign,
            Token::Assignment(XorEqual) => AssignmentOperator::BitwiseXorAssign,
            Token::Assignment(OrEqual) => AssignmentOperator::BitwiseOrAssign,
            _ => return Ok(conditional_expression),
        };

        tokens.next();
        let expression = Self::assignment_expression(tokens)?;

        Ok(Expression::Assignment {
            variable: Box::new(conditional_expression),
            expression: Box::new(expression),
            operator,
        })
    }

    fn conditional_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let logical_or_expression = Self::logical_or_expression(tokens)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(logical_or_expression),
        };

        let out = match peek {
            Token::QuestionMark => {
                tokens.next();
                let true_expression = Self::parse(tokens)?;
                Self::expect_token(tokens, Token::Colon)?;
                let false_expression = Self::parse(tokens)?;
                Expression::Ternary {
                    condition: Box::new(logical_or_expression),
                    true_expression: Box::new(true_expression),
                    false_expression: Box::new(false_expression),
                }
            }
            _ => logical_or_expression,
        };

        Ok(out)
    }

    fn logical_or_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let logical_and_expression = Self::logical_and_expression(tokens)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(logical_and_expression),
        };

        let out = match peek {
            Token::LogicalOr => {
                tokens.next();
                let right = Self::logical_or_expression(tokens)?;
                Expression::Logical {
                    left: Box::new(logical_and_expression),
                    right: Box::new(right),
                    operator: LogicalOperator::Or,
                }
            }
            _ => logical_and_expression,
        };

        Ok(out)
    }

    fn logical_and_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let inclusive_or_expression = Self::inclusive_or_expression(tokens)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(inclusive_or_expression),
        };

        let out = match peek {
            Token::LogicalAnd => {
                tokens.next();
                let right = Self::inclusive_or_expression(tokens)?;
                Expression::Logical {
                    left: Box::new(inclusive_or_expression),
                    right: Box::new(right),
                    operator: LogicalOperator::And,
                }
            }
            _ => inclusive_or_expression,
        };

        Ok(out)
    }

    fn inclusive_or_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let exclusive_or_expression = Self::exclusive_or_expression(tokens)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(exclusive_or_expression),
        };

        let out = match peek {
            Token::BitwiseOr => {
                tokens.next();
                let right = Self::exclusive_or_expression(tokens)?;
                Expression::Bitwise {
                    left: Box::new(exclusive_or_expression),
                    right: Box::new(right),
                    operator: BitwiseOperator::Or,
                }
            }
            _ => exclusive_or_expression,
        };

        Ok(out)
    }

    fn exclusive_or_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let and_expression = Self::and_expression(tokens)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(and_expression),
        };

        let out = match peek {
            Token::BitwiseXor => {
                tokens.next();
                let right = Self::and_expression(tokens)?;
                Expression::Bitwise {
                    left: Box::new(and_expression),
                    right: Box::new(right),
                    operator: BitwiseOperator::Xor,
                }
            }
            _ => and_expression,
        };

        Ok(out)
    }

    fn and_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let equality_expression = Self::equality_expression(tokens)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(equality_expression),
        };

        let out = match peek {
            Token::Ampersand => {
                tokens.next();
                let right = Self::equality_expression(tokens)?;
                Expression::Bitwise {
                    left: Box::new(equality_expression),
                    right: Box::new(right),
                    operator: BitwiseOperator::And,
                }
            }
            _ => equality_expression,
        };

        Ok(out)
    }

    fn equality_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let relation_expression = Self::relation_expression(tokens)?;

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
        let right = Self::relation_expression(tokens)?;
        Ok(Expression::Equality {
            left: Box::new(relation_expression),
            right: Box::new(right),
            operator,
        })
    }

    fn relation_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let shift_expression = Self::shift_expression(tokens)?;

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
        let right = Self::shift_expression(tokens)?;
        Ok(Expression::Relation {
            left: Box::new(shift_expression),
            right: Box::new(right),
            operator,
        })
    }

    fn shift_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let additive_expression = Self::additive_expression(tokens)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(additive_expression),
        };

        let out = match peek {
            Token::ShiftLeft => {
                tokens.next();
                let right = Self::additive_expression(tokens)?;
                Expression::Shift {
                    left: Box::new(additive_expression),
                    right: Box::new(right),
                    operator: ShiftOperator::Left,
                }
            }
            Token::ShiftRight => {
                tokens.next();
                let right = Self::additive_expression(tokens)?;
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

    fn additive_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let multiplicative_expression = Self::multiplicative_expression(tokens)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(multiplicative_expression),
        };

        let out = match peek {
            Token::Addition => {
                tokens.next();
                let right = Self::multiplicative_expression(tokens)?;
                Expression::Additive {
                    left: Box::new(multiplicative_expression),
                    right: Box::new(right),
                    operator: AdditiveOperator::Addition,
                }
            }
            Token::Negation => {
                tokens.next();
                let right = Self::multiplicative_expression(tokens)?;
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
    ) -> Result<Self> {
        let cast_expression = Self::cast_expression(tokens)?;

        let peek = match tokens.peek() {
            Some(token) => token,
            None => return Ok(cast_expression),
        };

        let out = match peek {
            Token::Star => {
                tokens.next();
                let right = Self::cast_expression(tokens)?;
                Expression::Multiplicative {
                    left: Box::new(cast_expression),
                    right: Box::new(right),
                    operator: MultiplicativeOperator::Multiply,
                }
            }
            Token::Division => {
                tokens.next();
                let right = Self::cast_expression(tokens)?;
                Expression::Multiplicative {
                    left: Box::new(cast_expression),
                    right: Box::new(right),
                    operator: MultiplicativeOperator::Divide,
                }
            }
            Token::Modulo => {
                tokens.next();
                let right = Self::cast_expression(tokens)?;
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

    fn cast_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        Self::unary_expression(tokens)
    }

    fn unary_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            Token::Increment => {
                tokens.next();
                let unary_expression = Self::unary_expression(tokens)?;
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
                let unary_expression = Self::unary_expression(tokens)?;
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
                    Token::Keyword(_) => match TypeName::try_parse(tokens)? {
                        Some(type_name) => Expression::Sizeof(SizeofExpression::Type(type_name)),
                        None => {
                            let expression = Expression::unary_expression(tokens)?;
                            Expression::Sizeof(SizeofExpression::Expression(Box::new(expression)))
                        }
                    },
                    _ => {
                        let expression = Expression::unary_expression(tokens)?;
                        Expression::Sizeof(SizeofExpression::Expression(Box::new(expression)))
                    }
                };

                Self::allow_token(tokens, Token::CloseParenthesis)?;

                out
            }
            Token::Ampersand => {
                tokens.next();
                let expression = Self::cast_expression(tokens)?;
                Expression::Unary {
                    operator: UnaryOperator::AddressOf,
                    expression: Box::new(expression),
                }
            }
            Token::Star => {
                tokens.next();
                let expression = Self::cast_expression(tokens)?;
                Expression::Dereference {
                    expression: Box::new(expression),
                }
            }
            Token::Addition => {
                tokens.next();
                let expression = Self::cast_expression(tokens)?;
                Expression::Unary {
                    operator: UnaryOperator::Plus,
                    expression: Box::new(expression),
                }
            }
            Token::Negation => {
                tokens.next();
                let expression = Self::cast_expression(tokens)?;
                Expression::Unary {
                    operator: UnaryOperator::Minus,
                    expression: Box::new(expression),
                }
            }
            Token::BitwiseNot => {
                tokens.next();
                let expression = Self::cast_expression(tokens)?;
                Expression::Unary {
                    operator: UnaryOperator::BitwiseNot,
                    expression: Box::new(expression),
                }
            }
            Token::LogicalNot => {
                tokens.next();
                let expression = Self::cast_expression(tokens)?;
                Expression::Unary {
                    operator: UnaryOperator::LogicalNot,
                    expression: Box::new(expression),
                }
            }
            _ => Self::postfix_expression(tokens)?,
        };

        Ok(out)
    }

    fn postfix_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let primary = Self::primary_expression(tokens)?;
        let peek = tokens.peek();

        let peek = match peek {
            Some(token) => token,
            None => return Ok(primary),
        };

        let out = match peek {
            // <postfix-expression> [ <expression> ]
            Token::OpenSquareBracket => {
                tokens.next();
                let expression = Expression::parse(tokens)?;
                Self::expect_token(tokens, Token::CloseSquareBracket)?;
                let variable =
                    primary
                        .get_identifier()
                        .ok_or_else(|| ParserError::UnexpectedToken {
                            unexpected: Token::OpenSquareBracket,
                            expected: vec![Token::Word("<identifier>".to_string())],
                            near_tokens: tokens.take(6).collect(),
                        })?;
                Expression::SquareBracket {
                    variable,
                    expression: Box::new(expression),
                }
            }
            // <postfix-expression> ( {<assignment-expression>}* )
            Token::OpenParenthesis => {
                tokens.next();
                let mut arguments = vec![];
                while let Some(token) = tokens.peek() {
                    match token {
                        Token::CloseParenthesis => {
                            tokens.next();
                            break;
                        }
                        _ => {
                            let expression = Expression::assignment_expression(tokens)?;
                            arguments.push(expression);
                            match tokens.peek() {
                                Some(Token::Comma) => {
                                    tokens.next();
                                }
                                Some(Token::CloseParenthesis) => {}
                                _ => {
                                    return Err(ParserError::ExpectedExpression {
                                        near_tokens: tokens.take(6).collect(),
                                    })
                                }
                            }
                        }
                    }
                }

                Expression::Call {
                    function: Box::new(primary),
                    arguments,
                }
            }
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
                let variable =
                    primary
                        .get_identifier()
                        .ok_or_else(|| ParserError::UnexpectedToken {
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
                let variable =
                    primary
                        .get_identifier()
                        .ok_or_else(|| ParserError::UnexpectedToken {
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

    fn primary_expression(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        let token = tokens
            .peek()
            .ok_or(ParserError::UnexpectedEndOfInput)?
            .clone();

        let out = match token {
            Token::Word(identifier) => {
                tokens.next();
                Expression::Identifier(identifier.to_string())
            }
            Token::Constant { .. } => Expression::Constant(Constant::parse(tokens)?),
            Token::StringLiteral(string_literal) => match string_literal {
                StringLiteral::ByteString(bytes) => {
                    let string = bytes.iter().collect();
                    Expression::String(string)
                }
            },
            Token::OpenParenthesis => {
                tokens.next();

                // cast
                if let Some(type_name) = TypeName::try_parse(tokens)? {
                    Self::expect_token(tokens, Token::CloseParenthesis)?;
                    let expression = Self::cast_expression(tokens)?;
                    Expression::Cast {
                        type_name,
                        expression: Box::new(expression),
                    }
                } else {
                    let expression = Expression::parse(tokens)?;
                    Self::expect_token(tokens, Token::CloseParenthesis)?;
                    expression
                }
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
    pub(crate) fn get_identifier(self) -> Option<String> {
        match self {
            Expression::Identifier(identifier) => Some(identifier),
            Expression::StructMember { member, .. } => Some(member),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{
        Assignment as TokenAssignment, Constant as TokenConstant, Keyword, StringLiteral, Token,
    };
    use crate::parser::constant::Constant;
    use crate::parser::declaration::specifier_qualifier::SpecifierQualifier;
    use crate::parser::declaration::type_qualifier::TypeQualifier;
    use crate::parser::declaration::type_specifier::TypeSpecifier;
    use rstest::rstest;

    #[rstest]
    #[case::identifier(vec![Token::Word("foo".to_string())], Expression::Identifier("foo".to_string()))]
    #[case::enumeration_constant(vec![Token::Word("bar".to_string())], Expression::Identifier("bar".to_string()))]
    #[case::signed_constant(vec![Token::Constant(TokenConstant::SignedInteger(42))], Expression::Constant(Constant::Integer(42)))]
    #[case::unsigned_constant(vec![Token::Constant(TokenConstant::UnsignedInteger(42))], Expression::Constant(Constant::Integer(42)))]
    #[case::character_constant(vec![Token::Constant(TokenConstant::Character('a'))], Expression::Constant(Constant::Character('a')))]
    #[case::decimal_constant(vec![Token::Constant(TokenConstant::Decimal(42.0))], Expression::Constant(Constant::Float(42.0)))]
    #[case::string_u8(vec![Token::StringLiteral(StringLiteral::ByteString(vec!['h', 'e', 'l', 'l', 'o']))], Expression::String("hello".to_string()))]
    #[case::parenthesis(vec![Token::OpenParenthesis, Token::Word("foo".to_string()), Token::CloseParenthesis], Expression::Identifier("foo".to_string()))]
    fn primary_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let result = Expression::primary_expression(&mut input.into_iter().peekable()).unwrap();
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
        let result = Expression::postfix_expression(&mut input.into_iter().peekable()).unwrap();
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
        Expression::Sizeof(SizeofExpression::Type(TypeName {
            specifier_qualifier: vec![SpecifierQualifier::TypeSpecifier(TypeSpecifier::Int)],
            abstract_declarator: None,
        })),
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
        Expression::Dereference {
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
        let result = Expression::unary_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::cast_type(
        vec![
            Token::OpenParenthesis,
            Token::Keyword(Keyword::Const),
            Token::Keyword(Keyword::Int),
            Token::CloseParenthesis,
            Token::Word("foo".to_string()),
        ],
        Expression::Cast {
            type_name: TypeName {
                specifier_qualifier: vec![
                    SpecifierQualifier::TypeQualifier(TypeQualifier::Const),
                    SpecifierQualifier::TypeSpecifier(TypeSpecifier::Int)
                ],
                abstract_declarator: None,
            },
            expression: Box::new(Expression::Identifier("foo".to_string())),
        },
    )]
    fn test_cast_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let result = Expression::cast_expression(&mut input.into_iter().peekable()).unwrap();
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
        let result =
            Expression::multiplicative_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
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
        let result = Expression::additive_expression(&mut input.into_iter().peekable()).unwrap();
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
        let result = Expression::shift_expression(&mut input.into_iter().peekable()).unwrap();
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
        let result = Expression::relation_expression(&mut input.into_iter().peekable()).unwrap();
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
        let result = Expression::equality_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_and_expression() {
        let input = vec![
            Token::Word("foo".to_string()),
            Token::Ampersand,
            Token::Word("bar".to_string()),
        ];
        let expected = Expression::Bitwise {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: BitwiseOperator::And,
        };

        let result = Expression::and_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_exclusive_or_expression() {
        let input = vec![
            Token::Word("foo".to_string()),
            Token::BitwiseXor,
            Token::Word("bar".to_string()),
        ];
        let expected = Expression::Bitwise {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: BitwiseOperator::Xor,
        };

        let result =
            Expression::exclusive_or_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_inclusive_or_expression() {
        let input = vec![
            Token::Word("foo".to_string()),
            Token::BitwiseOr,
            Token::Word("bar".to_string()),
        ];
        let expected = Expression::Bitwise {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: BitwiseOperator::Or,
        };

        let result =
            Expression::inclusive_or_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_logical_and_expression() {
        let input = vec![
            Token::Word("foo".to_string()),
            Token::LogicalAnd,
            Token::Word("bar".to_string()),
        ];
        let expected = Expression::Logical {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: LogicalOperator::And,
        };

        let result = Expression::logical_and_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_logical_or_expression() {
        let input = vec![
            Token::Word("foo".to_string()),
            Token::LogicalOr,
            Token::Word("bar".to_string()),
        ];
        let expected = Expression::Logical {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
            operator: LogicalOperator::Or,
        };

        let result = Expression::logical_or_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_conditional_expression() {
        let input = vec![
            Token::Word("foo".to_string()),
            Token::QuestionMark,
            Token::Word("bar".to_string()),
            Token::Colon,
            Token::Word("baz".to_string()),
        ];
        let expected = Expression::Ternary {
            condition: Box::new(Expression::Identifier("foo".to_string())),
            true_expression: Box::new(Expression::Identifier("bar".to_string())),
            false_expression: Box::new(Expression::Identifier("baz".to_string())),
        };

        let result = Expression::conditional_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::assigment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::Equal),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::Assign,
        },
    )]
    #[case::add_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::PlusEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::AddAssign,
        },
    )]
    #[case::subtract_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::MinusEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::SubtractAssign,
        },
    )]
    #[case::multiply_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::MultiplyEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::MultiplyAssign,
        },
    )]
    #[case::divide_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::DivideEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::DivideAssign,
        },
    )]
    #[case::modulo_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::ModuloEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::ModuloAssign,
        },
    )]
    #[case::shift_left_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::ShiftLeftEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::LeftShiftAssign,
        },
    )]
    #[case::shift_right_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::ShiftRightEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::RightShiftAssign,
        },
    )]
    #[case::and_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::AndEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::BitwiseAndAssign,
        },
    )]
    #[case::xor_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::XorEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::BitwiseXorAssign,
        },
    )]
    #[case::or_assignment(
        vec![
            Token::Word("foo".to_string()),
            Token::Assignment(TokenAssignment::OrEqual),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Identifier("foo".to_string())),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::BitwiseOrAssign,
        },
    )]
    #[case::assign_to_assign_member(
        vec![
            Token::Word("foo".to_string()),
            Token::Dot,
            Token::Word("bar".to_string()),
            Token::Assignment(Equal),
            Token::Word("baz".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::StructMember {
                variable: "foo".to_string(),
                member: "bar".to_string(),
            }),
            expression: Box::new(Expression::Identifier("baz".to_string())),
            operator: AssignmentOperator::Assign,
        },
    )]
    #[case::assign_to_assign_dereference(
        vec![
            Token::Star,
            Token::Word("foo".to_string()),
            Token::Assignment(Equal),
            Token::Word("bar".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::Dereference {
                expression: Box::new(Expression::Identifier("foo".to_string())),
            }),
            expression: Box::new(Expression::Identifier("bar".to_string())),
            operator: AssignmentOperator::Assign,
        },
    )]
    #[case::assign_to_assign_square_bracket(
        vec![
            Token::Word("foo".to_string()),
            Token::OpenSquareBracket,
            Token::Word("bar".to_string()),
            Token::CloseSquareBracket,
            Token::Assignment(Equal),
            Token::Word("baz".to_string()),
        ],
        Expression::Assignment {
            variable: Box::new(Expression::SquareBracket {
                variable: "foo".to_string(),
                expression: Box::new(Expression::Identifier("bar".to_string())),
            }),
            expression: Box::new(Expression::Identifier("baz".to_string())),
            operator: AssignmentOperator::Assign,
        },
    )]
    fn test_assignment_expression(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let result = Expression::assignment_expression(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_comma_expression() {
        let input = vec![
            Token::Word("foo".to_string()),
            Token::Comma,
            Token::Word("bar".to_string()),
        ];
        let expected = Expression::Comma {
            left: Box::new(Expression::Identifier("foo".to_string())),
            right: Box::new(Expression::Identifier("bar".to_string())),
        };

        let result = Expression::parse(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::cast_cast_post_increment(
        vec![
            Token::OpenParenthesis,
            Token::Keyword(Keyword::Char),
            Token::CloseParenthesis,
            Token::OpenParenthesis,
            Token::OpenParenthesis,
            Token::Keyword(Keyword::Int),
            Token::CloseParenthesis,
            Token::Word("foo".to_string()),
            Token::Increment,
            Token::CloseParenthesis,
        ],
        Expression::Cast {
            type_name: TypeName {
                specifier_qualifier: vec![SpecifierQualifier::TypeSpecifier(TypeSpecifier::Char)],
                abstract_declarator: None,
            },
            expression: Box::new(
                Expression::Cast {
                    type_name: TypeName {
                        specifier_qualifier: vec![SpecifierQualifier::TypeSpecifier(TypeSpecifier::Int)],
                        abstract_declarator: None,
                    },
                    expression: Box::new(
                        Expression::PostOperation {
                            variable: "foo".to_string(),
                            operator: InstantOperator::Increment,
                        },
                    ),
                },
            ),
        },
    )]
    fn test_edge_cases(#[case] input: Vec<Token>, #[case] expected: Expression) {
        let result = Expression::parse(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }
}
