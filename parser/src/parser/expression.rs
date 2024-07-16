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
}

impl Parse for Expression {
    fn parse(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        Expression::primary_expression(tokens, context)
    }
}

impl Expression {
    fn primary_expression(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result<Self, ParserError> {
        let token = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Word(identifier) => {
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

#[cfg(test)]
mod tests {
    use crate::lexer::{Constant as TokenConstant, StringLiteral, Token};
    use crate::parser::constant::Constant;
    use crate::parser::expression::Expression;
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
}
