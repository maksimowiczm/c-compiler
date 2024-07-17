use crate::lexer::{Keyword, Token};
use crate::parser::expression::Expression;
use crate::parser::{Context, Parse, ParserError};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Statement {
    Expression(Expression),
}

impl Parse for Statement {
    fn parse(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            _ => Statement::expression_statement(tokens, context)?,
        };

        Ok(out)
    }
}

impl Statement {
    fn expression_statement(
        tokens: &mut Peekable<impl Iterator<Item = Token>>,
        context: &Context,
    ) -> Result<Self, ParserError> {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            Token::SemiColon => {
                tokens.next();
                Statement::Expression(Expression::Empty)
            }
            _ => Expression::parse(tokens, context).map(Statement::Expression)?,
        };

        Ok(out)
    }
}

#[cfg(test)]
mod tests {
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
}
