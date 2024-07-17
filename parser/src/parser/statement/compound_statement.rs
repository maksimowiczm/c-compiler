use crate::lexer::Token;
use crate::parser::statement::statement::Statement;
use crate::parser::Parse;
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct CompoundStatement {
    pub(crate) statements: Vec<Statement>,
}

impl Parse for CompoundStatement {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> crate::parser::Result<Self>
    where
        Self: Sized,
    {
        Self::expect_token(tokens, Token::OpenBrace)?;
        let mut statements = Vec::new();
        while let Some(token) = tokens.peek() {
            if token == &Token::CloseBrace {
                break;
            }
            statements.push(Statement::parse(tokens)?);
        }

        Self::expect_token(tokens, Token::CloseBrace)?;

        Ok(CompoundStatement { statements })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Token;
    use crate::parser::expression::Expression;
    use rstest::rstest;

    #[rstest]
    #[case::empty(vec![Token::OpenBrace, Token::CloseBrace], CompoundStatement { statements: vec![] })]
    #[case::single_statement(
        vec![
            Token::OpenBrace,
            Token::SemiColon,
            Token::CloseBrace
        ],
        CompoundStatement {
            statements: vec![Statement::Expression(Expression::Empty)]
        }
    )]
    fn test_compound_statement(#[case] input: Vec<Token>, #[case] expected: CompoundStatement) {
        let mut input = input.into_iter().peekable();
        let result = CompoundStatement::parse(&mut input).unwrap();
        assert_eq!(result, expected);
    }
}
