use crate::lexer::Token;
use crate::parser::{Parse, ParserError, Result};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum StructOrUnionSpecifier {
    Identifier {
        identifier: String,
    },
    List {
        list: Vec<StructDeclaration>,
    },
    IdentifierList {
        identifier: String,
        list: Vec<StructDeclaration>,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct StructDeclaration {}

impl Parse for StructOrUnionSpecifier {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Word(identifier) => {
                let next_token = match tokens.peek() {
                    Some(token) => token,
                    None => return Ok(StructOrUnionSpecifier::Identifier { identifier }),
                };

                match next_token {
                    Token::OpenBrace => todo!(),
                    _ => StructOrUnionSpecifier::Identifier { identifier },
                }
            }
            Token::OpenBrace => {
                todo!()
            }
            _ => {
                return Err(ParserError::ExpectedIdentifier {
                    unexpected: token,
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
    use crate::lexer::Keyword;
    use rstest::rstest;

    #[rstest]
    #[case::identifier(
        vec![Token::Word("identifier".to_string())],
        StructOrUnionSpecifier::Identifier {
            identifier: "identifier".to_string()
        }
    )]
    #[case::empty_list(
        vec![Token::OpenBrace, Token::CloseBrace],
        StructOrUnionSpecifier::List { list: vec![] }
    )]
    #[case::list(
        vec![
            Token::OpenBrace,
            Token::Keyword(Keyword::Int),
            Token::Word("identifier".to_string()),
            Token::SemiColon,
            Token::CloseBrace
        ],
        todo!()
    )]
    fn test_struct_or_union_specifier(
        #[case] input: Vec<Token>,
        #[case] expected: StructOrUnionSpecifier,
    ) {
        let mut tokens = input.into_iter().peekable();
        let result = StructOrUnionSpecifier::parse(&mut tokens).unwrap();
        assert_eq!(result, expected);
    }
}
