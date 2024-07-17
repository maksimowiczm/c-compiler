use crate::lexer::{Keyword, Token};
use crate::parser::{Parse, ParserError, Result};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum StorageClass {
    Auto,
    Register,
    Static,
    Extern,
    Typedef,
}

impl Parse for StorageClass {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
        let out = match token {
            Token::Keyword(Keyword::Auto) => StorageClass::Auto,
            Token::Keyword(Keyword::Register) => StorageClass::Register,
            Token::Keyword(Keyword::Static) => StorageClass::Static,
            Token::Keyword(Keyword::Extern) => StorageClass::Extern,
            Token::Keyword(Keyword::Typedef) => StorageClass::Typedef,
            _ => {
                return Err(ParserError::UnexpectedToken {
                    unexpected: token,
                    expected: vec![
                        Token::Keyword(Keyword::Auto),
                        Token::Keyword(Keyword::Register),
                        Token::Keyword(Keyword::Static),
                        Token::Keyword(Keyword::Extern),
                        Token::Keyword(Keyword::Typedef),
                    ],
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
    use crate::lexer::{Keyword, Token};
    use rstest::rstest;

    #[rstest]
    #[case::auto(vec![Token::Keyword(Keyword::Auto)], StorageClass::Auto)]
    #[case::register(vec![Token::Keyword(Keyword::Register)], StorageClass::Register)]
    #[case::static_(vec![Token::Keyword(Keyword::Static)], StorageClass::Static)]
    #[case::extern_(vec![Token::Keyword(Keyword::Extern)], StorageClass::Extern)]
    #[case::typedef(vec![Token::Keyword(Keyword::Typedef)], StorageClass::Typedef)]
    fn test_storage_class(#[case] input: Vec<Token>, #[case] expected: StorageClass) {
        let mut tokens = input.into_iter().peekable();
        let result = StorageClass::parse(&mut tokens).unwrap();
        assert_eq!(result, expected);
    }
}
