use crate::lexer::{Keyword, Token};
use crate::parser::{ParserError, Result, TryParse};
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

impl TryParse for StorageClass {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            Token::Keyword(Keyword::Auto) => Some(StorageClass::Auto),
            Token::Keyword(Keyword::Register) => Some(StorageClass::Register),
            Token::Keyword(Keyword::Static) => Some(StorageClass::Static),
            Token::Keyword(Keyword::Extern) => Some(StorageClass::Extern),
            Token::Keyword(Keyword::Typedef) => Some(StorageClass::Typedef),
            _ => None,
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
        let result = StorageClass::try_parse(&mut tokens).unwrap().unwrap();
        assert_eq!(result, expected);
    }
}
