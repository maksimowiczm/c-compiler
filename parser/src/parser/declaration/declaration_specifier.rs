use crate::lexer::Token;
use crate::parser::declaration::storage_class::StorageClass;
use crate::parser::declaration::type_qualifier::TypeQualifier;
use crate::parser::declaration::type_specifier::TypeSpecifier;
use crate::parser::{ParserError, Result, TryParse};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DeclarationSpecifier {
    StorageClass(StorageClass),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

impl TryParse for DeclarationSpecifier {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        let token = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(_) => {
                if let Some(storage_class) = StorageClass::try_parse(tokens)? {
                    Some(DeclarationSpecifier::StorageClass(storage_class))
                } else if let Some(type_specifier) = TypeSpecifier::try_parse(tokens)? {
                    Some(DeclarationSpecifier::TypeSpecifier(type_specifier))
                } else {
                    TypeQualifier::try_parse(tokens)?.map(DeclarationSpecifier::TypeQualifier)
                }
            }
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
    #[case::storage_class(
        vec![Token::Keyword(Keyword::Extern)],
        DeclarationSpecifier::StorageClass(StorageClass::Extern)
    )]
    #[case::type_specifier(
        vec![Token::Keyword(Keyword::Int)],
        DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Int)
    )]
    #[case::type_qualifier(
        vec![Token::Keyword(Keyword::Const)],
        DeclarationSpecifier::TypeQualifier(TypeQualifier::Const)
    )]
    fn test_declaration_specifier(
        #[case] input: Vec<Token>,
        #[case] expected: DeclarationSpecifier,
    ) {
        let mut input = input.into_iter().peekable();
        let result = DeclarationSpecifier::try_parse(&mut input)
            .unwrap()
            .unwrap();
        assert_eq!(result, expected);
    }
}
