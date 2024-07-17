use crate::lexer::Token;
use crate::parser::declaration::storage_class::StorageClass;
use crate::parser::declaration::type_qualifier::TypeQualifier;
use crate::parser::declaration::type_specifier::TypeSpecifier;
use crate::parser::{Parse, ParserError};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DeclarationSpecifier {
    StorageClass(StorageClass),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

impl Parse for DeclarationSpecifier {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> crate::parser::Result<Self>
    where
        Self: Sized,
    {
        let token = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(keyword) => {
                if keyword.is_storage_class() {
                    DeclarationSpecifier::StorageClass(StorageClass::parse(tokens)?)
                } else if keyword.is_type_specifier() {
                    DeclarationSpecifier::TypeSpecifier(TypeSpecifier::parse(tokens)?)
                } else if keyword.is_type_qualifier() {
                    DeclarationSpecifier::TypeQualifier(TypeQualifier::parse(tokens)?)
                } else {
                    return Err(ParserError::UnexpectedToken {
                        unexpected: token.clone(),
                        expected: vec![], // todo
                        near_tokens: tokens.take(6).collect(),
                    });
                }
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    unexpected: token.clone(),
                    expected: vec![], // todo
                    near_tokens: tokens.take(6).collect(),
                });
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
        let result = DeclarationSpecifier::parse(&mut input).unwrap();
        assert_eq!(result, expected);
    }
}
