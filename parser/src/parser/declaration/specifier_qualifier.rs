use crate::lexer::Token;
use crate::parser::declaration::type_qualifier::TypeQualifier;
use crate::parser::declaration::type_specifier::TypeSpecifier;
use crate::parser::{Parse, ParserError};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum SpecifierQualifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

impl Parse for SpecifierQualifier {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        let token = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(keyword) => {
                if keyword.is_type_specifier() {
                    SpecifierQualifier::TypeSpecifier(TypeSpecifier::parse(tokens)?)
                } else if keyword.is_type_qualifier() {
                    SpecifierQualifier::TypeQualifier(TypeQualifier::parse(tokens)?)
                } else {
                    return Err(ParserError::ExpectedSpecifierQualifier {
                        unexpected: token.clone(),
                        near_tokens: tokens.take(5).collect(),
                    });
                }
            }
            _ => {
                return Err(ParserError::ExpectedSpecifierQualifier {
                    unexpected: token.clone(),
                    near_tokens: tokens.take(5).collect(),
                });
            }
        };

        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Keyword;
    use crate::parser::declaration::type_qualifier::TypeQualifier;
    use crate::parser::declaration::type_specifier::TypeSpecifier;
    use crate::parser::Parse;

    #[test]
    fn test_parse_type_specifier() {
        let mut tokens = vec![Token::Keyword(Keyword::Int)].into_iter().peekable();
        let specifier_qualifier = SpecifierQualifier::parse(&mut tokens).unwrap();
        assert_eq!(
            specifier_qualifier,
            SpecifierQualifier::TypeSpecifier(TypeSpecifier::Int)
        );
    }

    #[test]
    fn test_parse_type_qualifier() {
        let mut tokens = vec![Token::Keyword(Keyword::Const)].into_iter().peekable();
        let specifier_qualifier = SpecifierQualifier::parse(&mut tokens).unwrap();
        assert_eq!(
            specifier_qualifier,
            SpecifierQualifier::TypeQualifier(TypeQualifier::Const)
        );
    }

    #[test]
    fn test_parse_unexpected_token() {
        let mut tokens = vec![Token::Keyword(Keyword::Extern)].into_iter().peekable();
        let specifier_qualifier = SpecifierQualifier::parse(&mut tokens);
        assert!(specifier_qualifier.is_err());
    }
}
