use crate::lexer::Token;
use crate::parser::declaration::type_qualifier::TypeQualifier;
use crate::parser::declaration::type_specifier::TypeSpecifier;
use crate::parser::{ParserError, Result, TryParse};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum SpecifierQualifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

impl TryParse for SpecifierQualifier {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        let token = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(_) => {
                if let Some(type_specifier) = TypeSpecifier::try_parse(tokens)? {
                    Some(SpecifierQualifier::TypeSpecifier(type_specifier))
                } else {
                    TypeQualifier::try_parse(tokens)?.map(SpecifierQualifier::TypeQualifier)
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
    use crate::lexer::Keyword;
    use crate::parser::declaration::type_qualifier::TypeQualifier;
    use crate::parser::declaration::type_specifier::TypeSpecifier;

    #[test]
    fn test_parse_type_specifier() {
        let mut tokens = vec![Token::Keyword(Keyword::Int)].into_iter().peekable();
        let specifier_qualifier = SpecifierQualifier::try_parse(&mut tokens).unwrap().unwrap();
        assert_eq!(
            specifier_qualifier,
            SpecifierQualifier::TypeSpecifier(TypeSpecifier::Int)
        );
    }

    #[test]
    fn test_parse_type_qualifier() {
        let mut tokens = vec![Token::Keyword(Keyword::Const)].into_iter().peekable();
        let specifier_qualifier = SpecifierQualifier::try_parse(&mut tokens).unwrap().unwrap();
        assert_eq!(
            specifier_qualifier,
            SpecifierQualifier::TypeQualifier(TypeQualifier::Const)
        );
    }
}
