use super::type_qualifier::TypeQualifier;
use crate::parser::constant::Constant;
use crate::parser::TryParse;
use crate::{
    lexer::Token,
    parser::{Parse, ParserError, Result},
};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub enum Declarator {
    Abstract(AbstractDeclarator),
    Identifier {
        identifier: String,
        pointer: Option<Pointer>,
    },
    Wrapped {
        inner: Box<Declarator>,
        pointer: Option<Pointer>,
    },
    Array {
        inner: Box<Declarator>,
        size: Option<Constant>,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[allow(dead_code)]
pub struct AbstractDeclarator {
    pub pointer: Option<Pointer>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Pointer {
    pub type_qualifiers: Vec<TypeQualifier>,
    pub inner_pointer: Option<Box<Pointer>>,
}

impl TryParse for Declarator {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        let pointer = Pointer::try_parse(tokens)?;

        let _ = match tokens.peek() {
            None if pointer.is_none() => return Ok(None),
            None => return Err(ParserError::UnexpectedEndOfInput),
            Some(token) => token,
        };

        let token = tokens.peek().unwrap();

        // direct-declarator:
        // abstract-direct-declarator:
        let declarator = match token {
            Token::Word(identifier) => {
                let identifier = identifier.clone();
                tokens.next();
                Declarator::Identifier {
                    identifier,
                    pointer,
                }
            }
            Token::OpenParenthesis => {
                tokens.next();
                let declarator = Declarator::parse(tokens)?;

                <Self as TryParse>::expect_token(tokens, Token::CloseParenthesis)?;
                Declarator::Wrapped {
                    inner: Box::new(declarator),
                    pointer,
                }
            }
            _ => Declarator::Abstract(AbstractDeclarator { pointer }),
        };

        let peek = match tokens.peek() {
            Some(peek) => peek,
            None => return Ok(Some(declarator)),
        };

        let out = match peek {
            Token::OpenSquareBracket => {
                tokens.next();
                let size = Constant::try_parse(tokens)?;
                <Self as TryParse>::expect_token(tokens, Token::CloseSquareBracket)?;

                Declarator::Array {
                    inner: Box::new(declarator),
                    size,
                }
            }
            _ => declarator,
        };

        Ok(Some(out))
    }
}

impl Parse for Declarator {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        Self::try_parse(tokens)?.ok_or_else(|| ParserError::ExpectedDeclarator {
            near_tokens: tokens.take(6).collect(),
        })
    }
}

impl TryParse for AbstractDeclarator {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        match Declarator::try_parse(tokens)? {
            Some(Declarator::Abstract(abstract_declarator)) => Ok(Some(abstract_declarator)),
            None => Ok(None),
            _ => Err(ParserError::ExpectedAbstractDeclarator {
                near_tokens: tokens.take(6).collect(),
            }),
        }
    }
}

impl TryParse for Pointer {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        let _ = match tokens.peek() {
            Some(Token::Star) => tokens.next(),
            _ => return Ok(None),
        };

        let mut type_qualifiers = vec![];
        while let Some(qualifier) = TypeQualifier::try_parse(tokens)? {
            type_qualifiers.push(qualifier);
        }

        let inner_pointer = Pointer::try_parse(tokens)?.map(Box::new);

        Ok(Some(Pointer {
            type_qualifiers,
            inner_pointer,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Constant as TokenConstant, Keyword};
    use rstest::rstest;

    #[rstest]
    #[case::pointer(
        vec![Token::Star],
        Pointer {
            type_qualifiers: vec![],
            inner_pointer: None
        }
    )]
    #[case::const_pointer(
        vec![Token::Star, Token::Keyword(Keyword::Const)],
        Pointer {
            type_qualifiers: vec![TypeQualifier::Const],
            inner_pointer: None
        }
    )]
    #[case::const_volatile_pointer(
        vec![
            Token::Star,
            Token::Keyword(Keyword::Const),
            Token::Keyword(Keyword::Volatile)
        ],
        Pointer {
            type_qualifiers: vec![TypeQualifier::Const, TypeQualifier::Volatile],
            inner_pointer: None
        }
    )]
    #[case::double_pointer(
        vec![Token::Star, Token::Star],
        Pointer {
            type_qualifiers: vec![],
            inner_pointer: Some(Box::new(Pointer {
                type_qualifiers: vec![],
                inner_pointer: None
            }))
        }
    )]
    fn test_pointer(#[case] input: Vec<Token>, #[case] expected: Pointer) {
        let mut tokens = input.into_iter().peekable();
        let result = Pointer::try_parse(&mut tokens).unwrap().unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::identifier(
        vec![Token::Word("foo".to_string())],
        Declarator::Identifier {
            identifier: "foo".to_string(),
            pointer: None
        }
    )]
    #[case::parenthesis(
        vec![Token::OpenParenthesis, Token::Word("foo".to_string()), Token::CloseParenthesis],
        Declarator::Wrapped {
            inner: Box::new(Declarator::Identifier {
                identifier: "foo".to_string(),
                pointer: None
            }),
            pointer: None
        }
    )]
    #[case::empty_array(
        vec![Token::Word("foo".to_string()), Token::OpenSquareBracket, Token::CloseSquareBracket],
        Declarator::Array {
            size: None,
            inner: Box::new(Declarator::Identifier {
                identifier: "foo".to_string(),
                pointer: None
            })
        }
    )]
    #[case::sized_array(
        vec![
            Token::Word("foo".to_string()),
            Token::OpenSquareBracket,
            Token::Constant(TokenConstant::SignedInteger(42)),
            Token::CloseSquareBracket
        ],
        Declarator::Array {
            size: Some(Constant::Integer(42)),
            inner: Box::new(Declarator::Identifier {
                identifier: "foo".to_string(),
                pointer: None
            })
        }
    )]
    #[case::pointer_identifier(
        vec![Token::Star, Token::Word("foo".to_string())],
        Declarator::Identifier {
            identifier: "foo".to_string(),
            pointer: Some(Pointer {
                type_qualifiers: vec![],
                inner_pointer: None
            })
        }
    )]
    fn test_declarator(#[case] input: Vec<Token>, #[case] expected: Declarator) {
        let mut tokens = input.into_iter().peekable();
        let result = Declarator::parse(&mut tokens).unwrap();
        assert_eq!(result, expected);
    }
}
