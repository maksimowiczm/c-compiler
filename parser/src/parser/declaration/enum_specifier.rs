use crate::lexer::{Keyword, Token};
use crate::parser::constant::Constant;
use crate::parser::{Parse, ParserError, Result};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum EnumSpecifier {
    Identifier {
        identifier: String,
    },
    List {
        list: Vec<Enumerator>,
    },
    IdentifierList {
        identifier: String,
        list: Vec<Enumerator>,
    },
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Enumerator {
    pub identifier: String,
    pub constant_expression: Option<Constant>,
}

impl Parse for EnumSpecifier {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        Self::expect_token(tokens, Token::Keyword(Keyword::Enum))?;

        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Word(identifier) => {
                let peek = match tokens.peek() {
                    Some(token) => token,
                    None => return Ok(EnumSpecifier::Identifier { identifier }),
                };

                match peek {
                    Token::OpenBrace => {
                        tokens.next();
                        EnumSpecifier::IdentifierList {
                            identifier,
                            list: eat_list(tokens)?,
                        }
                    }
                    _ => EnumSpecifier::Identifier { identifier },
                }
            }
            Token::OpenBrace => EnumSpecifier::List {
                list: eat_list(tokens)?,
            },
            _ => {
                return Err(ParserError::UnexpectedToken {
                    unexpected: token,
                    expected: vec![Token::Word("identifier".to_string()), Token::OpenBrace],
                    near_tokens: tokens.take(6).collect(),
                })
            }
        };

        return Ok(out);

        fn eat_list(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Vec<Enumerator>> {
            let mut list = vec![];
            while let Some(token) = tokens.peek() {
                match token {
                    Token::CloseBrace => {
                        tokens.next();
                        break;
                    }
                    _ => {
                        list.push(Enumerator::parse(tokens)?);
                    }
                }
            }

            Ok(list)
        }
    }
}

impl Parse for Enumerator {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
        let identifier = match token {
            Token::Word(identifier) => identifier,
            _ => {
                return Err(ParserError::ExpectedIdentifier {
                    unexpected: token,
                    near_tokens: tokens.take(6).collect(),
                })
            }
        };

        let peek = tokens.peek();
        let constant_expression = match peek {
            Some(Token::Assignment(_)) => {
                tokens.next();
                Some(Constant::parse(tokens)?)
            }
            _ => None,
        };

        Ok(Enumerator {
            identifier,
            constant_expression,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Assignment, Constant as TokenConstant};
    use rstest::rstest;

    #[rstest]
    #[case::identifier(
        vec![
            Token::Keyword(Keyword::Enum),
            Token::Word("identifier".to_string())
        ],
        EnumSpecifier::Identifier {
            identifier: "identifier".to_string()
        }
    )]
    #[case::empty_list(
        vec![
            Token::Keyword(Keyword::Enum),
            Token::OpenBrace,
            Token::CloseBrace
        ],
        EnumSpecifier::List { list: vec![] }
    )]
    #[case::list(
        vec![
            Token::Keyword(Keyword::Enum),
            Token::OpenBrace,
            Token::Word("identifier".to_string()),
            Token::CloseBrace
        ],
        EnumSpecifier::List {
            list: vec![Enumerator {
                identifier: "identifier".to_string(),
                constant_expression: None
            }]
        }
    )]
    #[case::list_with_constant(
        vec![
            Token::Keyword(Keyword::Enum),
            Token::OpenBrace,
            Token::Word("identifier".to_string()),
            Token::Assignment(Assignment::Equal),
            Token::Constant(TokenConstant::SignedInteger(1)),
            Token::CloseBrace
        ],
        EnumSpecifier::List {
            list: vec![Enumerator {
                identifier: "identifier".to_string(),
                constant_expression: Some(Constant::Integer(1))
            }]
        }
    )]
    #[case::identifier_list(
        vec![
            Token::Keyword(Keyword::Enum),
            Token::Word("identifier".to_string()),
            Token::OpenBrace,
            Token::Word("identifier".to_string()),
            Token::CloseBrace
        ],
        EnumSpecifier::IdentifierList {
            identifier: "identifier".to_string(),
            list: vec![Enumerator {
                identifier: "identifier".to_string(),
                constant_expression: None
            }]
        }
    )]
    fn test_enum_specifier(#[case] input: Vec<Token>, #[case] expected: EnumSpecifier) {
        let mut tokens = input.into_iter().peekable();
        let result = EnumSpecifier::parse(&mut tokens).unwrap();
        assert_eq!(result, expected);
    }
}
