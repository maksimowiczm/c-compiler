use crate::lexer::{Keyword, Token};
use crate::parser::{Parse, ParserError, Result};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TypeInfo {
    Keyword(String),
    Struct {
        name: String,
        // members: Option<Vec<Declaration>>,
    },
    Union(String),
    Enum {
        name: String,
        values: Option<Vec<String>>,
    },
    Typedef(String),
}

impl Parse for TypeInfo {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match token {
            Token::Keyword(Keyword::Struct) => {
                let identifier = match tokens.next().ok_or(ParserError::UnexpectedEndOfInput)? {
                    Token::Word(identifier) => identifier,
                    unexpected => {
                        return Err(ParserError::UnexpectedToken {
                            unexpected,
                            expected: vec![Token::Word("<identifier>".to_string())],
                            near_tokens: tokens.take(6).collect(),
                        })
                    }
                };

                // todo parse members

                TypeInfo::Struct { name: identifier }
            }
            Token::Keyword(Keyword::Union) => {
                let identifier = match tokens.next().ok_or(ParserError::UnexpectedEndOfInput)? {
                    Token::Word(identifier) => identifier,
                    unexpected => {
                        return Err(ParserError::UnexpectedToken {
                            unexpected,
                            expected: vec![Token::Word("<identifier>".to_string())],
                            near_tokens: tokens.take(6).collect(),
                        })
                    }
                };

                TypeInfo::Union(identifier)
            }
            Token::Keyword(Keyword::Enum) => {
                let name = match tokens.next().ok_or(ParserError::UnexpectedEndOfInput)? {
                    Token::Word(identifier) => identifier,
                    unexpected => {
                        return Err(ParserError::ExpectedIdentifier {
                            unexpected,
                            near_tokens: tokens.take(6).collect(),
                        })
                    }
                };

                let values = if let Some(Token::OpenBrace) = tokens.peek() {
                    tokens.next();

                    let mut values = Vec::new();
                    loop {
                        match tokens.next().ok_or(ParserError::UnexpectedEndOfInput)? {
                            Token::Word(value) => values.push(value),
                            Token::CloseBrace => break,
                            unexpected => {
                                return Err(ParserError::UnexpectedToken {
                                    unexpected,
                                    expected: vec![Token::Word("<identifier>".to_string())],
                                    near_tokens: tokens.take(6).collect(),
                                })
                            }
                        }
                    }

                    Some(values)
                } else {
                    None
                };

                TypeInfo::Enum { name, values }
            }
            Token::Keyword(keyword) => {
                if keyword.is_type_specifier() {
                    TypeInfo::Keyword(keyword.to_string().to_lowercase())
                } else {
                    return Err(ParserError::ExpectedType {
                        unexpected: Token::Keyword(keyword),
                        near_tokens: tokens.take(6).collect(),
                    });
                }
            }
            token => {
                if let Token::Word(identifier) = token {
                    TypeInfo::Typedef(identifier)
                } else {
                    return Err(ParserError::ExpectedType {
                        unexpected: token,
                        near_tokens: tokens.take(6).collect(),
                    });
                }
            }
        };

        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::void(vec![Token::Keyword(Keyword::Void)], TypeInfo::Keyword("void".to_string()))]
    #[case::char(vec![Token::Keyword(Keyword::Char)], TypeInfo::Keyword("char".to_string()))]
    #[case::short(vec![Token::Keyword(Keyword::Short)], TypeInfo::Keyword("short".to_string()))]
    #[case::long(vec![Token::Keyword(Keyword::Long)], TypeInfo::Keyword("long".to_string()))]
    #[case::int(vec![Token::Keyword(Keyword::Int)], TypeInfo::Keyword("int".to_string()))]
    #[case::float(vec![Token::Keyword(Keyword::Float)], TypeInfo::Keyword("float".to_string()))]
    #[case::double(vec![Token::Keyword(Keyword::Double)], TypeInfo::Keyword("double".to_string()))]
    #[case::signed(vec![Token::Keyword(Keyword::Signed)], TypeInfo::Keyword("signed".to_string()))]
    #[case::unsigned(vec![Token::Keyword(Keyword::Unsigned)], TypeInfo::Keyword("unsigned".to_string()))]
    #[case::struct_(vec![Token::Keyword(Keyword::Struct), Token::Word("foo".to_string())], TypeInfo::Struct { name: "foo".to_string() })]
    #[case::union(vec![Token::Keyword(Keyword::Union), Token::Word("foo".to_string())], TypeInfo::Union("foo".to_string()))]
    #[case::enum_(vec![Token::Keyword(Keyword::Enum), Token::Word("foo".to_string())], TypeInfo::Enum { name: "foo".to_string(), values: None })]
    #[case::enum_declaration(
        vec![
            Token::Keyword(Keyword::Enum),
            Token::Word("foo".to_string()),
            Token::OpenBrace,
            Token::Word("bar".to_string()),
            Token::CloseBrace,
        ],
        TypeInfo::Enum {
            name: "foo".to_string(),
            values: Some(vec!["bar".to_string()]),
        }
    )]
    #[case::typedef(vec![Token::Word("bar".to_string())], TypeInfo::Typedef("bar".to_string()))]
    fn test_type_info(#[case] input: Vec<Token>, #[case] expected: TypeInfo) {
        let result = TypeInfo::parse(&mut input.into_iter().peekable()).unwrap();
        assert_eq!(result, expected);
    }
}
