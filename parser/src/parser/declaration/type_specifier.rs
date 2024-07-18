use crate::lexer::{Keyword, Token};
use crate::parser::declaration::enum_specifier::EnumSpecifier;
use crate::parser::{Parse, ParserError, Result};
use std::iter::Peekable;
use crate::parser::declaration::struct_or_union_specifier::StructOrUnionSpecifier;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Struct(StructOrUnionSpecifier),
    Union(StructOrUnionSpecifier),
    Enum(EnumSpecifier),
    Typedef(String),
}

impl Parse for TypeSpecifier {
    fn parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self>
    where
        Self: Sized,
    {
        let peek = tokens.peek().ok_or(ParserError::UnexpectedEndOfInput)?;

        let out = match peek {
            Token::Keyword(keyword) => match keyword {
                Keyword::Void => {
                    tokens.next();
                    TypeSpecifier::Void
                }
                Keyword::Char => {
                    tokens.next();
                    TypeSpecifier::Char
                }
                Keyword::Short => {
                    tokens.next();
                    TypeSpecifier::Short
                }
                Keyword::Int => {
                    tokens.next();
                    TypeSpecifier::Int
                }
                Keyword::Long => {
                    tokens.next();
                    TypeSpecifier::Long
                }
                Keyword::Float => {
                    tokens.next();
                    TypeSpecifier::Float
                }
                Keyword::Double => {
                    tokens.next();
                    TypeSpecifier::Double
                }
                Keyword::Signed => {
                    tokens.next();
                    TypeSpecifier::Signed
                }
                Keyword::Unsigned => {
                    tokens.next();
                    TypeSpecifier::Unsigned
                }
                Keyword::Struct | Keyword::Union => {
                    let keyword = match tokens.next() {
                        Some(Token::Keyword(keyword)) => keyword,
                        _ => unreachable!(),
                    };

                    let specifier = StructOrUnionSpecifier::parse(tokens)?;

                    match keyword {
                        Keyword::Struct => TypeSpecifier::Struct(specifier),
                        Keyword::Union => TypeSpecifier::Union(specifier),
                        _ => unreachable!(),
                    }
                }
                Keyword::Enum => TypeSpecifier::Enum(EnumSpecifier::parse(tokens)?),
                Keyword::Typedef => {
                    tokens.next();
                    let token = tokens.next().ok_or(ParserError::UnexpectedEndOfInput)?;
                    match token {
                        Token::Word(word) => TypeSpecifier::Typedef(word),
                        _ => {
                            return Err(ParserError::ExpectedTypeSpecifier {
                                unexpected: token.clone(),
                                near_tokens: tokens.take(6).collect(),
                            })
                        }
                    }
                }
                _ => {
                    return Err(ParserError::ExpectedTypeSpecifier {
                        unexpected: peek.clone(),
                        near_tokens: tokens.take(6).collect(),
                    })
                }
            },
            _ => {
                return Err(ParserError::ExpectedTypeSpecifier {
                    unexpected: peek.clone(),
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
    use crate::parser::declaration::enum_specifier::Enumerator;
    use rstest::rstest;

    #[rstest]
    #[case::void(
        vec![Token::Keyword(Keyword::Void)],
        TypeSpecifier::Void
    )]
    #[case::char(
        vec![Token::Keyword(Keyword::Char)],
        TypeSpecifier::Char
    )]
    #[case::short(
        vec![Token::Keyword(Keyword::Short)],
        TypeSpecifier::Short
    )]
    #[case::int(
        vec![Token::Keyword(Keyword::Int)],
        TypeSpecifier::Int
    )]
    #[case::long(
        vec![Token::Keyword(Keyword::Long)],
        TypeSpecifier::Long
    )]
    #[case::float(
        vec![Token::Keyword(Keyword::Float)],
        TypeSpecifier::Float
    )]
    #[case::double(
        vec![Token::Keyword(Keyword::Double)],
        TypeSpecifier::Double
    )]
    #[case::signed(
        vec![Token::Keyword(Keyword::Signed)],
        TypeSpecifier::Signed
    )]
    #[case::unsigned(
        vec![Token::Keyword(Keyword::Unsigned)],
        TypeSpecifier::Unsigned
    )]
    #[case::struct_(
        vec![Token::Keyword(Keyword::Struct), Token::Word("foo".to_string())],
        todo!()
    )]
    #[case::union(
        vec![Token::Keyword(Keyword::Union), Token::Word("foo".to_string())],
        todo!()
    )]
    #[case::enum_identifier(
        vec![Token::Keyword(Keyword::Enum), Token::Word("foo".to_string())],
        TypeSpecifier::Enum(EnumSpecifier::Identifier{ identifier: "foo".to_string() })
    )]
    #[case::enum_empty_list(
        vec![Token::Keyword(Keyword::Enum), Token::OpenBrace, Token::CloseBrace],
        TypeSpecifier::Enum(EnumSpecifier::List{ list: vec![] })
    )]
    #[case::enum_list(
        vec![
            Token::Keyword(Keyword::Enum),
            Token::OpenBrace,
            Token::Word("foo".to_string()),
            Token::CloseBrace
        ],
        TypeSpecifier::Enum(EnumSpecifier::List{ list: vec![Enumerator{ identifier: "foo".to_string(), constant_expression: None }] })
    )]
    #[case::typedef(
        vec![Token::Keyword(Keyword::Typedef), Token::Word("foo".to_string())],
        TypeSpecifier::Typedef("foo".to_string())
    )]
    fn test_type_specifier(#[case] input: Vec<Token>, #[case] expected: TypeSpecifier) {
        let mut input = input.into_iter().peekable();
        let result = TypeSpecifier::parse(&mut input).unwrap();
        assert_eq!(result, expected);
    }
}
