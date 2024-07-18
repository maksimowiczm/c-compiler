use derive_more::{Display, FromStr};
use std::cmp::PartialEq;
use std::iter::Peekable;

pub struct Lexer<TInput: Iterator<Item = char>> {
    input: Peekable<TInput>,
    end: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    EndOfFile,
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    SemiColon,
    Word(String),
    Keyword(Keyword),
    Constant(Constant),
    Negation,
    LogicalNot,
    BitwiseNot,
    Addition,
    Star,
    Division,
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Assignment(Assignment),
    ShiftLeft,
    ShiftRight,
    Modulo,
    BitwiseXor,
    BitwiseOr,
    Ampersand,
    Increment,
    Decrement,
    Colon,
    QuestionMark,
    Comma,
    StringLiteral(StringLiteral),
    OpenSquareBracket,
    CloseSquareBracket,
    Dot,
    Arrow,
    TripleDot,
    Hash,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assignment {
    Equal,           // =
    PlusEqual,       // +=
    MinusEqual,      // -=
    MultiplyEqual,   // *=
    DivideEqual,     // /=
    AndEqual,        // &=
    OrEqual,         // |=
    ShiftLeftEqual,  // <<=
    ShiftRightEqual, // >>=
    ModuloEqual,     // %=
    XorEqual,        // ^=
}

#[derive(Debug, PartialEq, Clone, FromStr, Display)]
pub enum Keyword {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    _Alignof,
    _Atomic,
    _Bool,
    _Complex,
    _Generic,
    _Imaginary,
    _Noreturn,
    _StaticAssert,
    _ThreadLocal,
}

impl Keyword {
    pub(crate) fn is_type_specifier(&self) -> bool {
        matches!(
            self,
            Keyword::Char
                | Keyword::Double
                | Keyword::Float
                | Keyword::Int
                | Keyword::Long
                | Keyword::Short
                | Keyword::Signed
                | Keyword::Unsigned
                | Keyword::Void
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    // every integer is 8 bytes
    SignedInteger(i64),
    UnsignedInteger(u64),
    Decimal(f64),
    Character(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringLiteral {
    ByteString(Vec<char>),
}

impl<TInput> Lexer<TInput>
where
    TInput: Iterator<Item = char>,
{
    pub fn new(input: Peekable<TInput>) -> Self {
        Lexer { input, end: false }
    }
}

impl<TInput> Iterator for Lexer<TInput>
where
    TInput: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            return None;
        }

        while let Some(ch) = self.input.next() {
            let next = match ch {
                '{' => Some(Token::OpenBrace),
                '}' => Some(Token::CloseBrace),
                '(' => Some(Token::OpenParenthesis),
                ')' => Some(Token::CloseParenthesis),
                ';' => Some(Token::SemiColon),
                '!' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::NotEqual)
                    } else {
                        Some(Token::LogicalNot)
                    }
                }
                '~' => Some(Token::BitwiseNot),
                '-' => {
                    let peek = match self.input.peek() {
                        Some(ch) => *ch,
                        None => return Some(Token::Negation),
                    };

                    if peek == '>' {
                        self.input.next();
                        return Some(Token::Arrow);
                    }

                    // check if the next character is a number
                    if peek.is_numeric() {
                        return parse_number('-', &mut self.input, Some(false));
                    }

                    // otherwise, check if it is an assignment or decrement
                    match peek {
                        '=' => {
                            self.input.next();
                            Some(Token::Assignment(Assignment::MinusEqual))
                        }
                        '-' => {
                            self.input.next();
                            Some(Token::Decrement)
                        }
                        _ => Some(Token::Negation),
                    }
                }
                '+' => {
                    let peek = match self.input.peek() {
                        Some(ch) => *ch,
                        None => return Some(Token::Addition),
                    };

                    // check if the next character is a number
                    if peek.is_numeric() {
                        self.input.next();
                        return parse_number(peek, &mut self.input, Some(false));
                    }

                    // otherwise, check if it is an assignment or increment
                    match peek {
                        '=' => {
                            self.input.next();
                            Some(Token::Assignment(Assignment::PlusEqual))
                        }
                        '+' => {
                            self.input.next();
                            Some(Token::Increment)
                        }
                        _ => Some(Token::Addition),
                    }
                }
                '*' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Assignment(Assignment::MultiplyEqual))
                    } else {
                        Some(Token::Star)
                    }
                }
                '/' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Assignment(Assignment::DivideEqual))
                    } else {
                        Some(Token::Division)
                    }
                }
                '&' => {
                    if let Some('&') = self.input.peek() {
                        self.input.next();
                        Some(Token::LogicalAnd)
                    } else if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Assignment(Assignment::AndEqual))
                    } else {
                        Some(Token::Ampersand)
                    }
                }
                '|' => {
                    if let Some('|') = self.input.peek() {
                        self.input.next();
                        Some(Token::LogicalOr)
                    } else if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Assignment(Assignment::OrEqual))
                    } else {
                        Some(Token::BitwiseOr)
                    }
                }
                '=' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Equal)
                    } else {
                        Some(Token::Assignment(Assignment::Equal))
                    }
                }
                '<' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::LessThanOrEqual)
                    } else if let Some('<') = self.input.peek() {
                        self.input.next();
                        if let Some('=') = self.input.peek() {
                            self.input.next();
                            Some(Token::Assignment(Assignment::ShiftLeftEqual))
                        } else {
                            Some(Token::ShiftLeft)
                        }
                    } else {
                        Some(Token::LessThan)
                    }
                }
                '>' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::GreaterThanOrEqual)
                    } else if let Some('>') = self.input.peek() {
                        self.input.next();
                        if let Some('=') = self.input.peek() {
                            self.input.next();
                            Some(Token::Assignment(Assignment::ShiftRightEqual))
                        } else {
                            Some(Token::ShiftRight)
                        }
                    } else {
                        Some(Token::GreaterThan)
                    }
                }
                '%' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Assignment(Assignment::ModuloEqual))
                    } else {
                        Some(Token::Modulo)
                    }
                }
                '^' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Assignment(Assignment::XorEqual))
                    } else {
                        Some(Token::BitwiseXor)
                    }
                }
                '0'..='9' => parse_number(ch, &mut self.input, None),
                ':' => Some(Token::Colon),
                '?' => Some(Token::QuestionMark),
                ',' => Some(Token::Comma),
                '\'' => parse_character(&mut self.input),
                '.' => {
                    let peek = self.input.peek();
                    if peek.is_none() {
                        return Some(Token::Dot);
                    }

                    let peek = *peek.unwrap();

                    if peek.is_numeric() {
                        parse_number(ch, &mut self.input, None)
                    } else if peek == '.' {
                        self.input.next();
                        if let Some('.') = self.input.peek() {
                            self.input.next();
                            Some(Token::TripleDot)
                        } else {
                            None
                        }
                    } else {
                        Some(Token::Dot)
                    }
                }
                '"' => parse_string(&mut self.input),
                '[' => Some(Token::OpenSquareBracket),
                ']' => Some(Token::CloseSquareBracket),
                '#' => Some(Token::Hash),
                _ => {
                    if ch.is_whitespace() {
                        continue;
                    }

                    if ch == 'u' {
                        if let Some(ch) = self.input.peek() {
                            if *ch == '8' {
                                self.input.next();
                                if let Some('"') = self.input.peek() {
                                    self.input.next();
                                    return parse_string(&mut self.input);
                                }
                            }
                        }
                    }

                    if ch.is_alphabetic() || ch == '_' {
                        let mut word = ch.to_string();
                        while let Some(ch) = self.input.peek() {
                            if ch.is_alphabetic() || ch.is_numeric() || *ch == '_' {
                                word.push(*ch);
                                self.input.next();
                            } else {
                                break;
                            }
                        }

                        if let Ok(keyword) = word.parse() {
                            Some(Token::Keyword(keyword))
                        } else {
                            Some(Token::Word(word))
                        }
                    } else {
                        None
                    }
                }
            };

            return if let Some(token) = next {
                Some(token)
            } else {
                self.end = true;
                None
            };
        }

        self.end = true;
        Some(Token::EndOfFile)
    }
}

fn parse_number<TInput>(
    start: char,
    input: &mut Peekable<TInput>,
    mut unsigned: Option<bool>,
) -> Option<Token>
where
    TInput: Iterator<Item = char>,
{
    let mut digits = vec![start];

    #[derive(PartialEq)]
    enum NumberType {
        Integer,
        Decimal,
        Binary,
        Hexadecimal,
        Octal,
    }

    let mut state = match start {
        '0' => match input.peek() {
            Some('b') | Some('B') => {
                digits.push(input.next().unwrap());
                NumberType::Binary
            }
            Some('x') | Some('X') => {
                digits.push(input.next().unwrap());
                NumberType::Hexadecimal
            }
            Some(_) => NumberType::Octal,
            None => NumberType::Integer,
        },
        '.' => NumberType::Decimal,
        _ => NumberType::Integer,
    };

    while let Some(ch) = input.peek() {
        match ch {
            '0' | '1' => {
                digits.push(*ch);
                input.next();
            }
            '0'..='7' => match state {
                NumberType::Integer
                | NumberType::Decimal
                | NumberType::Hexadecimal
                | NumberType::Octal => {
                    digits.push(*ch);
                    input.next();
                }
                NumberType::Binary => break,
            },
            '8'..='9' => match state {
                NumberType::Integer | NumberType::Decimal | NumberType::Hexadecimal => {
                    digits.push(*ch);
                    input.next();
                }
                NumberType::Octal | NumberType::Binary => break,
            },
            '.' => match state {
                NumberType::Integer | NumberType::Hexadecimal => {
                    state = NumberType::Decimal;
                    digits.push(*ch);
                    input.next();
                }
                NumberType::Decimal | NumberType::Octal | NumberType::Binary => break,
            },
            'a'..='f' | 'A'..='F' => match state {
                NumberType::Hexadecimal => {
                    digits.push(*ch);
                    input.next();
                }
                NumberType::Integer
                | NumberType::Decimal
                | NumberType::Octal
                | NumberType::Binary => break,
            },
            _ => break,
        }
    }

    match state {
        NumberType::Integer | NumberType::Binary | NumberType::Hexadecimal | NumberType::Octal => {
            // skip 'l' and 'u'
            while let Some(ch) = input.peek() {
                if *ch == 'u' || *ch == 'U' {
                    unsigned = Some(true);
                    input.next();
                } else if *ch == 'l' || *ch == 'L' {
                    input.next();
                } else {
                    break;
                }
            }
        }
        NumberType::Decimal => {
            // take 'f'
            while let Some(ch) = input.peek() {
                if *ch == 'f' || *ch == 'F' || *ch == 'l' || *ch == 'L' {
                    input.next();
                } else {
                    break;
                }
            }
        }
    }

    // yeah, that is a bit stupid to collect and parse the number
    let number = digits.iter().collect::<String>();
    let parse_integer = |value| match unsigned {
        Some(true) => Some(Token::Constant(Constant::UnsignedInteger(value as u64))),
        _ => Some(Token::Constant(Constant::SignedInteger(value as i64))),
    };

    match state {
        NumberType::Integer => number.parse::<i128>().ok().and_then(parse_integer),
        NumberType::Decimal => number
            .parse::<f64>()
            .ok()
            .map(|value| Some(Token::Constant(Constant::Decimal(value))))?,
        NumberType::Binary => i128::from_str_radix(&number[2..], 2)
            .ok()
            .and_then(parse_integer),
        NumberType::Hexadecimal => i128::from_str_radix(&number[2..], 16)
            .ok()
            .and_then(parse_integer),
        NumberType::Octal => i128::from_str_radix(&number[..], 8)
            .ok()
            .and_then(parse_integer),
    }
}

fn parse_character<TInput>(input: &mut Peekable<TInput>) -> Option<Token>
where
    TInput: Iterator<Item = char>,
{
    let character = input.next()?;

    let character = if character != '\\' {
        character
    } else {
        let ch = input.next()?;
        match ch {
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0C',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0B',
            '\'' => '\'',
            '"' => '"',
            '\\' => '\\',
            '?' => '?',
            '0'..='7' => parse_octal_character(ch, input)?,
            'x' => parse_hexadecimal_character(input)?,
            other => other,
        }
    };

    if let Some('\'') = input.next() {
        Some(Token::Constant(Constant::Character(character)))
    } else {
        None
    }
}

fn parse_octal_character<TInput>(start: char, input: &mut Peekable<TInput>) -> Option<char>
where
    TInput: Iterator<Item = char>,
{
    let mut digits = vec![start];
    while let Some(ch) = input.peek() {
        match ch {
            '0'..='7' => {
                digits.push(*ch);
                input.next();
            }
            _ => break,
        }
    }

    let number = digits.iter().collect::<String>();
    if let Ok(value) = u8::from_str_radix(&number, 8) {
        Some(value as char)
    } else {
        None
    }
}

fn parse_hexadecimal_character<TInput>(input: &mut Peekable<TInput>) -> Option<char>
where
    TInput: Iterator<Item = char>,
{
    let mut digits = vec![];
    while let Some(ch) = input.peek() {
        match ch {
            '0'..='9' | 'a'..='f' | 'A'..='F' => {
                digits.push(*ch);
                input.next();
            }
            _ => break,
        }
    }

    let number = digits.iter().collect::<String>();
    if let Ok(value) = u8::from_str_radix(&number, 16) {
        Some(value as char)
    } else {
        None
    }
}

fn parse_string<TInput>(input: &mut Peekable<TInput>) -> Option<Token>
where
    TInput: Iterator<Item = char>,
{
    let mut string = vec![];
    while let Some(ch) = input.next() {
        if ch == '"' {
            break;
        }

        if ch == '\\' {
            let ch = input.next()?;
            match ch {
                'a' => string.push('\x07'),
                'b' => string.push('\x08'),
                'f' => string.push('\x0C'),
                'n' => string.push('\n'),
                'r' => string.push('\r'),
                't' => string.push('\t'),
                'v' => string.push('\x0B'),
                '\'' => string.push('\''),
                '"' => string.push('"'),
                '\\' => string.push('\\'),
                '?' => string.push('?'),
                '0'..='7' => {
                    let number = parse_octal_character(ch, input)?;
                    string.push(number);
                }
                'x' => {
                    let number = parse_hexadecimal_character(input)?;
                    string.push(number);
                }
                _ => panic!(),
            }
        } else {
            string.push(ch);
        }
    }

    Some(Token::StringLiteral(StringLiteral::ByteString(string)))
}
#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::open_parenthesis("(", vec![Token::OpenParenthesis, Token::EndOfFile])]
    #[case::close_parenthesis(")", vec![Token::CloseParenthesis, Token::EndOfFile])]
    #[case::open_brace("{", vec![Token::OpenBrace, Token::EndOfFile])]
    #[case::close_brace("}", vec![Token::CloseBrace, Token::EndOfFile])]
    #[case::semi_colon(";", vec![Token::SemiColon, Token::EndOfFile])]
    #[case::word("word", vec![Token::Word("word".to_string()), Token::EndOfFile])]
    #[case::negation("-", vec![Token::Negation, Token::EndOfFile])]
    #[case::logical_not("!", vec![Token::LogicalNot, Token::EndOfFile])]
    #[case::bitwise_not("~", vec![Token::BitwiseNot, Token::EndOfFile])]
    #[case::addition("+", vec![Token::Addition, Token::EndOfFile])]
    #[case::multiplication("*", vec![Token::Star, Token::EndOfFile])]
    #[case::division("/", vec![Token::Division, Token::EndOfFile])]
    #[case::and("&&", vec![Token::LogicalAnd, Token::EndOfFile])]
    #[case::or("||", vec![Token::LogicalOr, Token::EndOfFile])]
    #[case::equal("==", vec![Token::Equal, Token::EndOfFile])]
    #[case::not_equal("!=", vec![Token::NotEqual, Token::EndOfFile])]
    #[case::less_than("<", vec![Token::LessThan, Token::EndOfFile])]
    #[case::less_than_or_equal("<=", vec![Token::LessThanOrEqual, Token::EndOfFile])]
    #[case::greater_than(">", vec![Token::GreaterThan, Token::EndOfFile])]
    #[case::greater_than_or_equal(">=", vec![Token::GreaterThanOrEqual, Token::EndOfFile])]
    #[case::assignment("=", vec![Token::Assignment(Assignment::Equal), Token::EndOfFile])]
    #[case::plus_equal("+=", vec![Token::Assignment(Assignment::PlusEqual), Token::EndOfFile])]
    #[case::minus_equal("-=", vec![Token::Assignment(Assignment::MinusEqual), Token::EndOfFile])]
    #[case::multiply_equal("*=", vec![Token::Assignment(Assignment::MultiplyEqual), Token::EndOfFile])]
    #[case::divide_equal("/=", vec![Token::Assignment(Assignment::DivideEqual), Token::EndOfFile])]
    #[case::and_equal("&=", vec![Token::Assignment(Assignment::AndEqual), Token::EndOfFile])]
    #[case::or_equal("|=", vec![Token::Assignment(Assignment::OrEqual), Token::EndOfFile])]
    #[case::shift_left_equal("<<=", vec![Token::Assignment(Assignment::ShiftLeftEqual), Token::EndOfFile])]
    #[case::shift_right_equal(">>=", vec![Token::Assignment(Assignment::ShiftRightEqual), Token::EndOfFile])]
    #[case::modulo_equal("%=", vec![Token::Assignment(Assignment::ModuloEqual), Token::EndOfFile])]
    #[case::xor_equal("^=", vec![Token::Assignment(Assignment::XorEqual), Token::EndOfFile])]
    #[case::shift_left("<<", vec![Token::ShiftLeft, Token::EndOfFile])]
    #[case::shift_right(">>", vec![Token::ShiftRight, Token::EndOfFile])]
    #[case::modulo("%", vec![Token::Modulo, Token::EndOfFile])]
    #[case::xor("^", vec![Token::BitwiseXor, Token::EndOfFile])]
    #[case::or("|", vec![Token::BitwiseOr, Token::EndOfFile])]
    #[case::and("&", vec![Token::Ampersand, Token::EndOfFile])]
    #[case::increment("++", vec![Token::Increment, Token::EndOfFile])]
    #[case::decrement("--", vec![Token::Decrement, Token::EndOfFile])]
    #[case::colon(":", vec![Token::Colon, Token::EndOfFile])]
    #[case::question_mark("?", vec![Token::QuestionMark, Token::EndOfFile])]
    #[case::coma(",", vec![Token::Comma, Token::EndOfFile])]
    fn test_single_tokens(#[case] input: &str, #[case] expected: Vec<Token>) {
        let lexer = Lexer::new(input.chars().peekable());
        let tokens = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(tokens, expected);
    }

    #[rstest]
    #[case::auto_keyword("auto", vec![Token::Keyword(Keyword::Auto), Token::EndOfFile])]
    #[case::break_keyword("break", vec![Token::Keyword(Keyword::Break), Token::EndOfFile])]
    #[case::case_keyword("case", vec![Token::Keyword(Keyword::Case), Token::EndOfFile])]
    #[case::char_keyword("char", vec![Token::Keyword(Keyword::Char), Token::EndOfFile])]
    #[case::const_keyword("const", vec![Token::Keyword(Keyword::Const), Token::EndOfFile])]
    #[case::continue_keyword("continue", vec![Token::Keyword(Keyword::Continue), Token::EndOfFile])]
    #[case::default_keyword("default", vec![Token::Keyword(Keyword::Default), Token::EndOfFile])]
    #[case::do_keyword("do", vec![Token::Keyword(Keyword::Do), Token::EndOfFile])]
    #[case::double_keyword("double", vec![Token::Keyword(Keyword::Double), Token::EndOfFile])]
    #[case::else_keyword("else", vec![Token::Keyword(Keyword::Else), Token::EndOfFile])]
    #[case::enum_keyword("enum", vec![Token::Keyword(Keyword::Enum), Token::EndOfFile])]
    #[case::extern_keyword("extern", vec![Token::Keyword(Keyword::Extern), Token::EndOfFile])]
    #[case::float_keyword("float", vec![Token::Keyword(Keyword::Float), Token::EndOfFile])]
    #[case::for_keyword("for", vec![Token::Keyword(Keyword::For), Token::EndOfFile])]
    #[case::goto_keyword("goto", vec![Token::Keyword(Keyword::Goto), Token::EndOfFile])]
    #[case::if_keyword("if", vec![Token::Keyword(Keyword::If), Token::EndOfFile])]
    #[case::inline_keyword("inline", vec![Token::Keyword(Keyword::Inline), Token::EndOfFile])]
    #[case::int_keyword("int", vec![Token::Keyword(Keyword::Int), Token::EndOfFile])]
    #[case::long_keyword("long", vec![Token::Keyword(Keyword::Long), Token::EndOfFile])]
    #[case::register_keyword("register", vec![Token::Keyword(Keyword::Register), Token::EndOfFile])]
    #[case::restrict_keyword("restrict", vec![Token::Keyword(Keyword::Restrict), Token::EndOfFile])]
    #[case::return_keyword("return", vec![Token::Keyword(Keyword::Return), Token::EndOfFile])]
    #[case::short_keyword("short", vec![Token::Keyword(Keyword::Short), Token::EndOfFile])]
    #[case::signed_keyword("signed", vec![Token::Keyword(Keyword::Signed), Token::EndOfFile])]
    #[case::sizeof_keyword("sizeof", vec![Token::Keyword(Keyword::Sizeof), Token::EndOfFile])]
    #[case::static_keyword("static", vec![Token::Keyword(Keyword::Static), Token::EndOfFile])]
    #[case::struct_keyword("struct", vec![Token::Keyword(Keyword::Struct), Token::EndOfFile])]
    #[case::switch_keyword("switch", vec![Token::Keyword(Keyword::Switch), Token::EndOfFile])]
    #[case::typedef_keyword("typedef", vec![Token::Keyword(Keyword::Typedef), Token::EndOfFile])]
    #[case::union_keyword("union", vec![Token::Keyword(Keyword::Union), Token::EndOfFile])]
    #[case::unsigned_keyword("unsigned", vec![Token::Keyword(Keyword::Unsigned), Token::EndOfFile])]
    #[case::void_keyword("void", vec![Token::Keyword(Keyword::Void), Token::EndOfFile])]
    #[case::volatile_keyword("volatile", vec![Token::Keyword(Keyword::Volatile), Token::EndOfFile])]
    #[case::while_keyword("while", vec![Token::Keyword(Keyword::While), Token::EndOfFile])]
    #[case::alignof_keyword("_Alignof", vec![Token::Keyword(Keyword::_Alignof), Token::EndOfFile])]
    #[case::atomic_keyword("_Atomic", vec![Token::Keyword(Keyword::_Atomic), Token::EndOfFile])]
    #[case::bool_keyword("_Bool", vec![Token::Keyword(Keyword::_Bool), Token::EndOfFile])]
    #[case::complex_keyword("_Complex", vec![Token::Keyword(Keyword::_Complex), Token::EndOfFile])]
    #[case::generic_keyword("_Generic", vec![Token::Keyword(Keyword::_Generic), Token::EndOfFile])]
    #[case::imaginary_keyword("_Imaginary", vec![Token::Keyword(Keyword::_Imaginary), Token::EndOfFile])]
    #[case::noreturn_keyword("_Noreturn", vec![Token::Keyword(Keyword::_Noreturn), Token::EndOfFile])]
    #[case::static_assert_keyword("_StaticAssert", vec![Token::Keyword(Keyword::_StaticAssert), Token::EndOfFile])]
    #[case::thread_local_keyword("_ThreadLocal", vec![Token::Keyword(Keyword::_ThreadLocal), Token::EndOfFile])]
    #[case::open_square_bracket("[", vec![Token::OpenSquareBracket, Token::EndOfFile])]
    #[case::close_square_bracket("]", vec![Token::CloseSquareBracket, Token::EndOfFile])]
    #[case::dot(".", vec![Token::Dot, Token::EndOfFile])]
    #[case::triangle_dot("...", vec![Token::TripleDot, Token::EndOfFile])]
    #[case::arrow("->", vec![Token::Arrow, Token::EndOfFile])]
    #[case::ampersand("&", vec![Token::Ampersand, Token::EndOfFile])]
    #[case::star("*", vec![Token::Star, Token::EndOfFile])]
    #[case::hash("#", vec![Token::Hash, Token::EndOfFile])]
    fn test_keywords(#[case] input: &str, #[case] expected: Vec<Token>) {
        let lexer = Lexer::new(input.chars().peekable());
        let tokens = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(tokens, expected);
    }

    #[rstest]
    #[case::word("identifier", vec![Token::Word("identifier".to_string()), Token::EndOfFile])]
    #[case::underscore("identifier_", vec![Token::Word("identifier_".to_string()), Token::EndOfFile])]
    #[case::number("identifier123", vec![Token::Word("identifier123".to_string()), Token::EndOfFile])]
    #[case::mixed("identifier_123", vec![Token::Word("identifier_123".to_string()), Token::EndOfFile])]
    fn test_identifier(#[case] input: &str, #[case] expected: Vec<Token>) {
        let lexer = Lexer::new(input.chars().peekable());
        let tokens = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(tokens, expected);
    }

    #[rstest]
    #[case::integer("1234", vec![Token::Constant(Constant::SignedInteger(1234)), Token::EndOfFile])]
    #[case::hexadecimal("0xFF", vec![Token::Constant(Constant::SignedInteger(0xFF)), Token::EndOfFile])]
    #[case::hexadecimal_uppercase("0XFF", vec![Token::Constant(Constant::SignedInteger(0xFF)), Token::EndOfFile])]
    #[case::decimal("1234.5678", vec![Token::Constant(Constant::Decimal(1234.5678)), Token::EndOfFile])]
    #[case::decimal_no_integer(".5678", vec![Token::Constant(Constant::Decimal(0.5678)), Token::EndOfFile])]
    #[case::decimal_no_decimal("1234.", vec![Token::Constant(Constant::Decimal(1234.)), Token::EndOfFile])]
    #[case::character("'a'", vec![Token::Constant(Constant::Character('a')), Token::EndOfFile])]
    #[case::octal("0123", vec![Token::Constant(Constant::SignedInteger(0o123)), Token::EndOfFile])]
    #[case::binary("0b1010", vec![Token::Constant(Constant::SignedInteger(0b1010)), Token::EndOfFile])]
    #[case::binary_uppercase("0B1010", vec![Token::Constant(Constant::SignedInteger(0b1010)), Token::EndOfFile])]
    #[case::unsigned("1234u", vec![Token::Constant(Constant::UnsignedInteger(1234)), Token::EndOfFile])]
    #[case::unsigned_uppercase("1234U", vec![Token::Constant(Constant::UnsignedInteger(1234)), Token::EndOfFile])]
    #[case::long("1234l", vec![Token::Constant(Constant::SignedInteger(1234)), Token::EndOfFile])]
    #[case::long_uppercase("1234L", vec![Token::Constant(Constant::SignedInteger(1234)), Token::EndOfFile])]
    #[case::unsigned_long("1234ul", vec![Token::Constant(Constant::UnsignedInteger(1234)), Token::EndOfFile])]
    #[case::unsigned_long_uppercase("1234UL", vec![Token::Constant(Constant::UnsignedInteger(1234)), Token::EndOfFile])]
    #[case::long_long("1234ll", vec![Token::Constant(Constant::SignedInteger(1234)), Token::EndOfFile])]
    #[case::long_long_uppercase("1234LL", vec![Token::Constant(Constant::SignedInteger(1234)), Token::EndOfFile])]
    #[case::unsigned_long_long("1234ull", vec![Token::Constant(Constant::UnsignedInteger(1234)), Token::EndOfFile])]
    #[case::unsigned_long_long_uppercase("1234ULL", vec![Token::Constant(Constant::UnsignedInteger(1234)), Token::EndOfFile])]
    #[case::positive_prefix("+1234", vec![Token::Constant(Constant::SignedInteger(1234)), Token::EndOfFile])]
    #[case::negative_prefix("-1234", vec![Token::Constant(Constant::SignedInteger(-1234)), Token::EndOfFile])]
    #[case::decimal_suffix_float("1234.5678f", vec![Token::Constant(Constant::Decimal(1234.5678)), Token::EndOfFile])]
    #[case::decimal_suffix_long("1234.5678l", vec![Token::Constant(Constant::Decimal(1234.5678)), Token::EndOfFile])]
    #[case::char_escape_sequence_hex("'\\x41'", vec![Token::Constant(Constant::Character('A')), Token::EndOfFile])]
    #[case::char_escape_sequence_octal("'\\101'", vec![Token::Constant(Constant::Character('A')), Token::EndOfFile])]
    fn test_constants(#[case] input: &str, #[case] expected: Vec<Token>) {
        let lexer = Lexer::new(input.chars().peekable());
        let tokens = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(tokens, expected);
    }

    #[rstest]
    #[case::simple_escape_sequence_newline("'\\n'", '\n')]
    #[case::simple_escape_sequence_carriage_return("'\\r'", '\r')]
    #[case::simple_escape_sequence_tab("'\\t'", '\t')]
    #[case::simple_escape_sequence_backspace("'\\b'", '\x08')]
    #[case::simple_escape_sequence_form_feed("'\\f'", '\x0C')]
    #[case::simple_escape_sequence_vertical_tab("'\\v'", '\x0B')]
    #[case::simple_escape_sequence_alert("'\\a'", '\x07')]
    #[case::simple_escape_sequence_backslash("'\\\\'", '\\')]
    #[case::simple_escape_sequence_single_quote("'\\''", '\'')]
    #[case::simple_escape_sequence_double_quote("'\\\"'", '"')]
    #[case::simple_escape_sequence_question_mark("'\\?'", '?')]
    fn test_simple_escape_sequence(#[case] input: &str, #[case] expected: char) {
        let lexer = Lexer::new(input.chars().peekable());
        let tokens = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Constant(Constant::Character(expected)),
                Token::EndOfFile
            ]
        );
    }

    #[rstest]
    #[case::string("\"hello\"", vec![Token::StringLiteral(StringLiteral::ByteString("hello".chars().collect())), Token::EndOfFile])]
    #[case::string_escape_sequence("\"\\n\"", vec![Token::StringLiteral(StringLiteral::ByteString("\n".chars().collect())), Token::EndOfFile])]
    #[case::string_escape_sequence_hex("\"\\x41\"", vec![Token::StringLiteral(StringLiteral::ByteString("A".chars().collect())), Token::EndOfFile])]
    #[case::string_escape_sequence_octal("\"\\101\"", vec![Token::StringLiteral(StringLiteral::ByteString("A".chars().collect())), Token::EndOfFile])]
    #[case::u8_prefix("u8\"hello\"", vec![Token::StringLiteral(StringLiteral::ByteString("hello".chars().collect())), Token::EndOfFile])]
    fn test_string_literal(#[case] input: &str, #[case] expected: Vec<Token>) {
        let lexer = Lexer::new(input.chars().peekable());
        let tokens = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(tokens, expected);
    }
}
