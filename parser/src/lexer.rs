use derive_more::FromStr;
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
    Integer(i32),
    Negation,
    LogicalNot,
    BitwiseNot,
    Addition,
    Multiplication,
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
    BitwiseAnd,
    Increment,
    Decrement,
    Colon,
    QuestionMark,
    Comma,
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

#[derive(Debug, PartialEq, Clone, FromStr)]
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
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Assignment(Assignment::MinusEqual))
                    } else if let Some('-') = self.input.peek() {
                        self.input.next();
                        Some(Token::Decrement)
                    } else {
                        Some(Token::Negation)
                    }
                }
                '+' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Assignment(Assignment::PlusEqual))
                    } else if let Some('+') = self.input.peek() {
                        self.input.next();
                        Some(Token::Increment)
                    } else {
                        Some(Token::Addition)
                    }
                }
                '*' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Assignment(Assignment::MultiplyEqual))
                    } else {
                        Some(Token::Multiplication)
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
                        Some(Token::BitwiseAnd)
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
                '0'..='9' => {
                    let mut number = ch.to_digit(10).unwrap() as i32;
                    while let Some(ch) = self.input.peek() {
                        if let Some(digit) = ch.to_digit(10) {
                            number = number * 10 + digit as i32;
                        } else {
                            break;
                        }

                        self.input.next();
                    }
                    Some(Token::Integer(number))
                }
                ':' => Some(Token::Colon),
                '?' => Some(Token::QuestionMark),
                ',' => Some(Token::Comma),
                _ => {
                    if ch.is_whitespace() {
                        continue;
                    }

                    if ch.is_alphabetic() {
                        let mut word = ch.to_string();
                        while let Some(ch) = self.input.peek() {
                            if ch.is_alphabetic() || ch.is_numeric() {
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
    #[case::integer("1234", vec![Token::Integer(1234), Token::EndOfFile])]
    #[case::word("word", vec![Token::Word("word".to_string()), Token::EndOfFile])]
    #[case::negation("-", vec![Token::Negation, Token::EndOfFile])]
    #[case::logical_not("!", vec![Token::LogicalNot, Token::EndOfFile])]
    #[case::bitwise_not("~", vec![Token::BitwiseNot, Token::EndOfFile])]
    #[case::addition("+", vec![Token::Addition, Token::EndOfFile])]
    #[case::multiplication("*", vec![Token::Multiplication, Token::EndOfFile])]
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
    #[case::and("&", vec![Token::BitwiseAnd, Token::EndOfFile])]
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
    #[case::word_wrapped_in_parenthesis("{(1234)}", vec![
        Token::OpenBrace,
        Token::OpenParenthesis,
        Token::Integer(1234),
        Token::CloseParenthesis,
        Token::CloseBrace,
        Token::EndOfFile
    ])]
    fn test_combination(#[case] input: &str, #[case] expected: Vec<Token>) {
        let lexer = Lexer::new(input.chars().peekable());
        let tokens = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(tokens, expected);
    }

    #[rstest]
    #[case::logical_not("!!3", vec![
        Token::LogicalNot,
        Token::LogicalNot,
        Token::Integer(3),
        Token::EndOfFile
    ])]
    #[case::negation("-3", vec![
        Token::Negation,
        Token::Integer(3),
        Token::EndOfFile
    ])]
    #[case::bitwise_not("~~3", vec![
        Token::BitwiseNot,
        Token::BitwiseNot,
        Token::Integer(3),
        Token::EndOfFile
    ])]
    #[case::negation_bitwise_not_logical_not("-!~3", vec![
        Token::Negation,
        Token::LogicalNot,
        Token::BitwiseNot,
        Token::Integer(3),
        Token::EndOfFile
    ])]
    fn test_unary_operators(#[case] input: &str, #[case] expected: Vec<Token>) {
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
    fn test_keywords(#[case] input: &str, #[case] expected: Vec<Token>) {
        let lexer = Lexer::new(input.chars().peekable());
        let tokens = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(tokens, expected);
    }
}
