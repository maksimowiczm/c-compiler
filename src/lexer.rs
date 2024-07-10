use std::iter::Peekable;

pub struct Lexer<TInput: Iterator<Item = char>> {
    input: Peekable<TInput>,
    end: bool,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(test, derive(Clone))]
pub enum Token {
    EndOfFile,
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    SemiColon,
    Word(String),
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
                '-' => Some(Token::Negation),
                '+' => Some(Token::Addition),
                '*' => Some(Token::Multiplication),
                '/' => Some(Token::Division),
                '&' => {
                    if let Some('&') = self.input.peek() {
                        self.input.next();
                        Some(Token::LogicalAnd)
                    } else {
                        None
                    }
                }
                '|' => {
                    if let Some('|') = self.input.peek() {
                        self.input.next();
                        Some(Token::LogicalOr)
                    } else {
                        None
                    }
                }
                '=' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::Equal)
                    } else {
                        None
                    }
                }
                '<' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::LessThanOrEqual)
                    } else {
                        Some(Token::LessThan)
                    }
                }
                '>' => {
                    if let Some('=') = self.input.peek() {
                        self.input.next();
                        Some(Token::GreaterThanOrEqual)
                    } else {
                        Some(Token::GreaterThan)
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
                        Some(Token::Word(word))
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
    #[case::negation("--3", vec![
        Token::Negation,
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
}
