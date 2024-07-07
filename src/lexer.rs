use std::iter::Peekable;

pub struct Lexer<TInput: Iterator<Item = char>> {
    input: Peekable<TInput>,
    end: bool,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Token {
    EndOfFile,
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    SemiColon,
    Word(String),
    Integer(i32),
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
            return match ch {
                '{' => Some(Token::OpenBrace),
                '}' => Some(Token::CloseBrace),
                '(' => Some(Token::OpenParenthesis),
                ')' => Some(Token::CloseParenthesis),
                ';' => Some(Token::SemiColon),
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
}
