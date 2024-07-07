use std::iter::Peekable;

pub struct Lexer;

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

impl Lexer {
    pub fn tokenize(&self, mut input: Peekable<impl Iterator<Item = char>>) -> Vec<Token> {
        let mut tokens = vec![];

        while let Some(ch) = input.next() {
            match ch {
                '{' => tokens.push(Token::OpenBrace),
                '}' => tokens.push(Token::CloseBrace),
                '(' => tokens.push(Token::OpenParenthesis),
                ')' => tokens.push(Token::CloseParenthesis),
                ';' => tokens.push(Token::SemiColon),
                '0'..='9' => {
                    let mut number = ch.to_digit(10).unwrap() as i32;
                    while let Some(ch) = input.peek() {
                        if let Some(digit) = ch.to_digit(10) {
                            number = number * 10 + digit as i32;
                        } else {
                            break;
                        }

                        input.next();
                    }
                    tokens.push(Token::Integer(number));
                }
                _ => {
                    if ch.is_whitespace() {
                        continue;
                    }

                    if ch.is_alphabetic() {
                        let mut word = ch.to_string();
                        while let Some(ch) = input.peek() {
                            if ch.is_alphabetic() || ch.is_numeric() {
                                word.push(*ch);
                                input.next();
                            } else {
                                break;
                            }
                        }
                        tokens.push(Token::Word(word));
                    }
                }
            }
        }

        tokens.push(Token::EndOfFile);

        tokens
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
        let lexer = Lexer {};
        let tokens = lexer.tokenize(input.chars().peekable());

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
        let lexer = Lexer {};
        let tokens = lexer.tokenize(input.chars().peekable());

        assert_eq!(tokens, expected);
    }
}
