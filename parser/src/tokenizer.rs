use std::iter::Peekable;

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum TokenKind {
    Identifier,
    // keywords
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
    Int,
    Long,
    Register,
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
    //
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    // operators
    Semicolon,
    Asterisk,
    Ampersand,
}

pub struct Tokenizer<I: Iterator<Item = char>> {
    source: Peekable<I>,
    end: bool,
}

impl<I> Iterator for Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            return None;
        }

        let mut value = String::new();

        while let Some(ch) = self.source.next() {
            value.push(ch);
            let kind = match ch {
                '{' => TokenKind::OpenBrace,
                '}' => TokenKind::CloseBrace,
                '(' => TokenKind::OpenParen,
                ')' => TokenKind::CloseParen,
                ';' => TokenKind::Semicolon,
                '*' => TokenKind::Asterisk,
                '&' => TokenKind::Ampersand,
                _ if ch.is_alphabetic() || ch == '_' => {
                    while let Some(&ch) = self.source.peek() {
                        if ch.is_alphanumeric() || ch == '_' {
                            value.push(self.source.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    match value.as_str() {
                        "auto" => TokenKind::Auto,
                        "break" => TokenKind::Break,
                        "case" => TokenKind::Case,
                        "char" => TokenKind::Char,
                        "const" => TokenKind::Const,
                        "continue" => TokenKind::Continue,
                        "default" => TokenKind::Default,
                        "do" => TokenKind::Do,
                        "double" => TokenKind::Double,
                        "else" => TokenKind::Else,
                        "enum" => TokenKind::Enum,
                        "extern" => TokenKind::Extern,
                        "float" => TokenKind::Float,
                        "for" => TokenKind::For,
                        "goto" => TokenKind::Goto,
                        "if" => TokenKind::If,
                        "int" => TokenKind::Int,
                        "long" => TokenKind::Long,
                        "register" => TokenKind::Register,
                        "return" => TokenKind::Return,
                        "short" => TokenKind::Short,
                        "signed" => TokenKind::Signed,
                        "sizeof" => TokenKind::Sizeof,
                        "static" => TokenKind::Static,
                        "struct" => TokenKind::Struct,
                        "switch" => TokenKind::Switch,
                        "typedef" => TokenKind::Typedef,
                        "union" => TokenKind::Union,
                        "unsigned" => TokenKind::Unsigned,
                        "void" => TokenKind::Void,
                        "volatile" => TokenKind::Volatile,
                        "while" => TokenKind::While,
                        _ => TokenKind::Identifier,
                    }
                }
                _ if ch.is_whitespace() => continue,
                _ => {
                    self.end = true;
                    return None;
                }
            };

            return Some(Token { kind, value });
        }

        self.end = true;
        None
    }
}

#[allow(dead_code)]
impl<I> Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(source: I) -> Self {
        let source = source.peekable();
        Self { source, end: false }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test {
        ($name:ident, $source:expr, $kind:expr) => {
            #[test]
            fn $name() {
                let mut tokenizer = Tokenizer::new($source.chars().peekable());
                assert_eq!(
                    tokenizer.next().unwrap(),
                    Token {
                        kind: $kind,
                        value: $source.to_string(),
                    }
                );
                assert!(tokenizer.next().is_none());
            }
        };
    }

    #[test]
    fn test_whitespace() {
        let mut tokenizer = Tokenizer::new(" \t\n".chars().peekable());
        assert!(tokenizer.next().is_none());
    }

    test!(test_identifier, "foo", TokenKind::Identifier);
    test!(test_auto, "auto", TokenKind::Auto);
    test!(test_break, "break", TokenKind::Break);
    test!(test_case, "case", TokenKind::Case);
    test!(test_char, "char", TokenKind::Char);
    test!(test_const, "const", TokenKind::Const);
    test!(test_continue, "continue", TokenKind::Continue);
    test!(test_default, "default", TokenKind::Default);
    test!(test_do, "do", TokenKind::Do);
    test!(test_double, "double", TokenKind::Double);
    test!(test_else, "else", TokenKind::Else);
    test!(test_enum, "enum", TokenKind::Enum);
    test!(test_extern, "extern", TokenKind::Extern);
    test!(test_float, "float", TokenKind::Float);
    test!(test_for, "for", TokenKind::For);
    test!(test_goto, "goto", TokenKind::Goto);
    test!(test_if, "if", TokenKind::If);
    test!(test_int, "int", TokenKind::Int);
    test!(test_long, "long", TokenKind::Long);
    test!(test_register, "register", TokenKind::Register);
    test!(test_return, "return", TokenKind::Return);
    test!(test_short, "short", TokenKind::Short);
    test!(test_signed, "signed", TokenKind::Signed);
    test!(test_sizeof, "sizeof", TokenKind::Sizeof);
    test!(test_static, "static", TokenKind::Static);
    test!(test_struct, "struct", TokenKind::Struct);
    test!(test_switch, "switch", TokenKind::Switch);
    test!(test_typedef, "typedef", TokenKind::Typedef);
    test!(test_union, "union", TokenKind::Union);
    test!(test_unsigned, "unsigned", TokenKind::Unsigned);
    test!(test_void, "void", TokenKind::Void);
    test!(test_volatile, "volatile", TokenKind::Volatile);
    test!(test_while, "while", TokenKind::While);
    test!(test_open_brace, "{", TokenKind::OpenBrace);
    test!(test_close_brace, "}", TokenKind::CloseBrace);
    test!(test_open_paren, "(", TokenKind::OpenParen);
    test!(test_close_paren, ")", TokenKind::CloseParen);
    test!(test_semicolon, ";", TokenKind::Semicolon);
    test!(test_asterisk, "*", TokenKind::Asterisk);
    test!(test_ampersand, "&", TokenKind::Ampersand);
}
