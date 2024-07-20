use crate::lexer::Lexer;
use crate::parser::Parse;
use crate::tokenizer::TokenKind;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum StorageClassSpecifier {
    Typedef,
    Extern,
    Static,
    Auto,
    Register,
}

impl Parse for StorageClassSpecifier {
    fn parse(lexer: &mut Lexer) -> Option<StorageClassSpecifier> {
        let token = &lexer.borrow()?.kind;

        let out = match token {
            TokenKind::Typedef => StorageClassSpecifier::Typedef,
            TokenKind::Extern => StorageClassSpecifier::Extern,
            TokenKind::Static => StorageClassSpecifier::Static,
            TokenKind::Auto => StorageClassSpecifier::Auto,
            TokenKind::Register => StorageClassSpecifier::Register,
            _ => {
                lexer.release(1);
                return None;
            }
        };

        lexer.consume();

        Some(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::tokenizer::{Token, TokenKind};

    macro_rules! test_storage_class_parse {
        ($name:ident, $source:expr, $kind:expr) => {
            #[test]
            fn $name() {
                let mut lexer = Lexer::new($source);
                assert_eq!(StorageClassSpecifier::parse(&mut lexer), Some($kind));
                assert_eq!(lexer.borrow(), None);
            }
        };
    }

    test_storage_class_parse!(
        parse_typedef,
        vec![Token {
            kind: TokenKind::Typedef,
            value: "typedef".to_string(),
        }],
        StorageClassSpecifier::Typedef
    );
    test_storage_class_parse!(
        parse_extern,
        vec![Token {
            kind: TokenKind::Extern,
            value: "extern".to_string(),
        }],
        StorageClassSpecifier::Extern
    );
    test_storage_class_parse!(
        parse_static,
        vec![Token {
            kind: TokenKind::Static,
            value: "static".to_string(),
        }],
        StorageClassSpecifier::Static
    );
    test_storage_class_parse!(
        parse_auto,
        vec![Token {
            kind: TokenKind::Auto,
            value: "auto".to_string(),
        }],
        StorageClassSpecifier::Auto
    );
    test_storage_class_parse!(
        parse_register,
        vec![Token {
            kind: TokenKind::Register,
            value: "register".to_string(),
        }],
        StorageClassSpecifier::Register
    );
}
