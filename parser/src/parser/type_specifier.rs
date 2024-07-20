use crate::lexer::Lexer;
use crate::parser::Parse;
use crate::tokenizer::TokenKind;

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
    // todo struct, union, enum
    TypedefName(String),
}

impl Parse for TypeSpecifier {
    fn parse(lexer: &mut Lexer) -> Option<Self> {
        let token = lexer.borrow()?;

        let out = match token.kind {
            TokenKind::Void => TypeSpecifier::Void,
            TokenKind::Char => TypeSpecifier::Char,
            TokenKind::Short => TypeSpecifier::Short,
            TokenKind::Int => TypeSpecifier::Int,
            TokenKind::Long => TypeSpecifier::Long,
            TokenKind::Float => TypeSpecifier::Float,
            TokenKind::Double => TypeSpecifier::Double,
            TokenKind::Signed => TypeSpecifier::Signed,
            TokenKind::Unsigned => TypeSpecifier::Unsigned,
            TokenKind::Identifier => TypeSpecifier::TypedefName(token.value.clone()),
            _ => {
                lexer.release(1);
                return None;
            }
        };

        Some(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::tokenizer::Token;

    macro_rules! test_type_specifier_parse {
        ($name:ident, $source:expr, $kind:expr) => {
            #[test]
            fn $name() {
                let mut lexer = Lexer::new($source);
                assert_eq!(TypeSpecifier::parse(&mut lexer), Some($kind));
                assert_eq!(lexer.borrow(), None);
            }
        };
    }

    test_type_specifier_parse!(
        parse_int,
        vec![Token {
            kind: TokenKind::Int,
            value: "int".to_string(),
        }],
        TypeSpecifier::Int
    );
    test_type_specifier_parse!(
        parse_char,
        vec![Token {
            kind: TokenKind::Char,
            value: "char".to_string(),
        }],
        TypeSpecifier::Char
    );
    test_type_specifier_parse!(
        parse_short,
        vec![Token {
            kind: TokenKind::Short,
            value: "short".to_string(),
        }],
        TypeSpecifier::Short
    );
    test_type_specifier_parse!(
        parse_long,
        vec![Token {
            kind: TokenKind::Long,
            value: "long".to_string(),
        }],
        TypeSpecifier::Long
    );
    test_type_specifier_parse!(
        parse_float,
        vec![Token {
            kind: TokenKind::Float,
            value: "float".to_string(),
        }],
        TypeSpecifier::Float
    );
    test_type_specifier_parse!(
        parse_double,
        vec![Token {
            kind: TokenKind::Double,
            value: "double".to_string(),
        }],
        TypeSpecifier::Double
    );
    test_type_specifier_parse!(
        parse_signed,
        vec![Token {
            kind: TokenKind::Signed,
            value: "signed".to_string(),
        }],
        TypeSpecifier::Signed
    );
    test_type_specifier_parse!(
        parse_unsigned,
        vec![Token {
            kind: TokenKind::Unsigned,
            value: "unsigned".to_string(),
        }],
        TypeSpecifier::Unsigned
    );
    test_type_specifier_parse!(
        typedef_name,
        vec![Token {
            kind: TokenKind::Identifier,
            value: "foo".to_string(),
        }],
        TypeSpecifier::TypedefName("foo".to_string())
    );
}
