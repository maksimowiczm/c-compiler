use crate::lexer::Lexer;
use crate::parser::storage_class_specifier::StorageClassSpecifier;
use crate::parser::type_qualifier::TypeQualifier;
use crate::parser::type_specifier::TypeSpecifier;
use crate::parser::Parse;

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DeclarationSpecifiers {
    StorageClassSpecifier {
        storage_class_specifier: StorageClassSpecifier,
        declaration_specifiers: Option<Box<DeclarationSpecifiers>>,
    },
    TypeSpecifier {
        type_specifier: TypeSpecifier,
        declaration_specifiers: Option<Box<DeclarationSpecifiers>>,
    },
    TypeQualifier {
        type_qualifier: TypeQualifier,
        declaration_specifiers: Option<Box<DeclarationSpecifiers>>,
    },
}

impl Parse for DeclarationSpecifiers {
    fn parse(lexer: &mut Lexer) -> Option<DeclarationSpecifiers> {
        if let Some(storage_class_specifier) = StorageClassSpecifier::parse(lexer) {
            return Some(DeclarationSpecifiers::StorageClassSpecifier {
                storage_class_specifier,
                declaration_specifiers: DeclarationSpecifiers::parse(lexer).map(Box::new),
            });
        }

        if let Some(type_specifier) = TypeSpecifier::parse(lexer) {
            return Some(DeclarationSpecifiers::TypeSpecifier {
                type_specifier,
                declaration_specifiers: DeclarationSpecifiers::parse(lexer).map(Box::new),
            });
        }

        if let Some(type_qualifier) = TypeQualifier::parse(lexer) {
            return Some(DeclarationSpecifiers::TypeQualifier {
                type_qualifier,
                declaration_specifiers: DeclarationSpecifiers::parse(lexer).map(Box::new),
            });
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::{Token, TokenKind};

    macro_rules! test_declaration_specifiers {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let mut lexer = Lexer::new($input);
                let declaration_specifiers = DeclarationSpecifiers::parse(&mut lexer).unwrap();
                assert_eq!(declaration_specifiers, $expected);
            }
        };
    }

    test_declaration_specifiers!(
        parse_storage_class_specifier,
        vec![Token {
            kind: TokenKind::Typedef,
            value: "typedef".to_string(),
        }],
        DeclarationSpecifiers::StorageClassSpecifier {
            storage_class_specifier: StorageClassSpecifier::Typedef,
            declaration_specifiers: None,
        }
    );

    test_declaration_specifiers!(
        parse_type_specifier,
        vec![Token {
            kind: TokenKind::Int,
            value: "int".to_string(),
        }],
        DeclarationSpecifiers::TypeSpecifier {
            type_specifier: TypeSpecifier::Int,
            declaration_specifiers: None,
        }
    );

    test_declaration_specifiers!(
        parse_type_qualifier,
        vec![Token {
            kind: TokenKind::Const,
            value: "const".to_string(),
        }],
        DeclarationSpecifiers::TypeQualifier {
            type_qualifier: TypeQualifier::Const,
            declaration_specifiers: None,
        }
    );

    test_declaration_specifiers!(
        parse_storage_class_specifier_type_specifier_type_qualifier,
        vec![
            Token {
                kind: TokenKind::Typedef,
                value: "typedef".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                value: "int".to_string(),
            },
            Token {
                kind: TokenKind::Const,
                value: "const".to_string(),
            },
        ],
        DeclarationSpecifiers::StorageClassSpecifier {
            storage_class_specifier: StorageClassSpecifier::Typedef,
            declaration_specifiers: Some(Box::new(DeclarationSpecifiers::TypeSpecifier {
                type_specifier: TypeSpecifier::Int,
                declaration_specifiers: Some(Box::new(DeclarationSpecifiers::TypeQualifier {
                    type_qualifier: TypeQualifier::Const,
                    declaration_specifiers: None,
                })),
            })),
        }
    );
}
