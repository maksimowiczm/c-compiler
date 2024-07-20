use crate::lexer::Lexer;
use crate::parser::declaration_specifiers::DeclarationSpecifiers;
use crate::parser::init_declarator::InitDeclaratorList;
use crate::parser::Parse;
use crate::tokenizer::TokenKind;

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Declaration {
    declaration_specifiers: DeclarationSpecifiers,
    init_declarator_list: Option<InitDeclaratorList>,
}

impl Parse for Declaration {
    fn parse(lexer: &mut Lexer) -> Option<Declaration> {
        let declaration_specifiers = DeclarationSpecifiers::parse(lexer)?;
        let init_declarator_list = InitDeclaratorList::parse(lexer);

        let token = lexer.borrow()?;
        if !matches!(token.kind, TokenKind::Semicolon) {
            panic!("expected ';'");
        }

        Some(Declaration {
            declaration_specifiers,
            init_declarator_list,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::declarator::{Declarator, DirectDeclarator};
    use crate::parser::init_declarator::InitDeclarator;
    use crate::parser::type_specifier::TypeSpecifier;
    use crate::tokenizer::Token;

    #[test]
    fn test_declaration() {
        let mut lexer = Lexer::new(vec![
            Token {
                kind: TokenKind::Int,
                value: "int".to_string(),
            },
            Token {
                kind: TokenKind::Identifier,
                value: "a".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                value: ";".to_string(),
            },
        ]);
        let declaration = Declaration::parse(&mut lexer);
        assert_eq!(
            declaration,
            Some(Declaration {
                declaration_specifiers: DeclarationSpecifiers::TypeSpecifier {
                    type_specifier: TypeSpecifier::Int,
                    declaration_specifiers: None,
                },
                init_declarator_list: Some(InitDeclaratorList {
                    init_declarator_list: None,
                    init_declarator: InitDeclarator {
                        declarator: Declarator {
                            pointer: None,
                            direct_declarator: DirectDeclarator::Identifier("a".to_string())
                        },
                        initializer: None,
                    }
                }),
            })
        );
    }
}
