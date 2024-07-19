use crate::lexer::Token;
use crate::parser::declaration::declaration_specifier::DeclarationSpecifier;
use crate::parser::declaration::declarator::Declarator;
use crate::parser::{Result, TryParse};
use std::iter::Peekable;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ParameterTypeList;

impl TryParse for ParameterTypeList {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        todo!()
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ParameterDeclaration {
    WithDeclarator {
        declaration_specifier: Vec<DeclarationSpecifier>,
        declarator: Declarator,
    },
    Specifier {
        declaration_specifier: Vec<DeclarationSpecifier>,
    },
}

impl TryParse for ParameterDeclaration {
    fn try_parse(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<Self>>
    where
        Self: Sized,
    {
        let mut declaration_specifier = vec![];
        while let Some(specifier) = DeclarationSpecifier::try_parse(tokens)? {
            declaration_specifier.push(specifier);
        }

        if let Some(declarator) = Declarator::try_parse(tokens)? {
            return Ok(Some(ParameterDeclaration::WithDeclarator {
                declaration_specifier,
                declarator,
            }));
        }

        Ok(Some(ParameterDeclaration::Specifier {
            declaration_specifier,
        }))
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Keyword;
    use crate::parser::declaration::declarator::{AbstractDeclarator, Declarator, Pointer};
    use crate::parser::declaration::type_specifier::TypeSpecifier;
    use rstest::rstest;

    // #[rstest]
    // #[case::empty(vec![], todo!())]
    // fn test_parameter_type_list(#[case] input: Vec<Token>, #[case] expected: ParameterTypeList) {
    //     let mut input = input.into_iter().peekable();
    //     let result = ParameterTypeList::try_parse(&mut input).unwrap().unwrap();
    //     assert_eq!(result, expected);
    // }

    #[rstest]
    #[case::declaration_specifier(
        vec![
            Token::Keyword(Keyword::Int),
        ],
        ParameterDeclaration::Specifier {
            declaration_specifier: vec![DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Int)]
        }
    )]
    #[case::with_declarator(
        vec![
            Token::Keyword(Keyword::Int),
            Token::Word("a".to_string()),
        ],
        ParameterDeclaration::WithDeclarator {
            declaration_specifier: vec![DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Int)],
            declarator: Declarator::Identifier{
                identifier: "a".to_string(),
                pointer: None
            }
        }
    )]
    #[case::with_abstract_declarator(
        vec![
            Token::Keyword(Keyword::Int),
            Token::Star,
        ],
        ParameterDeclaration::WithDeclarator {
            declaration_specifier: vec![DeclarationSpecifier::TypeSpecifier(TypeSpecifier::Int)],
            declarator: Declarator::Abstract(AbstractDeclarator{
                pointer: Some(Pointer {
                    type_qualifiers: vec![],
                    inner_pointer: None
                })
            })
        }
    )]
    fn test_parameter_declaration(
        #[case] input: Vec<Token>,
        #[case] expected: ParameterDeclaration,
    ) {
        let mut input = input.into_iter().peekable();
        let result = ParameterDeclaration::try_parse(&mut input)
            .unwrap()
            .unwrap();
        assert_eq!(result, expected);
    }
}
