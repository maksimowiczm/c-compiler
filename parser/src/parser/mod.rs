pub mod declaration;
pub mod declaration_specifiers;
mod declarator;
mod expression;
mod init_declarator;
mod initializer;
mod pointer;
pub mod storage_class_specifier;
mod type_qualifier;
mod type_specifier;

use crate::lexer::Lexer;

pub trait Parse
where
    Self: Sized,
{
    fn parse(lexer: &mut Lexer) -> Option<Self>;
}
