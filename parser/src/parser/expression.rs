use crate::lexer::Lexer;
use crate::parser::Parse;

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expression {
    AssignmentExpression(Box<AssignmentExpression>),
    SignedInteger(i64),
}

impl Parse for Expression {
    fn parse(_lexer: &mut Lexer) -> Option<Self> {
        Some(Expression::SignedInteger(999))
    }
}

#[allow(dead_code)]
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct AssignmentExpression {
    pub expression: Box<Expression>,
}

impl Parse for AssignmentExpression {
    fn parse(lexer: &mut Lexer) -> Option<Self> {
        Some(AssignmentExpression {
            expression: Expression::parse(lexer).map(Box::new)?,
        })
    }
}
