use crate::code_generator::CodeGenerator;
use std::io::Write;
use parser::parser::Program;

pub(crate) struct IR;

impl CodeGenerator for IR {
    type Err = Box<dyn std::error::Error>;

    fn generate(self, _node: Program, _buffer: &mut impl Write) -> Result<(), Self::Err> {
        todo!()
    }
}
