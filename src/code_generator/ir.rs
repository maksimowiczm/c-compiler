use crate::code_generator::CodeGenerator;
use parser::parser::Program;
use std::io::Write;

pub(crate) struct IR;

impl CodeGenerator for IR {
    type Err = Box<dyn std::error::Error>;

    fn generate(self, _node: Program, _buffer: &mut impl Write) -> Result<(), Self::Err> {
        todo!()
    }
}
