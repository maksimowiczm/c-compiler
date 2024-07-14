use crate::code_generator::CodeGenerator;
use crate::parser::Program;
use std::io::Write;

pub(crate) struct IR;

impl CodeGenerator for IR {
    type Err = Box<dyn std::error::Error>;

    fn generate(self, node: Program, buffer: &mut impl Write) -> Result<(), Self::Err> {
        todo!()
    }
}
