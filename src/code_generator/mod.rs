pub mod asm64;

use crate::parser::Program;
use io::Write;
use std::io;

pub trait CodeGenerator {
    type Err;

    fn generate(self, node: Program, buffer: &mut impl Write) -> Result<(), Self::Err>;
}
