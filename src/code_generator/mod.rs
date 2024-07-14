pub mod asm64;
pub mod ir;

use crate::parser::Program;
use io::Write;
use std::io;

pub trait CodeGenerator {
    type Err;

    fn generate(self, node: Program, buffer: &mut impl Write) -> Result<(), Self::Err>;
}
