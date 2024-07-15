pub mod asm64;
pub mod ir;

use io::Write;
use parser::parser::Program;
use std::io;

pub trait CodeGenerator {
    type Err;

    fn generate(self, node: Program, buffer: &mut impl Write) -> Result<(), Self::Err>;
}
