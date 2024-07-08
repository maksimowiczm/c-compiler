pub mod asm64;

use crate::parser::Program;
use io::Write;
use std::error::Error;
use std::io;

pub trait CodeGenerator<B, E>
where
    B: Write,
    E: Error,
{
    fn generate(self, node: Program, buffer: &mut B) -> Result<(), E>;
}
