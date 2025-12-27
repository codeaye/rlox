mod compiler;
mod errors;
mod interner;
mod scanner;
mod vm;

use miette::{IntoDiagnostic, Result};
use std::{
    env,
    fs::File,
    io::{BufRead, Read, Write, stdin, stdout},
    path::PathBuf,
};

use crate::{
    compiler::Compiler,
    errors::{InvalidInputPath, InvalidSource},
    interner::Interner,
    scanner::Scanner,
    vm::VM,
};

fn read_input(prompt: &str, buf: &mut Vec<u8>) -> Result<(), std::io::Error> {
    buf.clear();
    print!("{prompt}");
    stdout().flush()?;
    stdin().lock().read_until(b'\n', buf)?;
    buf.pop();
    Ok(())
}

fn read_file(path: PathBuf, buf: &mut Vec<u8>) -> Result<(), std::io::Error> {
    File::open(path)?.read_to_end(buf)?;
    Ok(())
}

fn compile(source_str: &str) -> Result<()> {
    let mut scanner = Scanner::new(source_str);
    scanner.scan()?;
    // println!("{:#?}", scanner.output);
    let mut interner = Interner::new();
    let mut vm = VM::new();
    let mut compiler = Compiler::new(&scanner.output, &mut vm, &mut interner, source_str);
    compiler.compile()?;
    // println!("{:?}", vm);
    vm.run(&mut interner)?;
    Ok(())
}

fn main() -> Result<()> {
    let mut buf = Vec::new();
    let args = env::args().collect::<Vec<String>>();

    if args.len() > 1 {
        let path_loc = &env::args().collect::<Vec<String>>()[1];
        let path = PathBuf::from(&path_loc);
        if path.is_file() {
            read_file(path, &mut buf).into_diagnostic()?;
            let source_str = std::str::from_utf8(&buf).map_err(|_| InvalidSource {})?;
            compile(source_str)?;
        } else {
            return Err(InvalidInputPath {
                advice: format!("the file \"{}\" does not exist!", path_loc),
            }
            .into());
        }
    } else {
        loop {
            read_input("> ", &mut buf).into_diagnostic()?;
            let source_str = std::str::from_utf8(&buf).map_err(|_| InvalidSource {})?;
            if let Err(e) = compile(source_str) {
                println!("{:?}", e)
            }
        }
    }

    Ok(())
}
