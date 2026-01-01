mod arena;
mod compiler;
mod errors;
mod interner;
mod scanner;
mod typedef;
mod vm;

use miette::{IntoDiagnostic, Result};
use std::{
    env,
    fs::File,
    io::{BufRead, Read, Write, stdin, stdout},
    path::PathBuf,
    sync::Arc,
};

use crate::{
    compiler::Compiler, errors::ProcessError, interner::Interner, scanner::Scanner, vm::VM,
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
    let source: Arc<str> = Arc::from(source_str);
    let bytes = source.as_bytes();

    // println!("{:#?}", scanner.output);

    let mut interner = Interner::new(source.clone());
    let mut vm = VM::new();
    let mut binding = Scanner::new(bytes, source.clone(), &mut interner);
    let mut compiler = Compiler::new(&mut binding, source.clone(), &mut vm)?;
    compiler.compile()?;
    vm.run(&interner)?;
    // #[cfg(debug_assertions)]
    // vm.debug(&interner);
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
            let source_str = std::str::from_utf8(&buf).map_err(|_| ProcessError {
                advice: "invalid source provided as input!".into(),
            })?;
            compile(source_str)?;
        } else {
            return Err(ProcessError {
                advice: format!("the file \"{}\" does not exist!", path_loc),
            }
            .into());
        }
    } else {
        loop {
            let mut buf_clone = buf.clone();
            let mut new = Vec::new();

            read_input("> ", &mut new).into_diagnostic()?;
            buf_clone.append(&mut new);

            let source_str = std::str::from_utf8(&buf).map_err(|_| ProcessError {
                advice: "invalid source provided as input!".into(),
            })?;
            compile(source_str)?;
        }
    }

    Ok(())
}
