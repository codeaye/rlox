mod arena;
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
    let mut interner = Interner::new();
    let mut scanner = Scanner::new(source_str, &mut interner);
    scanner.scan()?;
    // println!("{:#?}", scanner.output);
    let mut vm = VM::new();
    let mut compiler = Compiler::new(&scanner.output, &mut vm, source_str);
    compiler.compile()?;
    #[cfg(debug_assertions)]
    vm.debug(&interner);
    dbg!(&interner);
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

            let source_str = std::str::from_utf8(&buf_clone).map_err(|_| ProcessError {
                advice: "invalid source provided as input!".into(),
            })?;
            let mut interner: Interner = Interner::new();

            let mut scanner = Scanner::new(source_str, &mut interner);
            if let Err(e) = scanner.scan() {
                println!("{:?}", e);
                println!("-> Reverting to state from previous command");
                continue;
            };

            let mut vm = VM::new();

            let mut compiler = Compiler::new(&scanner.output, &mut vm, source_str);

            if let Err(e) = compiler.compile() {
                println!("{:?}", e);
                println!("-> Reverting to state from previous command");
                continue;
            };

            if let Err(e) = vm.run(&interner) {
                println!("{:?}", e);
                println!("-> Reverting to state from previous command");
                continue;
            }

            buf = buf_clone
        }
    }

    Ok(())
}
