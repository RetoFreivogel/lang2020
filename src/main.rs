/*
	symbol tables
	string tables
	type checking and inference
	stack machine execution
	x64 assembly output?
*/

mod parser;
mod lexer;
mod module;
mod builder;

fn main() {
    use parser::Parser;
    let filename = "input.txt".to_string();
    let input = std::fs::File::open(&filename).unwrap();
    let mut parser = Parser::new_file(filename, input);
    let items = parser.module();
    let (module, builder) = parser.done();

    println!("{}", module);
	println!("--------------");
    let mut out = String::new();
    builder.print(&mut out).unwrap();
    println!("{}", out);

    for item in items{
        if let Err(e) = item{
		  eprintln!("{}", e);
        }
    }
}

