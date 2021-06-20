/*
    type checking and inference
    stack machine execution
    x64 assembly output ???
*/

mod builder;
mod lexer;
mod parser;
mod strtab;

fn main() {
    use parser::Parser;
    let filename = "input.txt".to_string();
    let mut parser = Parser::new_file(filename);
    let ast_module = parser.module();
    let module = builder::build_mod(&ast_module);

    println!("OUTPUT--------------------------\n");
    println!("{}\n", module);

    println!("ASSEMBLY------------------------\n");
    //println!("{}", module.assemble());
}
