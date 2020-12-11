/*
    symbol tables
    string tables
    type checking and inference
    stack machine execution
    x64 assembly output?
*/

mod builder;
mod lexer;
//mod module;
mod parser;
mod strtab;

fn main() {
    use parser::Parser;
    let filename = "input.txt".to_string();
    let mut parser = Parser::new_file(filename);
    let mut module = parser::module(&mut parser);

    for item in module.items.iter_mut() {
        item.remove_blocks();
    }

    println!("OUTPUT--------------------------\n");
    println!("{}\n", module);

    /*println!("BLOCKS--------------------------");
    println!("{}", builder);
    println!("ERRORS--------------------------");
    for item in items {
        if let Err(e) = item {
            eprintln!("{}", e);
        }
    }
    println!("ASM--------------------------");
    println!("{}", builder.out_asm().unwrap());
    */
}
