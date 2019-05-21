fn main() {
    let args: Vec<_> = std::env::args_os().collect();
    let filename = &args[1];
    let code = std::fs::read_to_string(filename).unwrap();
    let ast = match lang::parse(&code) {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };
    typechk::type_check(&ast).unwrap();
    for func in codegen::codegen(&ast).unwrap() {
        println!("{}:", func.declaration());
        for inst in func.instructions() {
            println!("  {}", inst);
        }
    }
}
