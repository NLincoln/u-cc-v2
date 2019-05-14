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
    let ast = lang::parse(&code).unwrap();
    typechk::type_check(&ast).unwrap();
}
