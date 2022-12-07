use std::{
    fs::{read_dir, read_to_string},
    path::Path,
};

use lang2::{
    analyzer::analyze,
    env::{Architecture, Target},
    parser::parse,
    scanner::scan,
    semantic::Builtin,
};

#[test]
fn test_compile() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("testcases");
    let files = read_dir(path).unwrap();

    for file in files {
        let source_code = read_to_string(file.unwrap().path()).unwrap();
        let tokens = scan(&source_code).unwrap();
        let root_ast = parse(tokens).unwrap();
        let program = analyze(
            root_ast,
            Target {
                architecture: Architecture::IA32,
            },
            Builtin {
                variables: vec![],
                functions: vec![],
            },
        )
        .unwrap();

        println!("{:?}", program);
    }
    panic!();
}
