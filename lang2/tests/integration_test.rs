use lang2::{
    env::{Architecture, Target},
    translate::translate,
};
use std::{
    fs::{read_dir, read_to_string},
    path::Path,
};

#[test]
fn test_compile() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("testcases");
    let files = read_dir(path).unwrap();

    for file in files {
        let file = file.unwrap();
        let name = file.file_name();
        let source_code = read_to_string(file.path()).unwrap();
        let program = translate(
            source_code,
            Target {
                architecture: Architecture::IA32,
            },
        )
        .expect(format!("cannot translate source code {:?}", name).as_str());
        println!("{:?}", program);
    }

    panic!();
}
