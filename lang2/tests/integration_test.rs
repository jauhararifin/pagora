use lang2::{
    env::{Architecture, Target},
    semantic::Program,
    translate::translate,
};
use std::{
    fs::{read_dir, read_to_string, File},
    path::Path,
};

#[test]
fn test_compile() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("testcases");
    let files = read_dir(path).unwrap();

    for file in files {
        let mut file = file.unwrap().path();
        if file.extension().unwrap() != "p" {
            continue;
        }

        let source_code = read_to_string(&file).unwrap();
        let program = translate(
            source_code,
            Target {
                architecture: Architecture::IA32,
            },
        )
        .expect(
            format!(
                "cannot translate source code {:?}",
                file.file_name().unwrap()
            )
            .as_str(),
        );

        file.set_extension("program.json");
        let mut expected_file = File::open(file).unwrap();
        let expected_program: Program = serde_json::from_reader(&mut expected_file).unwrap();
        assert_eq!(expected_program, program);
    }
}
