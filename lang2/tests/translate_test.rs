use lang2::{test::TranslateResult, translate::translate};
use std::{
    fs::{read_dir, read_to_string, File},
    path::Path,
};

#[test]
fn test_translation() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("testcases");
    let files = read_dir(path).unwrap();

    for file in files {
        let mut file = file.unwrap().path();
        if file.extension().unwrap() != "p" {
            continue;
        }

        let source_code = read_to_string(&file).unwrap();
        let result: TranslateResult = translate(source_code).into();

        file.set_extension("program.yaml");
        let mut expected_file = File::open(file).unwrap();
        let expected_result: TranslateResult = serde_yaml::from_reader(&mut expected_file).unwrap();
        assert_eq!(expected_result, result);
    }
}
