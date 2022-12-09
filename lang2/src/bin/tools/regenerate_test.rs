use lang2::{test::TranslateResult, translate};
use std::{
    fs::{read_dir, read_to_string, File},
    path::Path,
};

fn main() {
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
        let target_file = File::create(file).unwrap();
        serde_yaml::to_writer(target_file, &result).expect("cannot encode result to yaml")
    }
}
