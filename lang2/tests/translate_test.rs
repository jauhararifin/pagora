use lang2::{errors::Result, translate, semantic::Unit};
use std::{
    fs::{read_dir, read_to_string},
    path::{Path, PathBuf},
};

#[test]
fn test_translation() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("testcases");
    let files = read_dir(path).unwrap();

    for file in files {
        let mut file = file.unwrap().path();
        if !file.is_dir() {
            continue;
        }

        let mut path = PathBuf::from("example.com/testcases");
        path.push(file.file_name().unwrap());

        let pkg_name = path.as_path().to_str().unwrap();
        let unit = translate(pkg_name);

        file.set_extension("yaml");
        let expected = read_to_string(file).unwrap();
        let expected_unit: Result<Unit> = serde_yaml::from_str(&expected).unwrap();
        assert_eq!(expected_unit, unit);
    }
}
