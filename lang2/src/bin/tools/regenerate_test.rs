use std::{
    fs::{read_dir, File},
    path::{Path, PathBuf},
};

use lang2::translate;

fn main() {
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
        let file = File::create(file).unwrap();
        serde_yaml::to_writer(file, &unit).unwrap();
    }
}
