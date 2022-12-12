use std::{
    env::{set_current_dir, set_var},
    fs::{read_dir, File},
    path::{Path, PathBuf},
};

use lang2::translate;

fn main() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("testcases");
    set_current_dir(&path).unwrap();
    set_var("PAGORA_ROOT", "..");

    let entries = read_dir(path).unwrap();
    for entry in entries {
        let mut entry = entry.unwrap().path();
        if !entry.is_dir() {
            continue;
        }

        let mut path = PathBuf::from("example.com/testcases");
        path.push(entry.file_name().unwrap());

        let pkg_name = path.as_path().to_str().unwrap();
        println!("translating {}", pkg_name);
        let unit = translate(pkg_name);

        entry.set_extension("yaml");
        let file = File::create(entry).unwrap();
        serde_yaml::to_writer(file, &unit).unwrap();
    }
}
