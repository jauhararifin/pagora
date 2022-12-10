use std::{collections::HashMap, fs, path::PathBuf};

use crate::errors::{CompileError, Result};

pub struct Package<T> {
    path: String,
    files: HashMap<String, T>,
}

pub fn load_package(package_path: &str) -> Result<Package<Box<PathBuf>>> {
    let mut path = std::env::var("PAGORA_ROOT")
        .map(|path| PathBuf::from(path))
        .or_else(|_| std::env::current_dir())?;
    path.push("lib/");
    path.push(package_path);

    if !path.is_dir() {
        return Err(CompileError::from_message(
            None,
            format!("package {} not found", package_path),
        ));
    }

    let mut files: HashMap<String, Box<PathBuf>> = HashMap::new();
    for file in fs::read_dir(&path)? {
        let file = file?;
        files.insert(
            String::from(file.path().to_str().unwrap()),
            Box::new(file.path()),
        );
    }

    Ok(Package {
        path: String::from(path.to_str().unwrap()),
        files,
    })
}
