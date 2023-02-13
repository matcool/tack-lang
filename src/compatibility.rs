use std::process::Command;
use std::{fs::File, path::Path};

pub fn check_compatibility(linker: String) {
    let result = check_compatibility_impl(linker);
    if result.is_err() {
        println!("Error: {}", result.unwrap_err());
        // maybe exit here?
    }
}

fn check_compatibility_impl(linker: String) -> std::io::Result<()> {
    if !Path::new(".tkcache").exists() {
        initial_compat_check(linker);
        let _ = File::create(".tkcache")?;
    }
    Ok(())
}

fn initial_compat_check(linker: String) {
    if cfg!(windows) {
        if Command::new("bash").arg("--version").output().is_err() {
            println!("Error: WSL installation not found. Please install WSL and try again.");
            std::process::exit(1)
        }
    }

    if get_os_command().arg("-c").arg("nasm").output().is_err() {
        println!("Error: NASM installation not found.");
        std::process::exit(1)
    }

    if get_os_command()
        .arg("-c")
        .arg(format!("{linker}"))
        .output()
        .is_err()
    {
        println!("Error: {linker} installation not found.");
        std::process::exit(1)
    }
}

fn get_os_command() -> Command {
    return if cfg!(windows) {
        Command::new("bash")
    } else {
        Command::new("sh")
    };
}
