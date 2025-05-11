use std::{env, fs, path::Path, process::Command};

fn run_neu(exe: &str, script: &Path) -> (Vec<u8>, Vec<u8>) {
    let output = Command::new(exe)
        .arg(script)
        .output()
        .unwrap_or_else(|e| panic!("failed to run {} on {}: {}", exe, script.display(), e));
    (output.stdout, output.stderr)
}

fn read_golden(script: &Path, ext: &str) -> Vec<u8> {
    let golden = script.with_extension(ext);
    fs::read(&golden).unwrap_or_else(|_| panic!("missing golden file: {}", golden.display()))
}

#[test]
fn golden_programs() {
    let neu_exe = env!("CARGO_BIN_EXE_neu");
    let tests_dir = Path::new("tests/programs");

    let scripts = fs::read_dir(tests_dir)
        .unwrap_or_else(|_| panic!("couldn't read {}", tests_dir.display()))
        .filter_map(Result::ok)
        .map(|e| e.path())
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("neu"));

    for script in scripts {
        let (stdout, stderr) = run_neu(neu_exe, &script);

        let expected = read_golden(&script, "out");
        assert_eq!(stdout, expected, "STDOUT mismatch for {}", script.display());

        let expected_err = read_golden(&script, "err");
        assert_eq!(
            stderr,
            expected_err,
            "STDERR mismatch for {}",
            script.display()
        );
    }
}

