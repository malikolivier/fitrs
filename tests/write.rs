extern crate fitrs;

use std::process::Command;

use fitrs::{Fits, Hdu};

#[test]
fn write_single_hdu_file() {
    let data = (0..20)
        .map(|i| (0..20).map(move |j| i + j))
        .flatten()
        .collect();
    let primary_hdu = Hdu::new(&[20, 20], data);
    let _ = Fits::create("out.fits", primary_hdu).expect("created!");

    let output = Command::new("verify/verify.py")
        .arg("out.fits")
        .output()
        .expect("Failed to execute verify command");

    assert_eq!(output.stdout, vec![], "No standard output");
    assert_eq!(output.stderr, vec![], "No error output");
    assert!(output.status.success(), "Exit with success");
}
