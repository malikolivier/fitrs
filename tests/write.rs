extern crate fitrs;

use fitrs::{Fits, Hdu};

#[test]
fn write_single_hdu_file() {
    let primary_hdu = Hdu::new(&[10, 10], vec![0.0f32; 100]);
    let mut _fits = Fits::create("out.fits", primary_hdu).expect("created!");
}
