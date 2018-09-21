extern crate fitrs;

use fitrs::{Fits, Hdu};

#[test]
fn write_single_hdu_file() {
    let data = (0..20)
        .map(|i| (0..20).map(move |j| i + j))
        .flatten()
        .collect();
    let primary_hdu = Hdu::new(&[20, 20], data);
    let mut _fits = Fits::create("out.fits", primary_hdu).expect("created!");
}
