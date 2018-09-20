#![feature(test)]
extern crate test;

extern crate fitrs;
use fitrs::Fits;

const FITS_FILE: &str = "tests/testprog.fit";

#[bench]
fn fits_load_all(b: &mut test::Bencher) {
    b.iter(|| {
        let fits = Fits::open(FITS_FILE).unwrap();
        fits.load_all();
    });
}

#[bench]
fn fits_load_all_from_cache(b: &mut test::Bencher) {
    let fits = Fits::open(FITS_FILE).unwrap();
    fits.load_all();
    b.iter(|| {
        fits.load_all();
    });
}
