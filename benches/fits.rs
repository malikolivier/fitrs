#![feature(test)]
extern crate test;

extern crate fitrs;
use fitrs::Fits;

const BIG_FITS_FILE: &str = "/home/malik/workspace/lab/aflak/data/JCMT_CO32.FITS";

#[bench]
fn fits_load_all(b: &mut test::Bencher) {
    b.iter(|| {
        let fits = Fits::open(BIG_FITS_FILE).unwrap();
        fits.load_all();
    })
}

#[bench]
fn fits_load_all_from_cache(b: &mut test::Bencher) {
    let fits = Fits::open(BIG_FITS_FILE).unwrap();
    fits.load_all();
    b.iter(|| {
        fits.load_all();
    })
}
