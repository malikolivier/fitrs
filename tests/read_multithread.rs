extern crate fitrs;

use fitrs::{Fits, FitsData};

use std::sync::Arc;
use std::thread;

#[test]
fn read_second_hdu_array_from_n_threads() {
    const FIRST_30_CHARS: &[char] = &[
        '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}',
        '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{80}',
        '\u{0}', 'ÿ', 'ÿ', 'ÿ', 'ÿ', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}',
    ];

    const THREAD_COUNT: usize = 10;
    let fits = Arc::new(Fits::open("tests/testprog.fit").unwrap());
    let fits_arcs = (0..THREAD_COUNT).map(|_| fits.clone());
    let mut panicked = vec![];

    let mut children = Vec::with_capacity(THREAD_COUNT);
    for fits in fits_arcs {
        let child = thread::spawn(move || {
            let mut iter = fits.iter();
            iter.next();
            let table_hdu_1 = iter.next().unwrap();
            let data = table_hdu_1.read_data();
            match data {
                FitsData::Characters(array) => {
                    assert_eq!(&array.data[..30], FIRST_30_CHARS,);
                }
                _ => panic!("Should be Characters!"),
            }
        });
        children.push(child);
    }

    for (i, result) in children.into_iter().map(|child| child.join()).enumerate() {
        if let Err(e) = result {
            panicked.push((i, e));
        }
    }
    if panicked.len() > 0 {
        panic!("{} threads panicked: {:?}", panicked.len(), panicked);
    }
}
