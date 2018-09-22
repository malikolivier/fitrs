extern crate fitrs;

use fitrs::{Fits, FitsData, HeaderValue};

#[test]
fn read_first_hdu() {
    let fits = Fits::open("tests/testprog.fit").unwrap();
    let mut iter = fits.into_iter();
    let hdu = iter.next().unwrap();
    assert_eq!(hdu.value("SIMPLE"), Some(&HeaderValue::Logical(true)));
    assert_eq!(
        hdu.value("CARD1"),
        Some(&HeaderValue::CharacterString(String::from(
            "12345678901234567890123456789012345678901234567890123456789012345678",
        )))
    );
    assert_eq!(
        hdu.value("CARD2"),
        Some(&HeaderValue::CharacterString(String::from(
            "1234567890123456789012345678901234567890123456789012345678901234'67",
        )))
    );
    assert_eq!(
        hdu.value("CARD3"),
        Some(&HeaderValue::CharacterString(String::from(
            "1234567890123456789012345678901234567890123456789012345678901234''",
        )))
    );
    assert_eq!(hdu.value("KY_IKYJ"), Some(&HeaderValue::IntegerNumber(51)));
    assert_eq!(
        hdu.value("KY_IKYE"),
        Some(&HeaderValue::RealFloatingNumber(-1.3346E+01))
    );
}

#[test]
fn iterate_over_all_hdus() {
    let fits = Fits::open("tests/testprog.fit").unwrap();
    assert_eq!(fits.into_iter().count(), 8);
}

#[test]
fn make_primary_hdu_array() {
    let fits = Fits::open("tests/testprog.fit").unwrap();
    let mut iter = fits.into_iter();
    let primary_hdu = iter.next().unwrap();
    let data = primary_hdu.read_data();
    match data {
        FitsData::IntegersI32(array) => {
            assert_eq!(array.shape, vec![10, 2]);
            assert_eq!(
                array.data,
                vec![
                    None,
                    Some(2),
                    Some(3),
                    None,
                    Some(5),
                    Some(6),
                    Some(7),
                    None,
                    Some(9),
                    Some(10),
                    Some(11),
                    None,
                    Some(13),
                    Some(14),
                    Some(15),
                    None,
                    Some(17),
                    Some(18),
                    Some(19),
                    None,
                ]
            );
        }
        _ => panic!("Should be IntegersI32!"),
    }
}

#[test]
fn read_second_hdu_array() {
    // TODO TableHDU are not handled yet
    let fits = Fits::open("tests/testprog.fit").unwrap();
    let mut iter = fits.into_iter();
    iter.next();
    let table_hdu_1 = iter.next().unwrap();
    let data = table_hdu_1.read_data();
    match data {
        FitsData::Characters(array) => {
            assert_eq!(array.shape, vec![61, 20]);
            assert_eq!(
                &array.data[..30],
                &vec![
                    '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}',
                    '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}',
                    '\u{0}', '\u{0}', '\u{0}', '\u{80}', '\u{0}', 'ÿ', 'ÿ', 'ÿ', 'ÿ', '\u{0}',
                    '\u{0}', '\u{0}', '\u{0}', '\u{0}',
                ][..]
            );
        }
        _ => panic!("Should be Characters!"),
    }
}
