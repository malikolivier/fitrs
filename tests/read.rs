extern crate fitrs;

use fitrs::{Fits, HeaderValue};

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
