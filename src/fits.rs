use std::fs::File;
use std::io::{Read, Error, Seek, SeekFrom};
use std::result::Result;
use std::str::{FromStr, from_utf8};
use std::rc::Rc;
use std::cell::RefCell;

use byteorder::{BigEndian, ReadBytesExt};


pub struct Fits {
    file: Rc<RefCell<File>>,
}

pub struct FitsIntoIter {
    file: Rc<RefCell<File>>,
    position: u64,
}

#[derive(Debug)]
pub struct Hdu {
    header: Vec<(HeaderKeyWord, Option<HeaderValueComment>)>,
    data_start: u64,
    file: Rc<RefCell<File>>,
    data: Option<FitsData>,
}

#[derive(Debug)]
pub enum FitsData {
    Characters(FitsDataArray<char>),
    IntegersI32(FitsDataArray<Option<i32>>),
    IntegersU32(FitsDataArray<Option<u32>>),
    FloatingPoint32(FitsDataArray<f32>),
    FloatingPoint64(FitsDataArray<f64>),
}

#[derive(Debug)]
pub struct FitsDataArray<T> {
    shape: Vec<usize>,
    data: Vec<T>,
}

impl<T> FitsDataArray<T> {
    fn new(shape: &[usize]) -> Self {
        Self { shape: Vec::from(shape), data: Vec::new() }
    }
}

type HeaderKeyWord = String;

#[derive(Debug)]
struct HeaderValueComment {
    value: Option<HeaderValue>,
    comment: Option<HeaderComment>,
}

#[derive(PartialEq, Debug)]
pub enum HeaderValue {
    CharacterString(String),
    Logical(bool),
    IntegerNumber(i32),
    RealFloatingNumber(f64),
    ComplexIntegerNumber(i64, i64),
    ComplexFloatingNumber(f64, f64),
}

type HeaderComment = String;

struct CardImage([u8; 80]);

impl Fits {
    pub fn open(path: &str) -> Result<Fits, Error> {
        File::open(path).map(|file| {
            Fits { file: Rc::new(RefCell::new(file)) }
        })
    }
}

impl IntoIterator for Fits {
    type Item = Hdu;
    type IntoIter = FitsIntoIter;
    fn into_iter(self) -> Self::IntoIter {
        FitsIntoIter { file: self.file, position: 0 }
    }
}

impl FitsIntoIter {
    fn tell(&mut self) -> u64 {
        self.file.borrow_mut().seek(SeekFrom::Current(0))
                              .expect("Could not get cursor position!")
    }

    fn set_position(&mut self) {
        self.file.borrow_mut().seek(SeekFrom::Start(self.position))
                              .expect("Could not set position!");
    }
}

impl Hdu {
    fn tell(&mut self) -> u64 {
        self.file.borrow_mut().seek(SeekFrom::Current(0))
                              .expect("Could not get cursor position!")
    }

    fn set_position(&mut self) {
        self.file.borrow_mut().seek(SeekFrom::Start(self.data_start))
                              .expect("Could not set position!");
    }
}

impl Iterator for FitsIntoIter {
    type Item = Hdu;
    fn next(&mut self) -> Option<Self::Item> {
        self.set_position();
        let mut line = CardImage::new();
        let mut line_count = 0;
        let mut header = Vec::new();
        let mut end = false;
        while (line_count % 36) != 0 || !end {
            match self.file.borrow_mut().read_exact(&mut line.0) {
                Ok(_)  => {
                    line.to_header_key_value().map(|(key, val)| {
                        if key == "END" {
                            end = true;
                        }
                        header.push((key, val));
                    });
                },
                Err(_) => return None,
            };
            line_count += 1;
        }
        let data_start_position = self.tell();
        let hdu = Hdu {
            header: header,
            data_start: data_start_position,
            file: self.file.clone(),
            data: None,
        };
        hdu.data_byte_length().map(|len| {
            let mut next_position = data_start_position + (len as u64);
            /* Go to end of record */
            while (next_position % (36 * 80)) != 0 {
                next_position += 1;
            }
            self.position = next_position;
        });
        Some(hdu)
    }
}

impl Hdu {
    pub fn value(&self, key: &str) -> Option<&HeaderValue> {
        for line in self.header.iter() {
            if line.0 == key {
                return line.1.as_ref().and_then(|value_comment| { value_comment.value.as_ref() });
            }
        }
        None
    }

    fn value_as_integer_number(&self, key: &str) -> Option<i32> {
        self.value(key).and_then(|val| {
            match val {
                &HeaderValue::IntegerNumber(n) => Some(n),
                _                              => None,
            }
        })
    }

    fn naxis(&self) -> Option<Vec<usize>> {
        self.value_as_integer_number("NAXIS").and_then(|naxis| {
            let mut vec = Vec::new();
            for i in 1..(naxis + 1) {
                let mut key = String::from("NAXIS");
                key.push_str(&i.to_string());
                match self.value_as_integer_number(&key) {
                    None => return None,
                    Some(k) => vec.push(k as usize),
                }
            }
            Some(vec)
        })
    }

    fn data_length(&self) -> Option<usize> {
        self.naxis().map(|naxis| {
            let mut len = 0;
            for (i, k) in naxis.iter().enumerate() {
                if i == 0 {
                    len += *k as usize;
                } else {
                    len *= *k as usize;
                }
            }
            len
        })
    }

    fn data_byte_length(&self) -> Option<usize> {
        self.data_length().and_then(|len| {
            self.value_as_integer_number("BITPIX").map(|bit| {
                let bit = if bit < 0 { -bit } else { bit };
                len * (bit as usize / 8)
            })
        })
    }

    pub fn is_data_cached(&self) -> bool {
        self.data.is_some()
    }

    pub fn data(&self) -> Option<&FitsData> {
        self.data.as_ref()
    }

    pub fn read_data(&mut self) -> &FitsData {
        if self.is_data_cached() {
            self.data.as_ref().unwrap()
        } else {
            self.read_data_force()
        }
    }

    fn read_data_force(&mut self) -> &FitsData {
        let bitpix = self.value_as_integer_number("BITPIX").expect("BITPIX is present");
        match bitpix {
            8   => self.read_data_u8_force(),
            16  => self.read_data_i16_force(),
            32  => self.read_data_i32_force(),
            -32 => self.read_data_f32_force(),
            -64 => self.read_data_f64_force(),
            _   => panic!("Unexpected value for BITPIX")
        }
    }

    fn read_data_u8_force(&mut self) -> &FitsData {
        let naxis = self.naxis().expect("Get NAXIS");
        let length = naxis.iter().fold(1, |acc, x| acc * x);
        let mut array = FitsDataArray::new(&naxis);
        let mut buf = [0u8; 1];
        for i in 0..length {
            self.file.borrow_mut().read_exact(&mut buf).expect("Read array");
            for a in buf.iter() {
                array.data.push(*a as char);
            }

        }
        self.data = Some(FitsData::Characters(array));
        self.data.as_ref().unwrap()
    }

    fn read_data_i16_force(&mut self) -> &FitsData {
        unimplemented!()
    }

    fn read_data_i32_force(&mut self) -> &FitsData {
        let naxis = self.naxis().expect("Get NAXIS");
        let blank = self.value_as_integer_number("BLANK");
        let length = naxis.iter().fold(1, |acc, x| acc * x);
        let mut array = FitsDataArray::new(&naxis);
        let mut buf;
        self.set_position();
        for i in 0..length {
            buf = self.file.borrow_mut().read_i32::<BigEndian>().expect("Read array");
            if blank.is_some() && buf == blank.unwrap() {
                array.data.push(None);
            } else {
                array.data.push(Some(buf));
            }
        }
        self.data = Some(FitsData::IntegersI32(array));
        self.data.as_ref().unwrap()
    }

    fn read_data_f32_force(&mut self) -> &FitsData {
        unimplemented!()
    }

    fn read_data_f64_force(&mut self) -> &FitsData {
        unimplemented!()
    }
}

static EQUAL_U8: u8 = '=' as u8;
static SPACE_U8: u8 = ' ' as u8;
static SLASH_U8: u8 = '/' as u8;
static QUOTE_U8: u8 = '\'' as u8;
static T_U8: u8 = 'T' as u8;
static F_U8: u8 = 'F' as u8;

impl HeaderValue {
    fn new(value: &[u8]) -> Option<HeaderValue> {
        HeaderValue::new_character_string(value)
        .or_else(|| {
            HeaderValue::new_logical(value)
        })
        .or_else(|| {
            HeaderValue::new_integer(value)
        })
        .or_else(|| {
            HeaderValue::new_real_floating(value)
        })
    }

    fn new_character_string(subcard: &[u8]) -> Option<HeaderValue> {
        if subcard[0] != QUOTE_U8 {
            return None;
        }
        let subcard = &subcard[1..];
        let mut s = String::new();
        let mut prev_single_quote = false;
        let mut white_space_count = 0;
        for (i, c) in subcard.iter().enumerate() {
            if prev_single_quote {
                if *c == QUOTE_U8 {
                    s.push(*c as char);
                    prev_single_quote = false;
                } else {
                    break;
                }
            } else if *c == QUOTE_U8 {
                prev_single_quote = true;
                continue;
            } else if i > 0 && *c == SPACE_U8 {
                white_space_count += 1;
            } else {
                while white_space_count > 0 {
                    s.push(' ');
                    white_space_count -= 1;
                }
                s.push(*c as char);
            }
        }
        Some(HeaderValue::CharacterString(s))
    }

    fn new_logical(value: &[u8]) -> Option<HeaderValue> {
        let mut b = false;
        let logical_constant_column = 30 - 10 - 1;
        for (i, c) in value.iter().enumerate() {
            if i == logical_constant_column {
                if *c == T_U8 {
                    b = true;
                } else if *c == F_U8 {
                    b = false;
                } else {
                    return None;
                }
            } else if *c != SPACE_U8 {
                return None;
            }
        }
        Some(HeaderValue::Logical(b))
    }

    fn new_integer(value: &[u8]) -> Option<HeaderValue> {
        from_utf8(value).ok().and_then(|string| {
            let trimmed = string.trim();
            i32::from_str_radix(trimmed, 10).ok()
        }).map(HeaderValue::IntegerNumber)
    }

    fn new_real_floating(value: &[u8]) -> Option<HeaderValue> {
        from_utf8(value).ok().and_then(|string| {
            let trimmed = string.trim();
            f64::from_str(trimmed).ok()
        }).map(HeaderValue::RealFloatingNumber)
    }
}

impl HeaderValueComment {
    fn new(value_comment: &[u8]) -> HeaderValueComment {
        let mut value_comment_iter = value_comment.split(|c| { *c == SLASH_U8 });
        let value_slice = value_comment_iter.next();
        let comment_slice = value_comment_iter.next();
        HeaderValueComment {
            value: value_slice.and_then(HeaderValue::new),
            comment: comment_slice.map(|slice| {
                let mut comment = HeaderComment::new();
                for c in slice {
                    comment.push(*c as char);
                }
                String::from(comment.trim())
            }),
        }
    }
}

impl CardImage {
    fn new() -> CardImage {
        CardImage([0u8; 80])
    }

    fn to_header_key_value(&self) -> Option<(HeaderKeyWord, Option<HeaderValueComment>)> {
        let card = self.0;
        let keyword = &card[0..8];
        let value_indicator = &card[8..10];
        let value_comment = &card[10..80];
        let mut key = HeaderKeyWord::new();
        for c in keyword {
            if *c == SPACE_U8 {
                break;
            }
            key.push(*c as char);
        }
        if key.is_empty() {
            return None;
        }
        if value_indicator[0] == EQUAL_U8 && value_indicator[1] == SPACE_U8 {
            let val = HeaderValueComment::new(value_comment);
            Some((key, Some(val)))
        } else {
            Some((key, None))
        }
    }
}


#[cfg(test)]
mod tests {
    use super::{Fits, FitsData, CardImage, HeaderValue};

    impl CardImage {
        fn from(s: &str) -> CardImage {
            let mut card = [' ' as u8; 80];
            for (i, c) in s.chars().enumerate() {
                card[i] = c as u8;
            }
            CardImage(card)
        }
    }

    #[test]
    fn read_card_image_character_string() {
        let card = CardImage::from("AUTHOR  = 'Malik Olivier Boussejra <malik@boussejra.com>' /");
        let header_key_value = card.to_header_key_value().unwrap();
        assert_eq!(header_key_value.0, String::from("AUTHOR"));
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(value_comment.value, Some(HeaderValue::CharacterString(
            String::from("Malik Olivier Boussejra <malik@boussejra.com>"))));
        assert_eq!(value_comment.comment, Some(String::from("")));
    }

    #[test]
    fn read_card_image_no_comment() {
        let card = CardImage::from("AUTHOR  = ''");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(value_comment.value, Some(HeaderValue::CharacterString(String::from(""))));
        assert_eq!(value_comment.comment, None);
    }

    #[test]
    fn read_card_image_character_trailing_space() {
        let card = CardImage::from("AUTHOR  = '  ab d  '");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(value_comment.value, Some(HeaderValue::CharacterString(String::from("  ab d"))));
    }

    #[test]
    fn read_card_image_character_blank() {
        let card = CardImage::from("AUTHOR  = '  '");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(value_comment.value, Some(HeaderValue::CharacterString(String::from(" "))));
    }

    #[test]
    fn read_card_image_character_logical_true() {
        let card = CardImage::from("SIMPLE  =                    T /                     ");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(value_comment.value, Some(HeaderValue::Logical(true)));
    }

    #[test]
    fn read_card_image_character_logical_false() {
        let card = CardImage::from("SIMPLE  =                    F /                     ");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(value_comment.value, Some(HeaderValue::Logical(false)));
    }

    #[test]
    fn read_card_image_character_integer() {
        let card = CardImage::from("BITPIX  =                    8 /                     ");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(value_comment.value, Some(HeaderValue::IntegerNumber(8)));
    }

    #[test]
    fn read_card_image_character_real() {
        let card = CardImage::from("EXPTIME =              13501.5 / Total exposure time (seconds)");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(value_comment.value, Some(HeaderValue::RealFloatingNumber(13501.5)));
    }

    #[test]
    fn read_card_image_character_real_exp() {
        let card = CardImage::from("CDELT1  =      -1.666667E-03 /");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(value_comment.value, Some(HeaderValue::RealFloatingNumber(-1.666667E-03)));
    }

    #[test]
    fn read_first_hdu() {
        let fits = Fits::open("test/testprog.fit").unwrap();
        let mut iter = fits.into_iter();
        let hdu = iter.next().unwrap();
        assert_eq!(hdu.value("SIMPLE"), Some(&HeaderValue::Logical(true)));
        assert_eq!(hdu.value("CARD1"), Some(&HeaderValue::CharacterString(String::from("12345678901234567890123456789012345678901234567890123456789012345678"))));
        assert_eq!(hdu.value("CARD2"), Some(&HeaderValue::CharacterString(String::from("1234567890123456789012345678901234567890123456789012345678901234'67"))));
        assert_eq!(hdu.value("CARD3"), Some(&HeaderValue::CharacterString(String::from("1234567890123456789012345678901234567890123456789012345678901234''"))));
        assert_eq!(hdu.value("KY_IKYJ"), Some(&HeaderValue::IntegerNumber(51)));
        assert_eq!(hdu.value("KY_IKYE"), Some(&HeaderValue::RealFloatingNumber(-1.3346E+01)));
    }

    #[test]
    fn compute_hdu_data_byte_length() {
        let fits = Fits::open("test/testprog.fit").unwrap();
        let mut iter = fits.into_iter();
        let primary_hdu = iter.next().unwrap();
        assert_eq!(primary_hdu.data_byte_length(), Some((32 / 8) * 10 * 2));
    }

    #[test]
    fn iterate_over_hdu() {
        let fits = Fits::open("test/testprog.fit").unwrap();
        let mut iter = fits.into_iter();
        let primary_hdu = iter.next().unwrap();
        assert_eq!(primary_hdu.header[0].0, "SIMPLE");
        let hdu2 = iter.next().unwrap();
        assert_eq!(hdu2.header[0].0, "XTENSION");
        assert_eq!(hdu2.value("XTENSION").unwrap(), &HeaderValue::CharacterString(String::from("BINTABLE")));
        let hdu3 = iter.next().unwrap();
        assert_eq!(hdu3.header[0].0, "XTENSION");
        assert_eq!(hdu3.value("XTENSION").unwrap(), &HeaderValue::CharacterString(String::from("IMAGE")));
    }

    #[test]
    fn iterate_over_all_hdus() {
        let fits = Fits::open("test/testprog.fit").unwrap();
        assert_eq!(fits.into_iter().count(), 8);
    }

    #[test]
    fn make_primary_hdu_array() {
        let fits = Fits::open("test/testprog.fit").unwrap();
        let mut iter = fits.into_iter();
        let mut primary_hdu = iter.next().unwrap();
        let data = primary_hdu.read_data();
        match data {
            &FitsData::IntegersI32(ref array) => {
                assert_eq!(array.shape, vec![10, 2]);
                assert_eq!(array.data, vec![None, Some(2), Some(3), None, Some(5),
                                            Some(6), Some(7), None, Some(9), Some(10),
                                            Some(11), None, Some(13), Some(14), Some(15),
                                            None, Some(17), Some(18), Some(19), None]);
            }
            _ => panic!("Should be IntegersI32!")
        }
    }

    #[test]
    fn read_second_hdu_array() {
        // TODO TableHDU are not handled yet
        let fits = Fits::open("test/testprog.fit").unwrap();
        let mut iter = fits.into_iter();
        iter.next();
        let mut table_hdu_1 = iter.next().unwrap();
        let data = table_hdu_1.read_data();
        match data {
            &FitsData::Characters(ref array) => {
                assert_eq!(array.shape, vec![61, 20]);
                assert_eq!(&array.data[..30], &vec!['\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}',
                                                    '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}',
                                                    '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}',
                                                    '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{80}',
                                                    '\u{0}', 'ÿ', 'ÿ', 'ÿ', 'ÿ',
                                                    '\u{0}', '\u{0}', '\u{0}', '\u{0}', '\u{0}'][..]);
            }
            _ => panic!("Should be Characters!")
        }
    }
}
