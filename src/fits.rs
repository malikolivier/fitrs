use std::fs::File;
use std::io::{BufReader, Read, Error};
use std::result::Result;

pub struct Fits {
    buf: BufReader<File>,
}

pub struct FitsIntoIter {
    buf: BufReader<File>,
}

pub struct Hdu {
    header: Vec<(HeaderKeyWord, Option<HeaderValueComment>)>,
}

type HeaderKeyWord = String;

struct HeaderValueComment {
    value: Option<HeaderValue>,
    comment: Option<HeaderComment>,
}

#[derive(PartialEq, Debug)]
enum HeaderValue {
    CharacterString(String),
    Logical(bool),
    IntegerNumber(i64),
    RealFloatingNumber(f64),
    ComplexIntegerNumber(i64, i64),
    ComplexFloatingNumber(f64, f64),
}

type HeaderComment = String;

struct CardImage([u8; 80]);
struct HeaderRecord([CardImage; 36]);

impl Fits {
    pub fn open(path: &str) -> Result<Fits, Error> {
        File::open(path).map(|file| {
            let buf_reader = BufReader::new(file);
            Fits { buf: buf_reader }
        })
    }

    pub fn into_iter(self) -> FitsIntoIter {
        FitsIntoIter { buf: self.buf }
    }
}

static EQUAL_U8: u8 = '=' as u8;
static SPACE_U8: u8 = ' ' as u8;
static SLASH_U8: u8 = '/' as u8;
static QUOTE_U8: u8 = '\'' as u8;

impl HeaderValue {
    fn new(value: &[u8]) -> Option<HeaderValue> {
        HeaderValue::new_character_string(value)
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

impl Iterator for FitsIntoIter {
    type Item = Hdu;
    fn next(&mut self) -> Option<Self::Item> {
        let mut line = CardImage::new();
        let mut header = Vec::new();
        match self.buf.read_exact(&mut line.0) {
            Ok(_)  => {
                line.to_header_key_value().map(|v| {
                    header.push(v);
                    Hdu { header: header }
                })
            },
            Err(_) => None,
        }
    }
}


#[cfg(test)]
mod tests {
    use super::{CardImage, HeaderValue};

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
}
