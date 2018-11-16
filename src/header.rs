use fits::HeaderValue;

impl From<String> for HeaderValue {
    fn from(s: String) -> HeaderValue {
        HeaderValue::CharacterString(s)
    }
}

impl<'a> From<&'a str> for HeaderValue {
    fn from(s: &str) -> HeaderValue {
        HeaderValue::CharacterString(s.to_owned())
    }
}

impl From<i32> for HeaderValue {
    fn from(n: i32) -> HeaderValue {
        HeaderValue::IntegerNumber(n)
    }
}

impl From<f32> for HeaderValue {
    fn from(f: f32) -> HeaderValue {
        HeaderValue::RealFloatingNumber(f64::from(f))
    }
}

impl From<f64> for HeaderValue {
    fn from(f: f64) -> HeaderValue {
        HeaderValue::RealFloatingNumber(f)
    }
}
