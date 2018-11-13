/// Format real floating-point number according to FITS standard (section 4.2.4)
pub trait SciFmt {
    fn sci_fmt(&self) -> String;
}

impl SciFmt for f64 {
    fn sci_fmt(&self) -> String {
        let mut exp = 0;
        let mut f = *self;
        if f.abs() >= 10.0 {
            while f.abs() >= 10.0 {
                f /= 10.0;
                exp += 1;
            }
        } else {
            while f.abs() < 1.0 {
                f *= 10.0;
                exp -= 1;
            }
        }
        if exp >= 0 {
            format!("{}E+{:02}", f, exp)
        } else {
            format!("{}E-{:02}", f, -exp)
        }
    }
}

#[cfg(test)]
mod test {
    use super::SciFmt;

    #[test]
    fn format_f64() {
        let f = 15.1515151515152;
        assert_eq!(f.sci_fmt(), "1.51515151515152E+01".to_owned())
    }

    #[test]
    fn format_small_f64() {
        let f = 1.1515151515152;
        assert_eq!(f.sci_fmt(), "1.1515151515152E+00".to_owned())
    }

    #[test]
    fn format_negative_f64() {
        let f = -15.1515151515152;
        assert_eq!(f.sci_fmt(), "-1.51515151515152E+01".to_owned())
    }

    #[test]
    fn format_very_small_f64() {
        let f = -0.0001515151515152;
        assert_eq!(f.sci_fmt(), "-1.515151515152E-04".to_owned())
    }
}
