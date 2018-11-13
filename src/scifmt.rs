/// TODO: This formatter is not what we want
/// what we want: 1.51515151515152E+01
/// what we have: 1.51515151515152E1
pub trait SciFmt {
    fn sci_fmt(&self) -> String;
}

impl SciFmt for f64 {
    fn sci_fmt(&self) -> String {
        format!("{:E}", self)
    }
}
