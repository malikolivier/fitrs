use fits::{FitsData, FitsDataArray};

/// A type that can be stored in a FITS data array implements this trait.
pub trait FitsDataType: Sized {
    fn new_fits_array(shape: &[usize], data: Vec<Self>) -> FitsData;

    fn bitpix() -> i32;
}

impl FitsDataType for char {
    fn new_fits_array(shape: &[usize], data: Vec<char>) -> FitsData {
        FitsData::Characters(FitsDataArray {
            shape: Vec::from(shape),
            data,
        })
    }

    fn bitpix() -> i32 {
        8
    }
}

impl FitsDataType for i32 {
    fn new_fits_array(shape: &[usize], data: Vec<i32>) -> FitsData {
        FitsData::IntegersI32(FitsDataArray {
            shape: Vec::from(shape),
            data: data.into_iter().map(Some).collect(),
        })
    }

    fn bitpix() -> i32 {
        32
    }
}

impl FitsDataType for u16 {
    fn new_fits_array(shape: &[usize], data: Vec<u16>) -> FitsData {
        FitsData::IntegersU16(FitsDataArray {
            shape: Vec::from(shape),
            data: data.into_iter().map(Some).collect(),
        })
    }

    fn bitpix() -> i32 {
        16
    }
}

impl FitsDataType for u32 {
    fn new_fits_array(shape: &[usize], data: Vec<u32>) -> FitsData {
        FitsData::IntegersU32(FitsDataArray {
            shape: Vec::from(shape),
            data: data.into_iter().map(Some).collect(),
        })
    }

    fn bitpix() -> i32 {
        32
    }
}

impl FitsDataType for f32 {
    fn new_fits_array(shape: &[usize], data: Vec<f32>) -> FitsData {
        FitsData::FloatingPoint32(FitsDataArray {
            shape: Vec::from(shape),
            data,
        })
    }

    fn bitpix() -> i32 {
        -32
    }
}

impl FitsDataType for f64 {
    fn new_fits_array(shape: &[usize], data: Vec<f64>) -> FitsData {
        FitsData::FloatingPoint64(FitsDataArray {
            shape: Vec::from(shape),
            data,
        })
    }

    fn bitpix() -> i32 {
        -64
    }
}

impl FitsDataType for () {
    fn new_fits_array(_shape: &[usize], _data: Vec<()>) -> FitsData {
        FitsData::IntegersU32(FitsDataArray {
            shape: vec![],
            data: vec![],
        })
    }

    fn bitpix() -> i32 {
        8
    }
}
