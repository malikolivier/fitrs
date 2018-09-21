use fits::{FitsData, FitsDataArray};

pub trait FitsDataType: Sized {
    fn new_fits_array(shape: &[usize], data: Vec<Self>) -> FitsData;

    fn bitpix() -> usize;
}

impl FitsDataType for char {
    fn new_fits_array(shape: &[usize], data: Vec<char>) -> FitsData {
        FitsData::Characters(FitsDataArray {
            shape: Vec::from(shape),
            data,
        })
    }

    fn bitpix() -> usize {
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

    fn bitpix() -> usize {
        32
    }
}

impl FitsDataType for u32 {
    fn new_fits_array(shape: &[usize], data: Vec<u32>) -> FitsData {
        FitsData::IntegersU32(FitsDataArray {
            shape: Vec::from(shape),
            data: data.into_iter().map(Some).collect(),
        })
    }

    fn bitpix() -> usize {
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

    fn bitpix() -> usize {
        32
    }
}
impl FitsDataType for f64 {
    fn new_fits_array(shape: &[usize], data: Vec<f64>) -> FitsData {
        FitsData::FloatingPoint64(FitsDataArray {
            shape: Vec::from(shape),
            data,
        })
    }

    fn bitpix() -> usize {
        64
    }
}
