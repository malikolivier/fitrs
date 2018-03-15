extern crate byteorder;
#[cfg(test)]
extern crate time;

mod fits;
pub use fits::{Fits, FitsData, FitsDataArray};
