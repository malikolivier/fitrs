//! Library to parse FITS file written in pure rust.
//!
//! Uses only one dependency, [byteorder](../byteorder), to deal with endianness.
//!
//! Uses intelligent cache to parse big FITS files. Developed for use in
//! multi-threaded environments.
//!
//! # How to use
//!
//! ```rust,no_run
//! extern crate fitrs;
//! use fitrs::{Fits, FitsData, FitsDataArray};
//!
//! let fits = Fits::open("path/to/fits/file.fits").expect("Failed to open");
//! // Iterate over HDUs
//! for hdu in fits.iter() {
//!     println!("{:?}", hdu.value("EXTNAME"));
//!     println!("{:?}", hdu.read_data());
//! }
//!
//! // Get HDU by ID
//! let hdu_2 = &fits[2];
//! // Get HDU by EXTNAME
//! let hdu_flux = &fits["FLUX"];
//!
//! match hdu_flux.read_data() {
//!     &FitsData::FloatingPoint32(FitsDataArray { ref shape, ref data }) => {
//!         println!("{:?}", shape);
//!         println!("{:?}", data);
//!     }
//!     _ => { /* ... */ }
//! }
//! ```
//!
//! A lot of possibly desirable functionalities are still missing.
//! PR are welcome.
//! Support for writing FITS file is planned.

extern crate byteorder;

mod fits;
pub use fits::{Fits, FitsData, FitsDataArray, Hdu, HeaderValue};
pub use fits::{FitsIntoIter, FitsIter, FitsIterMut};
