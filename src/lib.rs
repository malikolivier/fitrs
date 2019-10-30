//! Library to parse FITS file written in pure rust.
//!
//! Uses only one dependency, [byteorder](../byteorder), to deal with endianness.
//!
//! Uses intelligent cache to parse big FITS files. Developed for use in
//! multi-threaded environments.
//!
//! # How to use
//!
//! ## Read FITS
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
//! let hdu_2 = fits.get(2);
//! // Get HDU by EXTNAME
//! if let Some(hdu_flux) = fits.get_by_name("FLUX") {
//!     match hdu_flux.read_data() {
//!         FitsData::FloatingPoint32(FitsDataArray { shape, data }) => {
//!             println!("{:?}", shape);
//!             println!("{:?}", data);
//!         }
//!         _ => { /* ... */ }
//!     }
//! }
//!
//! ```
//!
//! ## Write FITS
//!
//! The FITS files written by `fitrs` are verified by astropy.io.fits for
//! standard compliance. If `fitrs` outputs a non-compliant FITS file, please
//! file a bug.
//!
//! ```rust
//! extern crate fitrs;
//! use fitrs::{Fits, Hdu};
//!
//! // Make example dummy data array
//! let shape = [20, 20];
//! let data = (0..shape[0])
//!     .map(|i| (0..shape[1]).map(move |j| i + j))
//!     .flatten()
//!     .collect();
//! let mut primary_hdu = Hdu::new(&[20, 20], data);
//! // Insert values in header
//! primary_hdu.insert("KEYSTR", "My string");
//! primary_hdu.insert("KEYSTR2", "Whatever value you want to save in this FITS files. Continued (long) strings are supported, if you happen to care.");
//! primary_hdu.insert("KEYFLOAT", 3.14);
//! primary_hdu.insert("KEYINT", 42);
//!
//! // Save file
//! Fits::create("new_file.fits", primary_hdu).expect("Failed to create");
//! ```
//!
//! A lot of possibly desirable functionalities are still missing.
//! PR are welcome.

extern crate byteorder;

mod fits;
mod header;
mod scifmt;
mod types;
mod wcs;

pub use fits::{Fits, FitsData, FitsDataArray, Hdu, HduIter, HeaderValue};
pub use fits::{FitsIntoIter, FitsIter};
pub use types::FitsDataType;
pub use wcs::WCS;
