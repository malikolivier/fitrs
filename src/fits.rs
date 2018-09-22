use std::fs::File;
use std::io::{Error, Read, Seek, SeekFrom, Write};
use std::ops::{Index, IndexMut};
use std::path::Path;
use std::result::Result;
use std::str::{from_utf8, FromStr};
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, RwLock};

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

use types::FitsDataType;

type FileRc = Arc<Mutex<File>>;

/// Represent an open FITS file.
///
/// Implement caching. Thread-safe.
#[derive(Debug)]
pub struct Fits {
    file: FileRc,
    hdus: Mutex<AtomicPtr<Vec<Hdu>>>,
    total_hdu_count: RwLock<Option<usize>>,
}

/// We must release the Hdu cache!
impl Drop for Fits {
    fn drop(&mut self) {
        use std::ptr;
        let hdu_ptr = self.hdus.get_mut().unwrap().load(Ordering::SeqCst);
        unsafe { ptr::drop_in_place(hdu_ptr) };
    }
}

/// An iterator over [`Hdu`]s. Obtained from a consumed [`Fits`] object.
pub struct FitsIntoIter {
    fits: Fits,
    position: u64,
}

/// An iterator over references to [`Hdu`]s.
///
/// Use caching to avoid rereading the same data from file.
pub struct FitsIter<'f> {
    fits: &'f Fits,
    position: u64,
    count: usize,
}

/// An iterator over mutable references to [`Hdu`]s.
///
/// Use caching to avoid rereading the same data from file.
pub struct FitsIterMut<'f> {
    fits: &'f mut Fits,
    position: u64,
    count: usize,
}

/// Represent an HDU as defined in [FITS standard 4.1](https://archive.stsci.edu/fits/fits_standard/node13.html#SECTION00810000000000000000).
#[derive(Debug)]
pub struct Hdu {
    header: Vec<(HeaderKeyWord, Option<HeaderValueComment>)>,
    data_start: u64,
    file: Option<FileRc>,
    /// Cache of data inside Hdu
    data: RwLock<Option<FitsData>>,
}

/// Represent a data array inside an [`Hdu`].
///
/// Follows data representation as defined in [FITS standard 6](https://archive.stsci.edu/fits/fits_standard/node42.html#SECTION001000000000000000000).
#[derive(Debug)]
pub enum FitsData {
    Characters(FitsDataArray<char>),
    IntegersI32(FitsDataArray<Option<i32>>),
    IntegersU32(FitsDataArray<Option<u32>>),
    FloatingPoint32(FitsDataArray<f32>),
    FloatingPoint64(FitsDataArray<f64>),
}

/// Actual array data inside the [`Hdu`]
#[derive(Debug)]
pub struct FitsDataArray<T> {
    /// Shape of array.
    ///
    /// Example: A 2D image of width `w` and height `h` will be stored here as
    /// `[w, h]`.
    pub shape: Vec<usize>,
    /// Raw data stored in the [`Hdu`].
    pub data: Vec<T>,
}

impl<T> FitsDataArray<T> {
    fn new(shape: &[usize], data: Vec<T>) -> Self {
        Self {
            shape: Vec::from(shape),
            data,
        }
    }
}

impl FitsDataArray<char> {
    fn raw(&self) -> Vec<u8> {
        unimplemented!("Cannot write Characters")
    }
}

impl FitsDataArray<Option<i32>> {
    fn raw(&self) -> Vec<u8> {
        let mut data = Vec::with_capacity(4 * self.data.len());
        for n in &self.data {
            if let Some(n) = n {
                data.write_i32::<BigEndian>(*n).unwrap();
            } else {
                unimplemented!("Missing value not implemented for 32-bit integer arrays!");
            }
        }
        data
    }
}

impl FitsDataArray<Option<u32>> {
    fn raw(&self) -> Vec<u8> {
        let mut data = Vec::with_capacity(4 * self.data.len());
        for n in &self.data {
            if let Some(n) = n {
                data.write_u32::<BigEndian>(*n).unwrap();
            } else {
                unimplemented!("Missing value not implemented for unsigned 32-bit integer arrays!");
            }
        }
        data
    }
}

impl FitsDataArray<f32> {
    fn raw(&self) -> Vec<u8> {
        let mut data = Vec::with_capacity(4 * self.data.len());
        for f in &self.data {
            data.write_f32::<BigEndian>(*f).unwrap();
        }
        data
    }
}

impl FitsDataArray<f64> {
    fn raw(&self) -> Vec<u8> {
        let mut data = Vec::with_capacity(8 * self.data.len());
        for f in &self.data {
            data.write_f64::<BigEndian>(*f).unwrap();
        }
        data
    }
}

impl FitsData {
    fn raw(&self) -> Vec<u8> {
        match self {
            FitsData::Characters(chars) => chars.raw(),
            FitsData::IntegersI32(arr) => arr.raw(),
            FitsData::IntegersU32(arr) => arr.raw(),
            FitsData::FloatingPoint32(arr) => arr.raw(),
            FitsData::FloatingPoint64(arr) => arr.raw(),
        }
    }
}

type HeaderKeyWord = String;

#[derive(Debug)]
struct HeaderValueComment {
    value: Option<HeaderValue>,
    comment: Option<HeaderComment>,
}

/// Value stored inside the [`Hdu`] header.
///
/// As defined in [FITS standard 5.2](https://archive.stsci.edu/fits/fits_standard/node30.html#SECTION00920000000000000000).
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
    /// Open FITS file given in provided path.
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Fits, Error> {
        File::open(path).map(|file| Fits {
            file: Arc::new(Mutex::new(file)),
            hdus: Mutex::new(AtomicPtr::new(Box::into_raw(Box::new(Vec::new())))),
            total_hdu_count: RwLock::new(None),
        })
    }

    /// Iterate over references to [`Hdu`]s.
    pub fn iter(&self) -> FitsIter {
        FitsIter {
            fits: self,
            position: 0,
            count: 0,
        }
    }

    /// Iterate over mutable references to [`Hdu`]s.
    pub fn iter_mut(&mut self) -> FitsIterMut {
        FitsIterMut {
            fits: self,
            position: 0,
            count: 0,
        }
    }

    /// Force-read the whole FITS file and cache it.
    ///
    /// Beware of the size of the file you are loading before doing that.
    pub fn load_all(&self) {
        for hdu in self.iter() {
            hdu.read_data();
        }
    }

    /// Get reference to [`Hdu`] by index. Use `0` for primary HDU.
    pub fn get(&self, index: usize) -> Option<&Hdu> {
        for (i, hdu) in self.iter().enumerate() {
            if i == index {
                return Some(hdu);
            }
        }
        None
    }

    /// Get mutable reference to [`Hdu`] by index. Use `0` for primary HDU.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Hdu> {
        for (i, hdu) in self.iter_mut().enumerate() {
            if i == index {
                return Some(hdu);
            }
        }
        None
    }

    /// Get reference to [`Hdu`] by `EXTNAME`. Defined in [FIST standard 5.4.2.6](https://archive.stsci.edu/fits/fits_standard/node40.html#SECTION00942000000000000000)
    pub fn get_by_name(&self, index: &str) -> Option<&Hdu> {
        let value = Some(HeaderValue::CharacterString(String::from(index)));
        for hdu in self.iter() {
            if hdu.value("EXTNAME") == value.as_ref() {
                return Some(hdu);
            }
        }
        None
    }

    /// Get mutable reference to [`Hdu`] by `EXTNAME`. Defined in [FIST standard 5.4.2.6](https://archive.stsci.edu/fits/fits_standard/node40.html#SECTION00942000000000000000)
    pub fn get_mut_by_name(&mut self, index: &str) -> Option<&mut Hdu> {
        let value = Some(HeaderValue::CharacterString(String::from(index)));
        for hdu in self.iter_mut() {
            if hdu.value("EXTNAME") == value.as_ref() {
                return Some(hdu);
            }
        }
        None
    }

    fn hdus_guard(&self) -> MutexGuard<AtomicPtr<Vec<Hdu>>> {
        self.hdus.lock().unwrap()
    }
}

impl Fits {
    pub fn create<P: AsRef<Path>>(path: P, mut primary_hdu: Hdu) -> Result<Fits, Error> {
        File::create(path).and_then(|file| {
            let file_ptr = Arc::new(Mutex::new(file));

            primary_hdu.data_start = 0;
            primary_hdu.file = Some(file_ptr.clone());
            primary_hdu.write()?;

            Ok(Fits {
                file: file_ptr,
                hdus: Mutex::new(AtomicPtr::new(Box::into_raw(Box::new(vec![primary_hdu])))),
                total_hdu_count: RwLock::new(Some(1)),
            })
        })
    }

    pub fn push(&mut self, mut hdu: Hdu) -> Result<(), Error> {
        let hdu_guard = self.hdus_guard();
        let hdus = unsafe { &mut *hdu_guard.load(Ordering::SeqCst) };
        let last_hdu = &hdus[hdus.len() - 1];

        let mut header_len = last_hdu.header.len() as u64 * 80;
        while (header_len % (36 * 80)) != 0 {
            header_len += 1;
        }
        let data_len = last_hdu.data_byte_length().unwrap() as u64;

        let mut next_position = last_hdu.data_start + header_len + data_len;
        while (next_position % (36 * 80)) != 0 {
            next_position += 1;
        }
        hdu.data_start = next_position;
        hdu.file = Some(self.file.clone());

        hdu.header[0] = (
            "XTENSION".to_owned(),
            Some(HeaderValueComment {
                value: Some(HeaderValue::CharacterString("IMAGE".to_owned())),
                comment: None,
            }),
        );

        let penultimate = hdu.header.len() - 1;
        hdu.header.insert(
            penultimate,
            (
                "PCOUNT".to_owned(),
                Some(HeaderValueComment {
                    value: Some(HeaderValue::IntegerNumber(0)),
                    comment: None,
                }),
            ),
        );
        hdu.header.insert(
            penultimate + 1,
            (
                "GCOUNT".to_owned(),
                Some(HeaderValueComment {
                    value: Some(HeaderValue::IntegerNumber(1)),
                    comment: None,
                }),
            ),
        );

        hdu.write()
    }
}

///
impl Index<usize> for Fits {
    /// [`Hdu`] at index.
    type Output = Hdu;
    /// Get [`Hdu`] by index. Panic if index is larger than the number of
    /// [`Hdu`]s.
    /// Prefer [`Fits::get`] if you need to check.
    fn index(&self, index: usize) -> &Self::Output {
        for (i, hdu) in self.iter().enumerate() {
            if i == index {
                return hdu;
            }
        }
        panic!("Index out of range");
    }
}

impl IndexMut<usize> for Fits {
    /// Get mutable [`Hdu`] by index.
    /// Panic if index is larger than the number of [`Hdu`]s.
    /// Prefer [`Fits::get_mut`] if you need to check.
    fn index_mut(&mut self, index: usize) -> &mut Hdu {
        for (i, hdu) in self.iter_mut().enumerate() {
            if i == index {
                return hdu;
            }
        }
        panic!("Index out of range");
    }
}

///
impl<'s> Index<&'s str> for Fits {
    /// [`Hdu`] with provided `EXTNAME`.
    type Output = Hdu;
    /// Get [`Hdu`] by `EXTNAME`.
    /// Panic if `EXTNAME` is not found.
    /// Prefer [`Fits::get_by_name`] if you need to check.
    fn index(&self, index: &str) -> &Self::Output {
        let value = Some(HeaderValue::CharacterString(String::from(index)));
        for hdu in self.iter() {
            if hdu.value("EXTNAME") == value.as_ref() {
                return hdu;
            }
        }
        panic!("Extension not found!");
    }
}

impl<'s> IndexMut<&'s str> for Fits {
    /// Get mutable [`Hdu`] by `EXTNAME`.
    /// Panic if `EXTNAME` is not found.
    /// Prefer [`Fits::get_mut_by_name`] if you need to check.
    fn index_mut(&mut self, index: &str) -> &mut Self::Output {
        let value = Some(HeaderValue::CharacterString(String::from(index)));
        for hdu in self.iter_mut() {
            if hdu.value("EXTNAME") == value.as_ref() {
                return hdu;
            }
        }
        panic!("Extension not found!");
    }
}

///
impl IntoIterator for Fits {
    type Item = Hdu;
    type IntoIter = FitsIntoIter;
    fn into_iter(self) -> Self::IntoIter {
        FitsIntoIter {
            fits: self,
            position: 0,
        }
    }
}

trait MovableCursor {
    fn file(&self) -> MutexGuard<File>;
    fn position(&self) -> u64;

    fn tell(file_lock: &mut MutexGuard<File>) -> u64 {
        file_lock
            .seek(SeekFrom::Current(0))
            .expect("Could not get cursor position!")
    }

    fn set_position(&self) -> MutexGuard<File> {
        let position = self.position();
        let mut lock = self.file();
        lock.seek(SeekFrom::Start(position))
            .expect("Could not set position!");
        lock
    }
}

impl MovableCursor for FitsIntoIter {
    fn file(&self) -> MutexGuard<File> {
        self.fits.file.lock().expect("Get lock")
    }
    fn position(&self) -> u64 {
        self.position
    }
}

impl<'f> MovableCursor for FitsIter<'f> {
    fn file(&self) -> MutexGuard<File> {
        self.fits.file.lock().expect("Get lock")
    }
    fn position(&self) -> u64 {
        self.position
    }
}

impl<'f> MovableCursor for FitsIterMut<'f> {
    fn file(&self) -> MutexGuard<File> {
        self.fits.file.lock().expect("Get lock")
    }
    fn position(&self) -> u64 {
        self.position
    }
}

impl Iterator for FitsIntoIter {
    type Item = Hdu;
    fn next(&mut self) -> Option<Self::Item> {
        self.read_next_hdu().map(|(hdu, next_position)| {
            self.position = next_position;
            hdu
        })
    }
}

trait IterableOverHdu: MovableCursor {
    fn file_rc(&self) -> &FileRc;

    fn read_next_hdu(&self) -> Option<(Hdu, u64)> {
        let (header, data_start_position) = {
            // Get file lock
            let mut file_lock = self.set_position();
            let mut line = CardImage::new();
            let mut line_count = 0;
            let mut header = Vec::new();
            let mut end = false;
            while (line_count % 36) != 0 || !end {
                match file_lock.read_exact(&mut line.0) {
                    Ok(_) => {
                        if let Some((key, val)) = line.to_header_key_value() {
                            if key == "END" {
                                end = true;
                            }
                            header.push((key, val));
                        }
                    }
                    Err(_) => return None,
                };
                line_count += 1;
            }
            let data_start_position = Self::tell(&mut file_lock);
            (header, data_start_position)
        };
        // Lock released
        let hdu = Hdu {
            header,
            data_start: data_start_position,
            file: Some(self.file_rc().clone()),
            data: RwLock::new(None),
        };
        let len = hdu.data_byte_length().unwrap();
        let mut next_position = data_start_position + (len as u64);
        /* Compute next position to go to end of record */
        while (next_position % (36 * 80)) != 0 {
            next_position += 1;
        }
        Some((hdu, next_position))
    }
}

impl<'f> IterableOverHdu for FitsIter<'f> {
    fn file_rc(&self) -> &FileRc {
        &self.fits.file
    }
}

impl IterableOverHdu for FitsIntoIter {
    fn file_rc(&self) -> &FileRc {
        &self.fits.file
    }
}

impl<'f> IterableOverHdu for FitsIterMut<'f> {
    fn file_rc(&self) -> &FileRc {
        &self.fits.file
    }
}

impl<'f> Iterator for FitsIter<'f> {
    type Item = &'f Hdu;
    fn next(&mut self) -> Option<&'f Hdu> {
        if let Some(hdu_count) = *self.fits.total_hdu_count.read().unwrap() {
            if self.count >= hdu_count {
                return None;
            }
        }
        let hdu_guard = self.fits.hdus_guard();
        let hdus = unsafe { &mut *hdu_guard.load(Ordering::SeqCst) };
        if self.count < hdus.len() {
            self.count += 1;
            return Some(&hdus[self.count - 1]);
        }
        if let Some((hdu, next_position)) = self.read_next_hdu() {
            self.count += 1;
            self.position = next_position;
            hdus.push(hdu);
            hdus.last()
        } else {
            *self.fits.total_hdu_count.write().unwrap() = Some(self.count);
            None
        }
    }
}

impl<'f> Iterator for FitsIterMut<'f> {
    type Item = &'f mut Hdu;
    fn next(&mut self) -> Option<&'f mut Hdu> {
        if let Some(hdu_count) = *self.fits.total_hdu_count.read().unwrap() {
            if self.count >= hdu_count {
                return None;
            }
        }
        let hdu_guard = self.fits.hdus_guard();
        let hdus = unsafe { &mut *hdu_guard.load(Ordering::SeqCst) };
        if self.count < hdus.len() {
            self.count += 1;
            return Some(&mut hdus[self.count - 1]);
        }
        if let Some((hdu, next_position)) = self.read_next_hdu() {
            self.count += 1;
            self.position = next_position;
            hdus.push(hdu);
            hdus.last_mut()
        } else {
            *self.fits.total_hdu_count.write().unwrap() = Some(self.count);
            None
        }
    }
}

impl Hdu {
    /// Get [`HeaderValue`] by key. Return [`None`] if value is not found
    /// in [`Hdu`].
    pub fn value(&self, key: &str) -> Option<&HeaderValue> {
        for line in &self.header {
            if line.0 == key {
                return line
                    .1
                    .as_ref()
                    .and_then(|value_comment| value_comment.value.as_ref());
            }
        }
        None
    }

    fn value_as_integer_number(&self, key: &str) -> Option<i32> {
        self.value(key).and_then(|val| match *val {
            HeaderValue::IntegerNumber(n) => Some(n),
            _ => None,
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

    fn is_data_cached(&self) -> bool {
        self.data.read().unwrap().is_some()
    }

    fn data(&self) -> Option<&FitsData> {
        if let Some(ref data) = *self.data.read().unwrap() {
            let data = data as *const FitsData;
            Some(unsafe { &*data })
        } else {
            None
        }
    }

    /// Get data array stored in the [`Hdu`].
    pub fn read_data(&self) -> &FitsData {
        if self.is_data_cached() {
            self.data().unwrap()
        } else {
            self.read_data_force()
        }
    }

    fn read_data_force(&self) -> &FitsData {
        let bitpix = self
            .value_as_integer_number("BITPIX")
            .expect("BITPIX is present");
        let data = match bitpix {
            8 => FitsData::Characters(self.inner_read_data_force(|file, len| {
                let mut buf = vec![0u8; len];
                file.read_exact(&mut buf).expect("Read array");
                buf.into_iter().map(|n| n as char).collect()
            })),
            16 => {
                let blank = self.value_as_integer_number("BLANK");
                FitsData::IntegersI32(self.inner_read_data_force(|file, len| {
                    let mut buf = vec![0i16; len];
                    file.read_i16_into::<BigEndian>(&mut buf)
                        .expect("Read array");
                    if blank.is_some() {
                        let blank = blank.unwrap() as i16;
                        buf.into_iter()
                            .map(|n| if n == blank { None } else { Some(i32::from(n)) })
                            .collect()
                    } else {
                        buf.into_iter().map(|n| Some(i32::from(n))).collect()
                    }
                }))
            }
            32 => {
                let blank = self.value_as_integer_number("BLANK");
                FitsData::IntegersI32(self.inner_read_data_force(|file, len| {
                    let mut buf = vec![0i32; len];
                    file.read_i32_into::<BigEndian>(&mut buf)
                        .expect("Read array");
                    if blank.is_some() {
                        let blank = blank.unwrap();
                        buf.into_iter()
                            .map(|n| if n == blank { None } else { Some(n) })
                            .collect()
                    } else {
                        buf.into_iter().map(Some).collect()
                    }
                }))
            }
            -32 => FitsData::FloatingPoint32(self.inner_read_data_force(|file, len| {
                let mut buf = vec![0f32; len];
                file.read_f32_into::<BigEndian>(&mut buf)
                    .expect("Read array");
                buf
            })),
            -64 => FitsData::FloatingPoint64(self.inner_read_data_force(|file, len| {
                let mut buf = vec![0f64; len];
                file.read_f64_into::<BigEndian>(&mut buf)
                    .expect("Read array");
                buf
            })),
            _ => panic!("Unexpected value for BITPIX"),
        };
        let mut out = self.data.write().unwrap();
        *out = Some(data);
        // Release write-lock to be able to read and return back the data
        drop(out);
        self.data().unwrap()
    }

    fn inner_read_data_force<F, T>(&self, read: F) -> FitsDataArray<T>
    where
        F: Fn(&mut File, usize) -> Vec<T>,
    {
        let naxis = self.naxis().expect("Get NAXIS");
        let length = naxis.iter().product();
        let mut file_lock = self.file.as_ref().unwrap().lock().expect("Get file lock");
        file_lock
            .seek(SeekFrom::Start(self.data_start))
            .expect("Set data position");
        FitsDataArray::new(&naxis, read(&mut *file_lock, length))
    }
}

impl Hdu {
    pub fn new<T: FitsDataType>(shape: &[usize], data: Vec<T>) -> Hdu {
        let mut header = Vec::with_capacity(4 + shape.len());
        header.push((
            "SIMPLE".to_owned(),
            Some(HeaderValueComment {
                value: Some(HeaderValue::Logical(true)),
                comment: None,
            }),
        ));
        header.push((
            "BITPIX".to_owned(),
            Some(HeaderValueComment {
                value: Some(HeaderValue::IntegerNumber(T::bitpix() as i32)),
                comment: None,
            }),
        ));
        header.push((
            "NAXIS".to_owned(),
            Some(HeaderValueComment {
                value: Some(HeaderValue::IntegerNumber(shape.len() as i32)),
                comment: None,
            }),
        ));
        for (n, len) in shape.iter().enumerate() {
            let key = format!("NAXIS{}", n + 1);

            header.push((
                key,
                Some(HeaderValueComment {
                    value: Some(HeaderValue::IntegerNumber(*len as i32)),
                    comment: None,
                }),
            ));
        }

        header.push(("END".to_owned(), None));

        Hdu {
            header,
            data_start: 0,
            file: None,
            data: RwLock::new(Some(FitsDataType::new_fits_array(shape, data))),
        }
    }

    fn write(&mut self) -> Result<(), Error> {
        let mut file_lock = self.file.as_ref().unwrap().lock().expect("Get lock");
        file_lock.seek(SeekFrom::Start(self.data_start))?;

        for (key, value) in &self.header {
            file_lock.write_all(CardImage::from_header_key_value(key, value).raw())?;
        }
        let padding = 36 - (self.header.len() % 36);
        for _ in 0..padding {
            file_lock.write_all(CardImage::EMPTY.raw())?;
        }

        if let Some(data) = self.data() {
            let raw = data.raw();

            file_lock.write_all(&raw)?;

            const RECORD_SIZE: usize = 36 * 80;
            let padding = RECORD_SIZE - (raw.len() % RECORD_SIZE);
            let padding_data = vec![0u8; padding];
            file_lock.write_all(&padding_data)?;
        }
        Ok(())
    }
}
const EQUAL_U8: u8 = b'=';
const SPACE_U8: u8 = b' ';
const SLASH_U8: u8 = b'/';
const QUOTE_U8: u8 = b'\'';
const T_U8: u8 = b'T';
const F_U8: u8 = b'F';

impl HeaderValue {
    fn new(value: &[u8]) -> Option<HeaderValue> {
        HeaderValue::new_character_string(value)
            .or_else(|| HeaderValue::new_logical(value))
            .or_else(|| HeaderValue::new_integer(value))
            .or_else(|| HeaderValue::new_real_floating(value))
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
        from_utf8(value)
            .ok()
            .and_then(|string| {
                let trimmed = string.trim();
                i32::from_str_radix(trimmed, 10).ok()
            }).map(HeaderValue::IntegerNumber)
    }

    fn new_real_floating(value: &[u8]) -> Option<HeaderValue> {
        from_utf8(value)
            .ok()
            .and_then(|string| {
                let trimmed = string.trim();
                f64::from_str(trimmed).ok()
            }).map(HeaderValue::RealFloatingNumber)
    }
}

impl HeaderValue {
    fn raw(&self) -> Vec<u8> {
        let mut raw = Vec::with_capacity(32);
        match self {
            HeaderValue::CharacterString(string) => {
                raw.push(QUOTE_U8);
                for c in string.bytes() {
                    if c == QUOTE_U8 {
                        // Escape quote with another quote
                        raw.push(QUOTE_U8);
                        raw.push(QUOTE_U8);
                    } else {
                        raw.push(c);
                    }
                }
                raw.push(QUOTE_U8);
            }
            HeaderValue::Logical(b) => {
                for _ in 0..19 {
                    raw.push(SPACE_U8);
                }
                raw.push(if *b { T_U8 } else { F_U8 });
            }
            HeaderValue::IntegerNumber(n) => {
                for _ in 0..20 {
                    raw.push(SPACE_U8);
                }

                let mut byte_iter = n.to_string().into_bytes().into_iter();
                let mut i = 0;
                while let Some(c) = byte_iter.next_back() {
                    raw[19 - i] = c;
                    i += 1;
                }
            }
            HeaderValue::RealFloatingNumber(f) => {
                for _ in 0..20 {
                    raw.push(SPACE_U8);
                }

                // TODO: This formatter is not what we want
                // what we want: 1.313131E+01
                // what we have: 1.313131E1
                let mut byte_iter = format!("{:E}", f).into_bytes().into_iter();
                let mut i = 0;
                while let Some(c) = byte_iter.next_back() {
                    raw[19 - i] = c;
                    i += 1;
                }
            }
            HeaderValue::ComplexIntegerNumber(..) => {
                unimplemented!("ComplexIntegerNumber not implemented")
            }
            HeaderValue::ComplexFloatingNumber(..) => {
                unimplemented!("ComplexFloatingNumber not implemented")
            }
        }
        raw
    }
}

impl HeaderValueComment {
    fn new(value_comment: &[u8]) -> HeaderValueComment {
        let mut value_comment_iter = value_comment.split(|c| *c == SLASH_U8);
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

impl HeaderValueComment {
    fn raw(&self) -> [u8; 70] {
        let mut raw = [SPACE_U8; 70];
        let offset = if let Some(value) = &self.value {
            let value_raw = value.raw();
            let value_raw_len = value_raw.len();
            for (i, c) in value_raw.into_iter().enumerate() {
                raw[i] = c;
            }
            raw[value_raw_len + 1] = SLASH_U8;
            value_raw_len + 2
        } else {
            0
        };

        if let Some(comment) = &self.comment {
            for (i, c) in comment.bytes().enumerate() {
                raw[i + offset] = c;
            }
        };

        raw
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

impl CardImage {
    const EMPTY: CardImage = CardImage([SPACE_U8; 80]);

    fn raw(&self) -> &[u8] {
        &self.0
    }

    fn from_header_key_value(key: &str, value: &Option<HeaderValueComment>) -> CardImage {
        let mut raw = [SPACE_U8; 80];
        for (i, c) in key.bytes().enumerate().take(8) {
            raw[i] = c;
        }
        if let Some(value) = value {
            raw[8] = EQUAL_U8;
            for (i, c) in value.raw().into_iter().enumerate() {
                raw[10 + i] = *c;
            }
        }
        CardImage(raw)
    }
}

#[cfg(test)]
mod tests {
    use super::{CardImage, Fits, HeaderValue};

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
        assert_eq!(
            value_comment.value,
            Some(HeaderValue::CharacterString(String::from(
                "Malik Olivier Boussejra <malik@boussejra.com>",
            )))
        );
        assert_eq!(value_comment.comment, Some(String::from("")));
    }

    #[test]
    fn read_card_image_no_comment() {
        let card = CardImage::from("AUTHOR  = ''");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(
            value_comment.value,
            Some(HeaderValue::CharacterString(String::from("")))
        );
        assert_eq!(value_comment.comment, None);
    }

    #[test]
    fn read_card_image_character_trailing_space() {
        let card = CardImage::from("AUTHOR  = '  ab d  '");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(
            value_comment.value,
            Some(HeaderValue::CharacterString(String::from("  ab d")))
        );
    }

    #[test]
    fn read_card_image_character_blank() {
        let card = CardImage::from("AUTHOR  = '  '");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(
            value_comment.value,
            Some(HeaderValue::CharacterString(String::from(" ")))
        );
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
        let card =
            CardImage::from("EXPTIME =              13501.5 / Total exposure time (seconds)");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(
            value_comment.value,
            Some(HeaderValue::RealFloatingNumber(13501.5))
        );
    }

    #[test]
    fn read_card_image_character_real_exp() {
        let card = CardImage::from("CDELT1  =      -1.666667E-03 /");
        let header_key_value = card.to_header_key_value().unwrap();
        let value_comment = header_key_value.1.unwrap();
        assert_eq!(
            value_comment.value,
            Some(HeaderValue::RealFloatingNumber(-1.666667E-03))
        );
    }

    #[test]
    fn compute_hdu_data_byte_length() {
        let fits = Fits::open("tests/testprog.fit").unwrap();
        let mut iter = fits.into_iter();
        let primary_hdu = iter.next().unwrap();
        assert_eq!(primary_hdu.data_byte_length(), Some((32 / 8) * 10 * 2));
    }

    #[test]
    fn iterate_over_hdu() {
        let fits = Fits::open("tests/testprog.fit").unwrap();
        let mut iter = fits.into_iter();
        let primary_hdu = iter.next().unwrap();
        assert_eq!(primary_hdu.header[0].0, "SIMPLE");
        let hdu2 = iter.next().unwrap();
        assert_eq!(hdu2.header[0].0, "XTENSION");
        assert_eq!(
            hdu2.value("XTENSION").unwrap(),
            &HeaderValue::CharacterString(String::from("BINTABLE"))
        );
        let hdu3 = iter.next().unwrap();
        assert_eq!(hdu3.header[0].0, "XTENSION");
        assert_eq!(
            hdu3.value("XTENSION").unwrap(),
            &HeaderValue::CharacterString(String::from("IMAGE"))
        );
    }

    #[test]
    fn iterate_over_hdu_no_consume() {
        let fits = Fits::open("tests/testprog.fit").unwrap();
        let mut iter = fits.iter();
        let primary_hdu = iter.next().unwrap();
        assert_eq!(primary_hdu.header[0].0, "SIMPLE");
        let hdu2 = iter.next().unwrap();
        assert_eq!(hdu2.header[0].0, "XTENSION");
        assert_eq!(
            hdu2.value("XTENSION").unwrap(),
            &HeaderValue::CharacterString(String::from("BINTABLE"))
        );
        let hdu3 = iter.next().unwrap();
        assert_eq!(hdu3.header[0].0, "XTENSION");
        assert_eq!(
            hdu3.value("XTENSION").unwrap(),
            &HeaderValue::CharacterString(String::from("IMAGE"))
        );
    }

    #[test]
    fn iterate_over_hdu_mut() {
        let mut fits = Fits::open("tests/testprog.fit").unwrap();
        let mut iter = fits.iter_mut();
        let primary_hdu = iter.next().unwrap();
        assert_eq!(primary_hdu.header[0].0, "SIMPLE");
        let hdu2 = iter.next().unwrap();
        assert_eq!(hdu2.header[0].0, "XTENSION");
        assert_eq!(
            hdu2.value("XTENSION").unwrap(),
            &HeaderValue::CharacterString(String::from("BINTABLE"))
        );
        let hdu3 = iter.next().unwrap();
        assert_eq!(hdu3.header[0].0, "XTENSION");
        assert_eq!(
            hdu3.value("XTENSION").unwrap(),
            &HeaderValue::CharacterString(String::from("IMAGE"))
        );
    }

    #[test]
    fn iterate_over_hdu_mut_twice() {
        let mut fits = Fits::open("tests/testprog.fit").unwrap();
        {
            let mut iter = fits.iter_mut();
            let primary_hdu = iter.next().unwrap();
            assert_eq!(primary_hdu.header[0].0, "SIMPLE");
            let hdu2 = iter.next().unwrap();
            assert_eq!(hdu2.header[0].0, "XTENSION");
            assert_eq!(
                hdu2.value("XTENSION").unwrap(),
                &HeaderValue::CharacterString(String::from("BINTABLE"))
            );
            let hdu3 = iter.next().unwrap();
            assert_eq!(hdu3.header[0].0, "XTENSION");
            assert_eq!(
                hdu3.value("XTENSION").unwrap(),
                &HeaderValue::CharacterString(String::from("IMAGE"))
            );
        }
        {
            let mut iter = fits.iter_mut();
            let primary_hdu = iter.next().unwrap();
            assert_eq!(primary_hdu.header[0].0, "SIMPLE");
            let hdu2 = iter.next().unwrap();
            assert_eq!(hdu2.header[0].0, "XTENSION");
            assert_eq!(
                hdu2.value("XTENSION").unwrap(),
                &HeaderValue::CharacterString(String::from("BINTABLE"))
            );
            let hdu3 = iter.next().unwrap();
            assert_eq!(hdu3.header[0].0, "XTENSION");
            assert_eq!(
                hdu3.value("XTENSION").unwrap(),
                &HeaderValue::CharacterString(String::from("IMAGE"))
            );
        }
    }

    #[test]
    fn index_over_fits() {
        let fits = Fits::open("tests/testprog.fit").unwrap();
        let hdu2 = &fits[1];
        assert_eq!(hdu2.header[0].0, "XTENSION");
        assert_eq!(
            hdu2.value("XTENSION").unwrap(),
            &HeaderValue::CharacterString(String::from("BINTABLE"))
        );
    }

    #[test]
    #[should_panic]
    fn index_overflow_over_fits() {
        let fits = Fits::open("tests/testprog.fit").unwrap();
        let _hdu2 = &fits[10];
    }

    #[test]
    fn index_with_string_over_fits() {
        let fits = Fits::open("tests/testprog.fit").unwrap();
        let hdu2 = &fits["Test-ASCII"];
        assert_eq!(hdu2.header[0].0, "XTENSION");
        assert_eq!(
            hdu2.value("XTENSION").unwrap(),
            &HeaderValue::CharacterString(String::from("TABLE"))
        );
    }

    #[test]
    #[should_panic]
    fn index_with_string_not_found_over_fits() {
        let fits = Fits::open("tests/testprog.fit").unwrap();
        let _hdu2 = &fits["FOOBAR"];
    }

    #[test]
    fn header_value_character_string_to_raw() {
        let val = HeaderValue::CharacterString("Hey!".to_string());
        assert_eq!(val.raw(), b"'Hey!'");
    }

    #[test]
    fn header_value_escaped_character_string_to_raw() {
        let val = HeaderValue::CharacterString("'Hey!'".to_string());
        assert_eq!(val.raw(), b"'''Hey!'''");
    }

    #[test]
    fn header_value_logical_to_raw() {
        let val = HeaderValue::Logical(true);
        assert_eq!(val.raw(), b"                   T");
    }

    #[test]
    fn header_value_integer_number_to_raw() {
        let val = HeaderValue::IntegerNumber(12);
        assert_eq!(val.raw(), b"                  12");
    }

    #[test]
    #[should_panic]
    /// TODO: The implementation does not format floats as per the spec
    fn header_value_real_floating_number_to_raw() {
        let val = HeaderValue::RealFloatingNumber(15.1515151515152);
        println!(
            "{:?}",
            val.raw().into_iter().map(|c| c as char).collect::<String>()
        );
        assert_eq!(val.raw(), b"1.51515151515152E+01");
    }
}
