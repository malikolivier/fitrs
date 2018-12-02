use fits::{Hdu, HeaderValue};

const MAX_DIM: usize = 4;

/// World Coordinates
///
/// The implementation of this structure is *experimental*.
#[derive(Copy, Clone, PartialEq, Debug, Default)]
pub struct WCS {
    coefs: [CoordCoefs; MAX_DIM],
    size: usize,
}

#[derive(Copy, Clone, PartialEq, Debug, Default)]
struct CoordCoefs {
    crpix: f32,
    crval: f32,
    cd: f32,
}

impl WCS {
    /// Read how to convert pixel to real world coordinates from HDU header.
    pub fn new(hdu: &Hdu) -> Self {
        fn get_number(hdu: &Hdu, key: &str) -> Option<f32> {
            match hdu.value(key) {
                Some(HeaderValue::RealFloatingNumber(f)) => Some(*f as f32),
                Some(HeaderValue::IntegerNumber(n)) => Some(*n as f32),
                _ => None,
            }
        }

        let mut size = 0;
        let mut coefs = [CoordCoefs {
            crpix: 0.0,
            crval: 0.0,
            cd: 0.0,
        }; MAX_DIM];
        for (n, coef) in coefs.iter_mut().enumerate() {
            let crpix_key = format!("CRPIX{}", n + 1);
            let crval_key = format!("CRVAL{}", n + 1);
            let cd_key = format!("CD{}_{}", n + 1, n + 1);
            if let (Some(crpix), Some(crval), Some(cd)) = (
                get_number(hdu, &crpix_key),
                get_number(hdu, &crval_key),
                get_number(hdu, &cd_key),
            ) {
                // FITS are 1-indexed, so correct offset so that it becomes 0-indexed.
                coef.crpix = crpix - 1.0;
                coef.crval = crval;
                coef.cd = cd;
                size += 1;
            } else {
                break;
            }
        }
        Self { coefs, size }
    }

    /// Convert from pixel to world coordinates
    ///
    /// Currently only works with CDn_m values and assume there is no rotation
    /// in coordinates conversions.
    pub fn pix2world(&self, mut pixel: [f32; MAX_DIM]) -> [f32; MAX_DIM] {
        for (i, p) in pixel.iter_mut().enumerate().take(self.size) {
            let coef = &self.coefs[i];
            *p = coef.crval + (*p - coef.crpix) * coef.cd;
        }
        pixel
    }

    pub fn slice(self, indices: &[usize]) -> Self {
        let mut out = self;
        out.size = indices.len();
        for (i, idx) in indices.iter().enumerate() {
            out.coefs[i] = self.coefs[*idx];
        }
        out
    }

    pub fn transform(self, index: usize, pix: f32, factor: f32) -> Self {
        let mut out = self;
        {
            let coef = &mut out.coefs[index];
            let orig_coef = &self.coefs[index];
            coef.crpix = 0.0;
            coef.crval = orig_coef.crval + (pix - orig_coef.crpix) * orig_coef.cd;
            coef.cd = orig_coef.cd * factor;
        }
        out
    }
}
