use fits::{Hdu, HeaderValue};

const MAX_DIM: usize = 3;

#[derive(Copy, Clone, PartialEq, Debug, Default)]
pub struct WCS {
    coefs: [CoordCoefs; MAX_DIM],
    size: usize,
}

#[derive(Copy, Clone, PartialEq, Debug, Default)]
struct CoordCoefs {
    crpix: f32,
    crval: f32,
    cds: [f32; MAX_DIM],
}

impl WCS {
    pub fn new(hdu: &Hdu) -> Self {
        fn get_cds(hdu: &Hdu, n: usize) -> [f32; MAX_DIM] {
            let mut cds = [0.0; MAX_DIM];
            for (m, cd) in cds.iter_mut().enumerate() {
                let cd_key = format!("CD{}_{}", n + 1, m + 1);
                *cd = if let Some(HeaderValue::RealFloatingNumber(cd)) = hdu.value(&cd_key) {
                    *cd as f32
                } else if n == m {
                    1.0
                } else {
                    0.0
                }
            }
            cds
        }
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
            cds: [0.0; MAX_DIM],
        }; MAX_DIM];
        for (n, coef) in coefs.iter_mut().enumerate() {
            let crpix_key = format!("CRPIX{}", n + 1);
            let crval_key = format!("CRVAL{}", n + 1);
            if let (Some(crpix), Some(crval)) =
                (get_number(hdu, &crpix_key), get_number(hdu, &crval_key))
            {
                // FITS are 1-indexed, so correct offset so that it becomes 0-indexed.
                coef.crpix = crpix - 1.0;
                coef.crval = crval;
                size += 1;
            } else {
                break;
            }
            coef.cds = get_cds(hdu, n);
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
            *p = coef.crval + (*p - coef.crpix) * coef.cds[i];
        }
        pixel
    }
}
