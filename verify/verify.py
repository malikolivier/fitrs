#!/usr/bin/env python3

from astropy.io import fits

fits_filepath = "../test/testprog.fit"

hdus = fits.open(fits_filepath)

hdus.verify('exception')
