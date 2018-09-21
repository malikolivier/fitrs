#!/usr/bin/env python3

import argparse
from astropy.io import fits

parser = argparse.ArgumentParser(description="FITS file verifier")
parser.add_argument('file', type=argparse.FileType('r'),
    help='FITS file to check')

args = parser.parse_args()

fits_filepath = args.file.name
args.file.close()

hdus = fits.open(fits_filepath)

hdus.verify('exception')
