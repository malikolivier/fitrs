# FITRS

**DISCLAIMER:** This lib is experimental and unstable.
API changes will most probably occur.

Read FITS file for astronomical use. Not all the features of the FITS standard
are supported, but the basic features are there.

The objective is to make a lib to manage FITS files in pure rust, with as few
dependencies as possible, intelligent cache and multi-threading in mind.

## Documentation

You can refer to the [online doc](http://boussejra.com/rust-doc/fitrs/).

See the tests and benchmarks in the repository for more examples how to use.

## TODO

- Being able to write FITS file.
- Iterate over values in HDU header.
