# Changelog

## [Unreleased]

### Added

- Add support for CDELTn keyword for WCS

## [v0.4.1] - 2018-12-02

### Changed

- 4-dimensional FITS support for WCS

## [v0.4.0] - 2018-11-19

### Added
- Writing FITS file:
  - Can write custom values to header.
  - Support for writing continued strings.
  - Can write Hdu with no data array.

### Fixed
- Check for overflow on writing card image. Will truncate comments if they
  cannot fit in the 80-char-long card.
- Do not pad FITS header or data array when it was already just the right size.

## [v0.3.2] - 2018-11-16

### Fixed
- Link to latest documentation.

## [v0.3.1] - 2018-11-14

### Fixed
- Correctly format floating points header values according to the FITS
  standard.

## [v0.3.0] - 2018-11-13

### Added
- Read continued strings according to the FITS standard.

## [v0.2.6] - 2018-11-09

### Added
- Implement `Iterator` for `Hdu`. Can iterate over header values.
- Implement `IntoIterator` for `&Fits`, for convenience.

## [v0.2.5] - 2018-11-01

### Added
- Experimental, simplistic WCS support.

## [v0.2.4] - 2018-10-25

### Fixed
- Correctly read comment for some edge cases.

## [v0.2.3] - 2018-10-24

### Fixed
- Correctly read character string containing a "/" character.

## [v0.2.2] - 2018-09-23

### Fixed
- Correct BITPIX value on writing a FITS file with a floating point data array.

## [v0.2.1] - 2018-09-22

### Fixed
- Typo in documentation.

## [v0.2.0] - 2018-09-22

### Added
- Write multi-HDU FITS files.

## [v0.1.2] - 2018-07-03

### Changed
- Fits::open can take `AsRef<Path>` as argument.

## [v0.1.1] - 2018-04-10

- Improve doc.

## [v0.1.0] - 2018-04-10

- Very first release.
- Can read multi-header FITS files.
