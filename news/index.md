# Changelog

## SpectraQL 1.5

### Changes in 1.5.1

- Load test and example files from *MsDataHub* dropping the dependency
  from *msdata*.

## SpectraQL 1.3

### Changes in 1.3.1

- Add citation.

## SpectraQL 0.99

### Changes in 0.99.2

- Add man/figures folder to Rbuildignore.

### Changes in 0.99.1

- Add S4Vectors package to Suggests as it’s used in the unit tests.

### Changes in 0.99.0

- Prepare and clean package for Bioconductor submission.

## SpectraQL 0.98

### Changes in 0.98.6

- Update README, DESCRIPTION.

### Changes in 0.98.4

- Add support for `"FILTER MS1MZ"` and `"FILTER MS2MZ"`.

### Changes in 0.98.3

- Fix `MS1DATA` and `MS2DATA` to return only peaks data.
- Add support for functions (`scansum`, `scaninfo`).
- Fix parsing of where part.

### Changes in 0.98.2

- Change from `filterPrecursorMz` to `filterPrecursorMzValues`.
- Support `"OR"` in `"MS2PREC"`, `MS1MZ`, `MS2PROD`.
- Add `MS1MZ`.

### Changes in 0.98.1

- Add functionality to extract the requested data from a `Spectra`.
- Add vignette with first use cases.
