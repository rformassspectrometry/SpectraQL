library(testthat)
library(SpectraQL)
library(Spectra)
library(msdata)

fls <- dir(system.file("TripleTOF-SWATH", package = "msdata"),
           full.names = TRUE)
sps_dda <- Spectra(fls[1L])

test_check("SpectraQL")
