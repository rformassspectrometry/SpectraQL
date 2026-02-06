library(testthat)
library(SpectraQL)
library(Spectra)
library(MsDataHub)

fls <- MsDataHub::PestMix1_DDA.mzML()
sps_dda <- Spectra(fls)

test_check("SpectraQL")
