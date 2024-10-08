test_that("query,Spectra works", {
    expect_equal(query(sps_dda), sps_dda)
    expect_error(query(sps_dda, c("a", "b")), "single character")
    res <- query(sps_dda, "query *")
    expect_equal(res, sps_dda)

    ## TIC
    res <- query(sps_dda, "query scansum(ms1data)")
    expect_equal(res, ionCount(filterMsLevel(sps_dda, 1L)))

    expect_error(query(sps_dda, "query scansum(ms2da)"), "not supported")
    expect_error(query(sps_dda, "query ms2data where other"), "not supported")
    expect_error(query(sps_dda, "query ms2data ww"), "not supported")
    res <- query(sps_dda, "QUERY MS2DATA")
    expect_true(all(res$msLevel == 2))
})

test_that(".query_spectra works", {
    res <- .query_spectra(sps_dda, "QUERY * WHERE RTMIN = 200 AND RTMAX = 240")
    expect_true(all(rtime(res) > 200 & rtime(res) < 240))

    res <- .query_spectra(
        sps_dda, "QUERY * WHERE RTMIN = 200 AND RTMAX = 240 AND RTMAX = 230")
    expect_true(all(rtime(res) > 200 & rtime(res) < 230))

    res <- .query_spectra(
        sps_dda, "QUERY * WHERE SCANMIN = 9 AND SCANMAX = 400 AND RTMIN = 20")
    expect_true(all(rtime(res) > 20))
    expect_true(all(acquisitionNum(res) >= 9 & acquisitionNum(res) <= 400))

    expect_error(.query_spectra(sps_dda, "QUERY * WHERE MS2PREC"),
                 "Non-numeric")
    expect_error(.query_spectra(sps_dda, "QUERY * WHERE MS2PREC = a"),
                 "Non-numeric")

    ex_mz <- 304.1131
    mzr <- ex_mz + c(-1, 1) * ppm(ex_mz, 20)
    res <- .query_spectra(
        sps_dda, "QUERY * WHERE MS2PREC = 304.1131:TOLERANCEPPM=20")
    expect_equal(length(res), 6)

    expect_true(all(res$precursorMz > mzr[1] & res$precursorMz < mzr[2]))

    res <- .query_spectra(
        sps_dda, "QUERY * WHERE RTMIN=420 AND MS2PREC=304.1131:TOLERANCEPPM=20")
    expect_equal(length(res), 3)

    res <- .query_spectra(sps_dda, "QUERY * WHERE CHARGE = 0")
    expect_true(all(precursorCharge(res) == 0))

    res <- .query_spectra(sps_dda, "QUERY * WHERE CHARGE = NA OR -1")
    expect_true(all(is.na(precursorCharge(res)) | precursorCharge(res) == -1))

    res <- .query_spectra(sps_dda, "QUERY * WHERE POLARITY = Positive")
    expect_true(all(polarity(res) == 1))

    res <- .query_spectra(
        sps_dda, "QUERY * WHERE MS2PROD=(100 OR 104):TOLERANCEPPM=5")
    expect_true(all(containsMz(res, mz = 100, ppm = 5) |
                    containsMz(res, mz = 104, ppm = 5)))

    res <- .query_spectra(
        sps_dda, "QUERY * WHERE ms2prec = (99 or 90 or 89):tolerancemz=0.6")
    expect_true(all(precursorMz(res) > 89))
    expect_true(all(precursorMz(res) < 100))

    res <- .query_spectra(
        sps_dda, "QUERY * WHERE MS2NL=100:TOLERANCEPPM=5")
##     #expect_true(containsNeutralLoss(res, neutralLoss = 100, ppm = 5))
##     #res ha 0 spectra and containsNeutralLoss(res, neutralLoss = 100, ppm = 5)
##     # throws the error : Errore in value[[1L]] : subscript fuori limite. Is this
##     # ok or should containsNeutralLoss return a logical value?

})

test_that(".query_what works", {
    res <- .query_what(sps_dda, "QUERY * ")
    expect_equal(res, sps_dda)

    res <- .query_what(sps_dda, "QUERY MS2DATA")
    expect_true(all(res$msLevel == 2))

    expect_error(.query_what(sps_dda, "QUERY other WHERE RTIME = 300"),
                 "not supported")
})
