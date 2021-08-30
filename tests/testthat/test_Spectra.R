test_that("query,Spectra works", {
    expect_error(query(sps_dda), "single character")
    expect_error(query(sps_dda, c("a", "b")), "single character")
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


    res <- .query_spectra(sps_dda, "QUERY * WHERE MS2PREC")
    expect_true(length(res) == 0)

    ex_mz <- 304.1131
    mzr <- ex_mz + c(-1, 1) * ppm(ex_mz, 20)
    res <- .query_spectra(
        sps_dda, "QUERY * WHERE MS2PREC = 304.1131:TOLERANCEPPM=20")
    expect_equal(length(res), 6)

    expect_true(all(res$precursorMz > mzr[1] & res$precursorMz < mzr[2]))

    res <- .query_spectra(
        sps_dda, "QUERY * WHERE RTMIN=420 AND MS2PREC=304.1131:TOLERANCEPPM=20")
    expect_equal(length(res), 3)
})
