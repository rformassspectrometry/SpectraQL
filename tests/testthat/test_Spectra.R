test_that(".query_spectra works", {
    expect_error(.query_spectra(sps_dda), "single character")
    expect_error(.query_spectra(sps_dda, c("a", "b")), "single character")
    res <- .query_spectra(sps_dda, "QUERY * WHERE RTMIN = 200 AND RTMAX = 240")
    expect_true(all(rtime(res) > 200 & rtime(res) < 240))
    
    res <- .query_spectra(
        sps_dda, "QUERY * WHERE RTMIN = 200 AND RTMAX = 240 AND RTMAX = 230")
    expect_true(all(rtime(res) > 200 & rtime(res) < 230))

    res <- .query_spectra(
        sps_dda, "QUERY * WHERE SCANMIN = 9 AND SCANMAX = 400 AND RTMIN = 20")
    expect_true(all(rtime(res) > 20))
    expect_true(all(acquisitionNum(res) >= 9 & acquisitionNum(res) <= 400))
})
