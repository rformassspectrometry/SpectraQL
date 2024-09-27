test_that(".what works", {
    res <- .what("notworking this does")
    expect_equal(res, NA_character_)

    res <- .what("QUERY  somedata WHERE a < 10")
    expect_equal(res, "somedata")

    res <- .what("query somedata someotherdata  filter adfkladf")
    expect_equal(res, "somedata someotherdata")

    res <- .what("query *  ")
    expect_equal(res, "*")

    res <- .what("QUERY * WHERE RTMIN = 200 AND RTMAX = 300")
    expect_equal(res, "*")
})

test_that(".validate_what works", {
    expect_error(.validate_what(NA), "unable")
})

test_that(".parse_query works", {
    expect_equal(.parse_query(""), character())
    expect_equal(.parse_query("", split = FALSE), NA_character_)

    expect_equal(.parse_query("get some words in between"), character())
    expect_equal(.parse_query("get some words in between", split = FALSE),
                 NA_character_)
    expect_equal(.parse_query("get some words in between", "om", "in"),
                 "e words")
    expect_equal(.parse_query("get some words in between", "om", "in", FALSE),
                 "e words")
})

test_that(".where works", {
    res <- .where("query bla where a < 5")
    expect_equal(res, "a < 5")
    res <- .where("query bla where a < 5 and b > 5 and ")
    expect_equal(res, c("a < 5", "b > 5"))
    res <- .where("query bla where a < 5 and b > 5 filter fdf")
    expect_equal(res, c("a < 5", "b > 5"))
    res <- .where("query all")
    expect_equal(res, character())
})

test_that(".parse_where works", {
    expect_equal(.parse_where(NULL), character())
    res <- .parse_where("RTMIN = 123")
    expect_equal(res, c(RTMIN = "123"))
    res <- .parse_where("rtmin = 123")
    expect_equal(res, c(RTMIN = "123"))
    res <- .parse_where("RTMAX")
    expect_equal(res, c(RTMAX = NA_character_))
    res <- .parse_where("MS2PROD=312:TOLERANCEMZ= 0.1 :TOLERANCEMZ=3")
    expect_equal(res, c(MS2PROD = "312", TOLERANCEMZ = "0.1",
                        TOLERANCEMZ = "3"))
})

test_that(".group_min_max works", {
    x <- list(RTMIN = c(RTMIN = 213), RTMAX = c(RTMAX = 343), OTHER =  4,
              RTMAX = c(RTMAX = 4))
    res <- .group_min_max(x, name = "RT")
    expect_equal(res, list(RT = c(RTMIN = 213, RTMAX = 343), RT = c(RTMAX = 4),
                           OTHER = 4))

    x <- list(A = 4, B = 5)
    res <- .group_min_max(x)
    expect_equal(x, res)

    x <- list(B = 1, BMAX = 2, AMIN = 3, C = 4, BMAX = 5, BMIN = 6)
    res <- .group_min_max(x, name = "A")
    expect_equal(res, list(A = 3, B = 1, BMAX = 2, C = 4, BMAX = 5, BMIN = 6))

    res <- .group_min_max(x, name = "B")
    expect_equal(res, list(B = c(6, 2), B = 5, B = 1, AMIN = 3, C = 4))
})

test_that(".translate_condition_rt works", {
    res <- .translate_condition_rt(4)
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterRt)
    expect_equal(res@ARGS, list(rt = c(-Inf, Inf)))

    res <- .translate_condition_rt(RT = c(RTMAX = "124"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterRt)
    expect_equal(res@ARGS, list(rt = c(-Inf, 124)))

    res <- .translate_condition_rt(RT = c(RTMAX = "124", RTMIN = "23"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterRt)
    expect_equal(res@ARGS, list(rt = c(23, 124)))

    expect_error(.translate_condition_rt(RT = c(RTMAX = "b")), "Non-numeric")
})

test_that(".translate_condition_scan works", {
    res <- .translate_condition_scan(4)
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(scan = c(-Inf, Inf)))

    res <- .translate_condition_scan(SCAN = c(SCANMAX = "124"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(scan = c(-Inf, 124)))

    res <- .translate_condition_scan(SCAN = c(SCANMAX = "124", SCANMIN = "23"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(scan = c(23, 124)))

    expect_error(.translate_condition_scan(SCAN = c(SCANMAX = "b")), "Non-numeric")
})

test_that(".query_to_filters works", {
    q <- "QUERY * WHERE RTMIN = 123 AND RTMAX = 130 AND RTMIN = 129"
    res <- .query_to_filters(.where(q))
    expect_true(length(res) == 2)
    expect_true(all(vapply(res, inherits, what = "ProcessingStep", logical(1))))
    expect_equal(res[[1L]]@FUN, filterRt)
    expect_equal(res[[1L]]@ARGS, list(rt = c(123, 130)))
    expect_equal(res[[2L]]@FUN, filterRt)
    expect_equal(res[[2L]]@ARGS, list(rt = c(129, Inf)))

    q <- "QUERY * WHERE RTMIN = 123 AND RTMAX = 130 AND OTHER = 120"
    expect_error(.query_to_filters(.where(q)), "not supported")

    q <- "QUERY * WHERE RTMIN > 123"
    expect_error(.query_to_filters(.where(q)), "not supported")

    q <- "QUERY * WHERE RTMIN = 123 AND RTMAX = 12 FILTER MS1MZ =4"
    res <- .query_to_filters(.filter(q), MAP = .FILTER_FUNCTIONS)
    expect_length(res, 1)
    expect_equal(res[[1L]]@FUN, filterMzValues)
    expect_equal(res[[1L]]@ARGS, list(mz = 4, tolerance = 0,
                                      ppm = 0, msLevel. = 1L))

    q <- "QUERY * FILTER other = 3"
    expect_error(.query_to_filters(.filter(q), MAP = .FILTER_FUNCTIONS,
                                   label = "Filter"), "Filter 'OTHER'")
    q <- "QUERY * FILTER MS2MZ < 3"
    expect_error(.query_to_filters(.filter(q), MAP = .FILTER_FUNCTIONS,
                                   label = "Filter"), "Filter 'MS2MZ < 3'")
})

test_that(".translate_condition_charge works", {
    res <- .translate_condition_charge(4)
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(z = integer(0)))

    res <- .translate_condition_charge(CHARGE = c(CHARGE = -1))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(z = -1))

    res <- .translate_condition_charge(CHARGE = c(CHARGE = "(-1 OR 0 OR NA)"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(z = c(-1, 0, NA)))

    expect_error(.translate_condition_charge(CHARGE = c(CHARGE = "b")),
                 "Non-integer")
})

test_that(".translate_condition_polarity works", {
    res <- .translate_condition_polarity(4)
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(polarity = integer(0)))

    res <- .translate_condition_polarity(POLARITY = c(POLARITY = "Positive"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(polarity = 1L))

    res <- .translate_condition_polarity(POLARITY = c(POLARITY = "Positive OR NA"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(polarity = c(1L, -1)))

    expect_error(.translate_condition_polarity(POLARITY = c(POLARITY = "b")),
                 "Invalid value")

})

test_that(".translate_condition_peak_mz works", {
    res <- .translate_condition_peak_mz(4)
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, identity)
    expect_equal(res@ARGS, list())

    res <- .translate_condition_peak_mz(MS2PROD = c(MS2PROD = 123))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(mz = 123, tolerance = 0, ppm = 0, msLevel = 2L))

    res <- .translate_condition_peak_mz(
        MS2PROD = c(MS2PROD = 123, TOLERANCEMZ = 2, TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(mz = 123, tolerance = 2, ppm = 10, msLevel =2L))

    res <- .translate_condition_peak_mz(
        MS2PROD = c(MS2PROD = "(123 OR 125)", TOLERANCEMZ = 2,
                    TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(mz = c(123, 125), tolerance = 2,
                                ppm = 10, msLevel = 2L))

    expect_error(.translate_condition_peak_mz(MS2PROD = c(MS2PROD = "b")),
                 "non-numeric")

    ## MS1MZ
    res <- .translate_condition_ms1mz(MS1MZ = c(MS1MZ = 123, TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(mz = 123, tolerance = 0, ppm = 10, msLevel =1L))

    ## MS2PROD
    res <- .translate_condition_ms2prod(MS2PROD = c(MS2PROD = 123,
                                                 TOLERANCEPPM = 13))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(mz = 123, tolerance = 0, ppm = 13, msLevel =2L))
})

test_that(".translate_condition_ms2prec works", {
    res <- .translate_condition_ms2prec(4)
    expect_true(inherits(res, "ProcessingStep"))
    res <- .translate_condition_ms2prec(MS2PREC = c(MS2PREC = 123))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterPrecursorMzValues)
    expect_equal(res@ARGS, list(mz = 123, ppm = 0, tolerance = 0))

    res <- .translate_condition_ms2prec(MS2PREC = c(MS2PREC = 123, TOLERANCEMZ = 2))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterPrecursorMzValues)
    expect_equal(res@ARGS, list(mz = 123, ppm = 0, tolerance = 2))

    res <- .translate_condition_ms2prec(
        MS2PREC = c(MS2PREC = 123, TOLERANCEMZ = 2, TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterPrecursorMzValues)
    expect_equal(res@ARGS, list(mz = 123, ppm = 10, tolerance = 2))

    expect_error(.translate_condition_ms2prec(MS2PREC = c(MS2PREC = "b")),
                 "non-numeric")
    ## Or
    res <- .translate_condition_ms2prec(
        MS2PREC = c(MS2PREC = "(123 OR 125 or 129)", TOLERANCEMZ = 2,
                    TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(mz = c(123, 125, 129), ppm = 10, tolerance = 2))
})

test_that(".extract_what works", {
    df <- S4Vectors::DataFrame(msLevel = c(1L, 2L), rtime = c(2, 3.2))
    sps_tmp <- Spectra::Spectra(df)

    expect_error(.extract_what(sps_tmp, "what"), "not supported")
    res <- .extract_what(sps_tmp, "*")
    expect_equal(sps_tmp, res)
})

test_that(".what_data works", {
    ## *
    res <- .what_data(sps_dda, "*")
    expect_s4_class(res, "Spectra")
    expect_equal(length(res), length(sps_dda))

    res <- .what_data(sps_dda, "scaninfo(*)")
    expect_s4_class(res, "Spectra")
    expect_equal(length(res), length(sps_dda))

    expect_error(.what_data(sps_dda, "scaninfo(*"), "not supported")
    expect_error(.what_data(sps_dda, "other"), "not supported")

    res <- .what_data(sps_dda, "ms1data")
    expect_s4_class(res, "Spectra")
    expect_true(all(msLevel(res) == 1L))

    res <- .what_data(sps_dda, "other(ms1data)")
    expect_s4_class(res, "Spectra")
    expect_true(all(msLevel(res) == 1L))

    expect_error(.what_data(sps_dda, "ms1data)"), "not supported")

    res <- .what_data(sps_dda, "ms2data")
    expect_s4_class(res, "Spectra")
    expect_true(all(msLevel(res) == 2L))

    res <- .what_data(sps_dda, "other(ms2data)")
    expect_s4_class(res, "Spectra")
    expect_true(all(msLevel(res) == 2L))

    expect_error(.what_data(sps_dda, "ms2datas"), "not supported")
})

test_that(".what_extract works", {
    s <- sps_dda[1:4]

    res <- .what_extract(s, "MS1DATA")
    expect_true(length(res) == 4)
    expect_true(is.matrix(res[[1L]]))

    res <- .what_extract(s, "*")
    expect_s4_class(res, "Spectra")

    res <- .what_extract(s, " scansum( MS1DATA")
    expect_true(is.numeric(res))
    expect_equal(res, ionCount(s))

    res <- .what_extract(s, "scaninfo(MS2DATA)")
    expect_s4_class(res, "DataFrame")
    expect_true(nrow(res) == 4)

    expect_error(.what_extract(s, "scannum(MS1DATA"), "'scannum' not supported")
})


## query <- "QUERY * WHERE RTMIN = 123 AND RTMAX = 130  AND MZPREC = 312.2:TOLERANCEMZ = 0.1:TOLERANCEPPM=10"
## x <- .where(query)
## res <- lapply(x, .parse_where)
## names(res) <- vapply(res, function(z) names(z)[1], character(1))
## res <- .group_min_max(res, name = "RT")
## res <- .group_min_max(res, name = "SCAN")

test_that(".translate_condition_ms2nl works", {
    res <- .translate_condition_ms2nl(4)
    expect_true(inherits(res, "ProcessingStep"))
    res <- .translate_condition_ms2nl(MS2NL = c(MS2NL = 123))
    expect_true(inherits(res, "ProcessingStep"))
    #expect_equal(res@FUN, containsNeutralLoss)
    expect_equal(res@ARGS, list(neutralLoss = 123, tolerance = 0, ppm = 0))

    res <- .translate_condition_ms2nl(
        MS2NL = c(MS2NL = 123, TOLERANCEMZ = 2, TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    #expect_equal(res@FUN, containsNeutralLoss)
    expect_equal(res@ARGS, list(neutralLoss = 123, tolerance = 2, ppm = 10))

    expect_error(.translate_condition_ms2nl(MS2NL = c(MS2NL = "(123 OR 125)")),
                 "OR not yet supported")

    expect_error(.translate_condition_ms2nl(MS2NL = c(MS2NL = "b")),
                 "Non-numeric")
})

test_that(".parse_or works", {
    res <- .parse_or("ab")
    expect_equal(res, "ab")
    res <- .parse_or("ab OR   cd")
    expect_equal(res, c("ab", "cd"))
    res <- .parse_or("ab OR cd OR ef")
    expect_equal(res, c("ab", "cd", "ef"))
    res <- .parse_or("(ab OR  cd )")
    expect_equal(res, c("ab", "cd"))
    res <- .parse_or("ab or cd OR ef")
    expect_equal(res, c("ab", "cd", "ef"))
})

test_that(".filter works", {
    res <- .filter("QUERY MS1DATA FILTER MS1MZ=123 AND MS2MZ=32")
    expect_length(res, 2)
    expect_equal(res[1L], "MS1MZ=123")
    expect_equal(res[2L], "MS2MZ=32")

    res <- .filter("QUERY MS1DATA")
    expect_equal(res, character())
})

test_that(".translate_filter_mz_value works", {
    res <- .translate_filter_mz_value(4)
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, identity)
    expect_equal(res@ARGS, list())

    res <- .translate_filter_mz_value(MS2MZ = c(MS2MZ = 123))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterMzValues)
    expect_equal(res@ARGS, list(mz = 123, tolerance = 0, ppm = 0, msLevel. = 2L))

    res <- .translate_filter_mz_value(
        MS2MZ = c(MS2MZ = 123, TOLERANCEMZ = 2, TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterMzValues)
    expect_equal(res@ARGS, list(mz = 123, tolerance = 2,
                                ppm = 10, msLevel. = 2L))

    res <- .translate_filter_mz_value(
        MS2MZ = c(MS2MZ = "(123 OR 125)", TOLERANCEMZ = 2,
                    TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterMzValues)
    expect_equal(res@ARGS, list(mz = c(123, 125), tolerance = 2,
                                ppm = 10, msLevel. = 2L))

    expect_error(.translate_filter_mz_value(MS2MZ = c(MS2MZ = "b")),
                 "non-numeric")

    ## MS1MZ
    res <- .translate_filter_ms1mz(MS1MZ = c(MS1MZ = 123, TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterMzValues)
    expect_equal(res@ARGS, list(mz = 123, tolerance = 0,
                                ppm = 10, msLevel. = 1L))

    ## MS2MZ
    res <- .translate_filter_ms2mz(MS2MZ = c(MS2MZ = 123, TOLERANCEPPM = 13))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterMzValues)
    expect_equal(res@ARGS, list(mz = 123, tolerance = 0,
                                ppm = 13, msLevel. = 2L))
})
