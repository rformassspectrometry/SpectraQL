test_that(".what works", {
    res <- .what("notworking this does")
    expect_equal(res, NA_character_)

    res <- .what("QUERY  somedata WHERE a < 10")
    expect_equal(res, "somedata")

    res <- .what("query somedata someotherdata  filter adfkladf")
    expect_equal(res, "somedata someotherdata")

    res <- .what("query *  ")
    expect_equal(res, "*")
})

test_that(".validate_what works", {
    expect_error(.validate_what(NA), "unable")
})

test_that(".where works", {
    res <- .where("query bla where a < 5")
    expect_equal(res, "a < 5")
    res <- .where("query bla where a < 5 and b > 5 and ")
    expect_equal(res, c("a < 5", "b > 5"))
    res <- .where("query bla where a < 5 and b > 5 filter fdf")
    expect_equal(res, c("a < 5", "b > 5"))
    res <- .where("query all")
    expect_equal(res, NA_character_)
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
    expect_equal(res, c(MS2PROD = "312", TOLERANCEMZ = "0.1", TOLERANCEMZ = "3"))
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

test_that(".translate_filter_rt works", {
    res <- .translate_filter_rt(4)
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterRt)
    expect_equal(res@ARGS, list(rt = c(-Inf, Inf)))

    res <- .translate_filter_rt(RT = c(RTMAX = "124"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterRt)
    expect_equal(res@ARGS, list(rt = c(-Inf, 124)))

    res <- .translate_filter_rt(RT = c(RTMAX = "124", RTMIN = "23"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterRt)
    expect_equal(res@ARGS, list(rt = c(23, 124)))

    expect_error(.translate_filter_rt(RT = c(RTMAX = "b")), "Non-numeric")
})

test_that(".translate_filter_scan works", {
    res <- .translate_filter_scan(4)
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(scan = c(-Inf, Inf)))

    res <- .translate_filter_scan(SCAN = c(SCANMAX = "124"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(scan = c(-Inf, 124)))

    res <- .translate_filter_scan(SCAN = c(SCANMAX = "124", SCANMIN = "23"))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@ARGS, list(scan = c(23, 124)))

    expect_error(.translate_filter_scan(SCAN = c(SCANMAX = "b")), "Non-numeric")
})

test_that(".query_to_filter works", {
    q <- "QUERY * WHERE RTMIN = 123 AND RTMAX = 130 AND RTMIN = 129"
    res <- .query_to_filters(q)
    expect_true(length(res) == 2)
    expect_true(all(vapply(res, inherits, what = "ProcessingStep", logical(1))))
    expect_equal(res[[1L]]@FUN, filterRt)
    expect_equal(res[[1L]]@ARGS, list(rt = c(123, 130)))
    expect_equal(res[[2L]]@FUN, filterRt)
    expect_equal(res[[2L]]@ARGS, list(rt = c(129, Inf)))

    q <- "QUERY * WHERE RTMIN = 123 AND RTMAX = 130 AND OTHER = 120"
    expect_error(.query_to_filters(q), "not supported")

    q <- "QUERY * WHERE RTMIN > 123"
    expect_error(.query_to_filters(q), "not supported")
})

test_that(".translate_filter_ms2prec works", {
    res <- .translate_filter_ms2prec(4)
    expect_true(inherits(res, "ProcessingStep"))
    res <- .translate_filter_ms2prec(MS2PREC = c(MS2PREC = 123))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterPrecursorMz)
    expect_equal(res@ARGS, list(mz = c(123, 123)))

    res <- .translate_filter_ms2prec(MS2PREC = c(MS2PREC = 123, TOLERANCEMZ = 2))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterPrecursorMz)
    expect_equal(res@ARGS, list(mz = c(121, 125)))

    res <- .translate_filter_ms2prec(
        MS2PREC = c(MS2PREC = 123, TOLERANCEMZ = 2, TOLERANCEPPM = 10))
    expect_true(inherits(res, "ProcessingStep"))
    expect_equal(res@FUN, filterPrecursorMz)
    expect_equal(
        res@ARGS, list(mz = c(123 - 2 - ppm(123, 10), 123 + 2 + ppm(123, 10))))
})

## query <- "QUERY * WHERE RTMIN = 123 AND RTMAX = 130  AND MZPREC = 312.2:TOLERANCEMZ = 0.1:TOLERANCEPPM=10"
## x <- .where(query)
## res <- lapply(x, .parse_where)
## names(res) <- vapply(res, function(z) names(z)[1], character(1))
## res <- .group_min_max(res, name = "RT")
## res <- .group_min_max(res, name = "SCAN")
