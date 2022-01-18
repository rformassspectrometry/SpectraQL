#' @title Parsing an MassQL query
#'
#' Various functions are used to parse and extract the various elements from an
#' MassQL query:
#'
#' - `.what`: extracts what should be returned. It's the field betwee `QUERY`
#'   and `WHERE` or `FILTER`.
#' - `.where`: extracts the individual `WHERE` conditions.
#' - `.parse_where`: is used to process an individual `WHERE` condition to
#'   separate all fields and extract the variables and their values.
#' - `.group_min_max`: groups min and max values for conditions.
#'
#' @noRd
NULL

#' *Translate* MassQL query to filter functions.
#'
#' @param x `character` with the MassQL query.
#'
#' @return `list` with `ProcessingStep` objects, each representing one filter
#'     for a `Spectra` object.
#'
#' @author Johannes Rainer
#'
#' @noRd
.query_to_filters <- function(x) {
    qry <- lapply(.where(x), .parse_where)
    names(qry) <- vapply(qry, function(z) names(z)[1], character(1))
    qry <- .group_min_max(qry, name = "SCAN")
    qry <- .group_min_max(qry, name = "RT")
    res <- vector("list", length = length(qry))
    for (i in seq_along(qry)) {
        fun <- .CONDITION_FUNCTIONS[names(qry)[i]]
        if (is.na(fun) | length(fun) == 0)
            stop("Condition '", names(qry)[i], "' not supported.")
        res[[i]] <- do.call(fun, qry[i])
    }
    res[lengths(res) > 0]
}

#' What should be returned? This returns the part of the query which is between
#' `QUERY` and either the end of the line, `WHERE` or `FILTER`.
#'
#' @author Johannes Rainer
#'
#' @noRd
.what <- function(x) {
    res <- sub(".*?query[[:space:]]*(.*?)[[:space:]]*(where.*|filter.*|$)",
               "\\1", x, ignore.case = TRUE)
    res[res == x] <- NA_character_
    res
}

.validate_what <- function(x) {
    if (anyNA(x) || !length(x))
        stop("Syntax error: unable to extract type of data from query",
             call. = FALSE)
    x
}

#' Extracts the data as requested.
#'
#' @param x `Spectra` object.
#'
#' @param what `character(1)` with the information what should be extracted.
#'     This is expected to be the processed/parsed *what* string (returned by
#'     `.what`).
#'
#' @author Johannes Rainer
#'
#' @noRd
.extract_what <- function(x, what = character()) {
    what <- gsub("[[:space:]]", "", tolower(what))
    ## Define what type of data (all, MS1, MS2) should be returned.
    .what_data(x, what)
    ## Define if and how the data should be transformed.
}

#' @importMethodsFrom Spectra filterMsLevel
#'
#' Define which data to extract. Supported are *, ms1data, ms2data.
#'
#' @author Johannes Rainer
#'
#' @noRd
.what_data <- function(x, what = character()) {
    res <- NULL
    if (length(grep("(^\\*$|\\(\\*\\))", what)))
        res <- x
    if (length(grep("(^ms1data$|\\(ms1data\\))", what)))
        res <- filterMsLevel(x, msLevel. = 1L)
    if (length(grep("(^ms2data$|\\(ms2data\\))", what)))
        res <- filterMsLevel(x, msLevel. = 2L)
    if (is.null(res))
        stop("data definition '", what, "' not supported.", call. = FALSE)
    res
}

#' Condition(s): everything between WHERE and end of line or FILTER.
#' individual conditions are additionally split by `AND`.
#'
#' @author Johannes Rainer
#'
#' @noRd
.where <- function(x) {
    res <- sub(".*?where[[:space:]]*(.*?)[[:space:]]*(filter.*|$)",
               "\\1", x, ignore.case = TRUE)
    res[res == x] <- NA_character_
    res <- unlist(strsplit(res, split = "[[:space:]]*(and|AND)[[:space:]]*"))
    res[nchar(res) > 0]
}

#' group elements with name `*MIN` and `*MAX` into pairs of two.
#'
#' @author Johannes Rainer
#'
#' @noRd
.group_min_max <- function(x, name = "RT") {
    mini <- which(names(x) == paste0(name, "MIN"))
    lmini <- length(mini)
    maxi <- which(names(x) == paste0(name, "MAX"))
    lmaxi <- length(maxi)
    l <- max(lmini, lmaxi)
    if (!l)
        return(x)
    res <- list()
    for (i in seq_len(l)) {
        if (i <= lmini & i <= lmaxi)
            res[[i]] <- c(x[[mini[i]]], x[[maxi[i]]])
        if (i <= lmini & i > lmaxi)
            res[[i]] <- x[[mini[i]]]
        if (i > lmini & i <= lmaxi)
            res[[i]] <- x[[maxi[i]]]
    }
    names(res) <- rep(name, length(res))
    c(res, x[-union(mini, maxi)])
}

#' This function parses the where condition of a MassQL query. The string will
#' be parsed by first splitting by `:` (separating the main variable/condition
#' from *qualifiers*). Consecutively each element is splitted by `"="` to
#' extract the names of the variables and qualifiers as well as their value.
#'
#' Examples for x:
#' - WHERE RTMIN = 123
#' - WHERE MS2PROD=321.2:TOLERANCEMZ=0.1:TOLERANCEPPM=20
#'
#' @param x `character(1)` with (one!) *where* part of a MassQL query (e.g. one
#'     element of what is returned from `.where`.
#'
#' @return named `character`, names being the variable name(s) and elements
#'     their value.
#'
#' @author Johannes Rainer
#'
#' @noRd
.parse_where <- function(x) {
    if (!length(x))
        return(character())
    spl <- unlist(strsplit(x, split = "[[:space:]]*:[[:space:]]*"),
                  use.names = FALSE)
    spl <- unlist(lapply(spl, strsplit, split = "[[:space:]]*=[[:space:]]*"),
                  recursive = FALSE, use.names = FALSE)
    vals <- unlist(lapply(spl, `[`, 2), use.names = FALSE)
    names(vals) <- toupper(unlist(lapply(spl, `[`, 1), use.names = FALSE))
    vals
}

#' Mapping between variables (MassQL conditions) and the function that can
#' "translate" that into a Spectra filter.
#'
#' @noRd
.CONDITION_FUNCTIONS <- c(
    RT = ".translate_filter_rt",
    SCAN = ".translate_filter_scan",
    CHARGE = ".translate_filter_charge",
    POLARITY = ".translate_filter_polarity",
    MS2PROD = ".translate_filter_ms2prod",
    MS2PREC = ".translate_filter_ms2prec",
    MS2NL = ".translate_filter_ms2nl"
)

#' Convert the RT condition to a `ProcessingStep` with a filter function
#' for `Spectra` objects.
#'
#' @param ... the parameters (rtmin and rtmax) for the filter.
#'
#' @return `ProcessingStep`
#'
#' @importFrom ProtGenerics ProcessingStep
#'
#' @importFrom Spectra filterRt
#'
#' @author Johannes Rainer
#'
#' @noRd
.translate_filter_rt <- function(...) {
    parms <- list(...)[[1L]]
    rtmin <- -Inf
    rtmax <- Inf
    if (any(names(parms) == "RTMIN"))
        rtmin <- as.numeric(parms["RTMIN"])
    ## Could eventually also call eval(parse(parms["RTMIN"])) to support
    ## numeric operations as input, such as "3 * 5"
    if (any(names(parms) == "RTMAX"))
        rtmax <- as.numeric(parms["RTMAX"])
    if (is.na(rtmin) | is.na(rtmax))
        stop("Non-numeric value for 'RTMIN' or 'RTMAX': got RTMIN=",
             rtmin, " RTMAX=", rtmax, call. = FALSE)
    ProcessingStep(filterRt, ARGS = list(rt = c(rtmin, rtmax)))
}

#' Filter a `Spectra` based on provided scan numbers.
#'
#' @importMethodsFrom Spectra acquisitionNum
#'
#' @author Johannes Rainer
#'
#' @noRd
.translate_filter_scan <- function(...) {
    parms <- list(...)[[1L]]
    scan_min <- -Inf
    scan_max <- Inf
    if (any(names(parms) == "SCANMIN"))
        scan_min <- as.numeric(parms["SCANMIN"])
    if (any(names(parms) == "SCANMAX"))
        scan_max <- as.numeric(parms["SCANMAX"])
    if (is.na(scan_min) | is.na(scan_max))
        stop("Non-numeric value for 'SCANMIN' or 'SCANMAX': got SCANMIN=",
             scan_min, " SCANMAX=", scan_max, call. = FALSE)
    filt_fun <- function(x, scan) {
        acq <- acquisitionNum(x)
        x[which(acq >= scan[1L] & acq <= scan[2L])]
    }
    ProcessingStep(filt_fun, ARGS = list(scan = c(scan_min, scan_max)))
}

#' Filter a `Spectra` based on provided charge.
#'
#' @importFrom Spectra filterPrecursorCharge
#'
#' @author Johannes Rainer, Andrea Vicini
#'
#' @noRd
.translate_filter_charge <- function(...) {
    parms <- list(...)[[1L]]
    charge <- integer(0)
    if (any(names(parms) == "CHARGE")) {
        tmp <- .parse_or(parms["CHARGE"])
        if(any(!grepl("(^[+-]?[0-9]+)$|^NA$", tmp)))
            stop("Non-integer value for 'CHARGE'")
        charge <- as.integer(tmp)
    }
    ProcessingStep(filterPrecursorCharge, ARGS = list(z = charge))
}

#' Filter a `Spectra` based on provided polarity.
#'
#' @importFrom Spectra filterPolarity
#'
#' @author Johannes Rainer, Andrea Vicini
#'
#' @noRd
.translate_filter_polarity <- function(...) {
    parms <- list(...)[[1L]]
    polarity <- numeric(0)
    if (any(names(parms) == "POLARITY")) {
        polarity <- toupper(.parse_or(parms["POLARITY"]))
        polarity <- unname(c("POSITIVE" = 1L, "NEGATIVE" = 0L,
                            "NA" = -1L)[polarity])
        if(anyNA(polarity))
            stop("Invalid value for 'POLARITY'")
    }
    ProcessingStep(filterPolarity, ARGS = list(polarity = polarity))
}

#' Filter a `Spectra` based on MS2 peak.
#'
#' @importFrom Spectra containsMz
#'
#' @author Johannes Rainer, Andrea Vicini
#'
#' @noRd
.translate_filter_ms2prod <- function(...) {
    parms <- list(...)[[1L]]
    pmz <- numeric(0)
    ppm <- 0
    tolerance <- 0
    if (any(names(parms) == "MS2PROD"))
        pmz <- as.numeric(.parse_or(parms["MS2PROD"]))
    if(anyNA(pmz))
        stop("Non-numeric values for 'MS2PROD'")
    if (any(names(parms) == "TOLERANCEMZ"))
        tolerance <- as.numeric(parms["TOLERANCEMZ"])
    if(is.na(tolerance))
        stop("Non-numeric value for 'TOLERANCEMZ'")
    if (any(names(parms) == "TOLERANCEPPM"))
        ppm <- as.numeric(parms["TOLERANCEPPM"])
    if(is.na(ppm))
        stop("Non-numeric value for 'TOLERANCEPPM'")
    if (length(pmz)) {
        filt_ms2prod <- function(x, mz, tolerance, ppm) {
            x[containsMz(x, mz, tolerance, ppm)]
        }
        ProcessingStep(filt_ms2prod, ARGS = list(mz = pmz, tolerance = tolerance,
                                               ppm = ppm))
    } else ProcessingStep(identity)
}

filt_fun <- function(x, pmz, tolerance, ppm) {
    mzr <- pmz + c(-1, 1) * (tolerance + ppm(pmz, ppm = ppm))
    do.call(c, lapply(mzr, function(v) ProcessingStep(filterPrecursorMz,
                                                      ARGS = list(mz = v))))
}

#' @importFrom MsCoreUtils ppm
#'
#' @importMethodsFrom Spectra filterPrecursorMz
#'
#' @author Johannes Rainer
#'
#' @noRd
.translate_filter_ms2prec <- function(...) {
    parms <- list(...)[[1L]]
    pmz <- numeric()
    ppm <- 0
    tolerance <- 0
    if (any(names(parms) == "MS2PREC"))
        pmz <- as.numeric(parms["MS2PREC"])
    if (is.na(pmz))
        stop("Non-numeric value for 'MS2PREC'")
    if (any(names(parms) == "TOLERANCEMZ"))
        tolerance <- as.numeric(parms["TOLERANCEMZ"])
    if (is.na(tolerance))
        stop("Non-numeric value for 'TOLERANCEMZ'")
    if (any(names(parms) == "TOLERANCEPPM"))
        ppm <- as.numeric(parms["TOLERANCEPPM"])
    if (is.na(ppm))
        stop("Non-numeric value for 'TOLERANCEPPM'")
    if (length(pmz)) {
        mzr <- pmz + c(-1, 1) * (tolerance + ppm(pmz, ppm = ppm))
        ProcessingStep(filterPrecursorMz, ARGS = list(mz = mzr))
    } else ProcessingStep(identity)
}

## query <- "QUERY * WHERE RTMIN = 123 AND RTMAX = 130  AND MZPREC = 312.2:TOLERANCEMZ = 0.1:TOLERANCEPPM=10"
## x <- .where(query)
## res <- lapply(x, .parse_where)
## names(res) <- vapply(res, function(z) names(z)[1], character(1))
## res <- .group_min_max(res, name = "RT")
## res <- .group_min_max(res, name = "SCAN")

#' @importFrom MsCoreUtils ppm
#'
#' @importMethodsFrom Spectra containsNeutralLoss
#'
#' @author Johannes Rainer
#'
#' @noRd
.translate_filter_ms2nl <- function(...) {
    parms <- list(...)[[1L]]
    nl <- numeric(0)
    ppm <- 0
    tolerance <- 0
    if (any(names(parms) == "MS2NL"))
        nl <- as.numeric(.parse_or(parms["MS2NL"]))
    if (anyNA(nl))
        stop("Non-numeric value/s for 'MS2NL'")
    if (any(names(parms) == "TOLERANCEMZ"))
        tolerance <- as.numeric(parms["TOLERANCEMZ"])
    if (is.na(tolerance))
        stop("Non-numeric value for 'TOLERANCEMZ'")
    if (any(names(parms) == "TOLERANCEPPM"))
        ppm <- as.numeric(parms["TOLERANCEPPM"])
    if (is.na(ppm))
        stop("Non-numeric value for 'TOLERANCEPPM'")
    if (length(nl)) {
        if(length(nl) > 1)
            stop("OR not yet supported for 'MS2NL'")
        filt_ms2nl <- function(x, neutralLoss, tolerance, ppm) {
            x[containsNeutralLoss(x, neutralLoss, tolerance, ppm)]
        }
        ProcessingStep(filt_ms2nl, ARGS = list(neutralLoss = nl,
                                                        tolerance = tolerance,
                                                        ppm = ppm))
    } else ProcessingStep(identity)
}

.parse_or <- function(x) {
    unlist(strsplit(gsub("^\\s+|\\s+$|\\(|\\)", "", gsub("\\s+", " ", x)),
                    split = " OR "))
}
