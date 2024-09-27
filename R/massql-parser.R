#' @title Parsing an MassQL query
#'
#' @description
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

#' *Translate* MassQL query to filter functions. This function works both for
#' "conditions" (WHERE MS2MZ= ...) as well as "filters" (FILTER MS1MZ = ).
#'
#' @param x result from a call to `.where(<query>)` or `.filter(<query>)`
#'
#' @param MAP defines how conditions or filters should be translated to filter
#'     functions.
#'
#' @return `list` with `ProcessingStep` objects, each representing one filter
#'     for a `Spectra` object.
#'
#' @author Johannes Rainer
#'
#' @noRd
.query_to_filters <- function(x, MAP = .CONDITION_FUNCTIONS,
                              label = "Condition") {
    qry <- lapply(x, .parse_where)
    names(qry) <- vapply(qry, function(z) names(z)[1], character(1))
    qry <- .group_min_max(qry, name = "SCAN")
    qry <- .group_min_max(qry, name = "RT")
    res <- vector("list", length = length(qry))
    for (i in seq_along(qry)) {
        fun <- MAP[names(qry)[i]]
        if (is.na(fun) | length(fun) == 0)
            stop(label, " '", names(qry)[i], "' not supported.")
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
    .parse_query(x, "query", "where.*|filter", split = FALSE)
}

.validate_what <- function(x) {
    if (anyNA(x) || !length(x))
        stop("Wrong syntax: unable to extract type of data from query",
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
#' @return the requested data. Can be the subsetted `Spectra` input object or,
#'     depending on `what`, also other data types.
#'
#' @author Johannes Rainer
#'
#' @noRd
.extract_what <- function(x, what = character()) {
    what <- gsub("[[:space:]]", "", tolower(what))
    x <- .what_data(x, what)
    .what_extract(x, what)
}

#' Define which data to extract. Supported are *, ms1data, ms2data.
#'
#' @author Johannes Rainer
#'
#' @importMethodsFrom Spectra filterMsLevel
#'
#' @noRd
.what_data <- function(x, what = character()) {
    res <- NULL
    if (grepl("(^\\*$|\\(\\*\\))", what))
        res <- x
    if (grepl("(^ms1data$|\\(ms1data\\))", what))
        res <- filterMsLevel(x, msLevel. = 1L)
    if (grepl("(^ms2data$|\\(ms2data\\))", what))
        res <- filterMsLevel(x, msLevel. = 2L)
    if (is.null(res))
        stop("data definition '", what, "' not supported.", call. = FALSE)
    res
}

#' Process specific data extraction functions. This function collects the
#' results and returns it to the user.
#'
#' @param x `Spectra`.
#'
#' @param what `character`.
#'
#' @return depends on `what`
#'
#' @author Johannes Rainer
#'
#' @importMethodsFrom Spectra ionCount
#'
#' @importMethodsFrom Spectra peaksData
#'
#' @importMethodsFrom Spectra spectraData
#'
#' @noRd
.what_extract <- function(x, what = character()) {
    what <- strsplit(what, split = "(", fixed = TRUE)[[1L]]
    if (length(what) > 1L) {
        fun <- what[1L]
        if (grepl("scaninfo", fun))
            return(spectraData(x))
        if (grepl("scansum", fun))
            return(ionCount(x))
        stop("function '", fun, "' not supported.", call. = FALSE)
    }
    ## If there is no function we return the peaksData unless what is * in which
    ## case we return a `Spectra`.
    if (grepl("(^\\*$|\\(\\*\\))", what))
        x
    else peaksData(x)
}

#' Condition(s): everything between WHERE and end of line or FILTER.
#' individual conditions are additionally split by `AND`.
#'
#' @author Johannes Rainer
#'
#' @noRd
.where <- function(x) {
    .parse_query(x, "where", "filter", split = TRUE)
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
    RT = ".translate_condition_rt",
    SCAN = ".translate_condition_scan",
    CHARGE = ".translate_condition_charge",
    POLARITY = ".translate_condition_polarity",
    MS2PROD = ".translate_condition_ms2prod",
    MS2MZ = ".translate_condition_ms2prod",
    MS2PREC = ".translate_condition_ms2prec",
    MS1MZ = ".translate_condition_ms1mz",
    MS2NL = ".translate_condition_ms2nl"
)

#' Convert the RT condition to a `ProcessingStep` with a filter function
#' for `Spectra` objects.
#'
#' @param ... the parameters (rtmin and rtmax) for the filter.
#'
#' @return `ProcessingStep`
#'
#' @author Johannes Rainer
#'
#' @importFrom ProtGenerics ProcessingStep
#'
#' @importFrom Spectra filterRt
#'
#' @noRd
.translate_condition_rt <- function(...) {
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
#' @author Johannes Rainer
#'
#' @importMethodsFrom Spectra acquisitionNum
#'
#' @noRd
.translate_condition_scan <- function(...) {
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
#' @author Johannes Rainer, Andrea Vicini
#'
#' @importFrom Spectra filterPrecursorCharge
#'
#' @noRd
.translate_condition_charge <- function(...) {
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
#' @author Johannes Rainer, Andrea Vicini
#'
#' @importFrom Spectra filterPolarity
#'
#' @noRd
.translate_condition_polarity <- function(...) {
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

.translate_condition_ms2prod <- function(...) {
    .translate_condition_peak_mz(..., msLevel = 2L, value = "MS2PROD")
}

.translate_condition_ms1mz <- function(...) {
    .translate_condition_peak_mz(..., msLevel = 1L, value = "MS1MZ")
}

#' Filter a `Spectra` based on MS2 peak.
#'
#' @author Johannes Rainer, Andrea Vicini
#'
#' @importFrom Spectra containsMz
#'
#' @noRd
.translate_condition_peak_mz <- function(..., msLevel = 2L, value = "MS2PROD") {
    parms <- list(...)[[1L]]
    pmz <- numeric(0)
    ppm <- 0
    tolerance <- 0
    if (any(names(parms) == value))
        pmz <- as.numeric(.parse_or(parms[value]))
    if(anyNA(pmz))
        stop("Missing or non-numeric value(s) for '", value, "'")
    if (any(names(parms) == "TOLERANCEMZ"))
        tolerance <- as.numeric(parms["TOLERANCEMZ"])
    if(is.na(tolerance))
        stop("Non-numeric value for 'TOLERANCEMZ'")
    if (any(names(parms) == "TOLERANCEPPM"))
        ppm <- as.numeric(parms["TOLERANCEPPM"])
    if(is.na(ppm))
        stop("Non-numeric value for 'TOLERANCEPPM'")
    if (length(pmz)) {
        filt_peak_mz <- function(x, mz, tolerance, ppm, msLevel) {
            x <- filterMsLevel(x, msLevel = msLevel)
            x[containsMz(x, mz, tolerance, ppm)]
        }
        ProcessingStep(filt_peak_mz,
                       ARGS = list(mz = pmz, tolerance = tolerance, ppm = ppm,
                                   msLevel = msLevel))
    } else ProcessingStep(identity)
}

#' @author Johannes Rainer
#'
#' @importFrom MsCoreUtils ppm
#'
#' @importMethodsFrom Spectra filterPrecursorMzValues
#'
#' @noRd
.translate_condition_ms2prec <- function(...) {
    parms <- list(...)[[1L]]
    pmz <- numeric()
    ppm <- 0
    tolerance <- 0
    if (any(names(parms) == "MS2PREC"))
        pmz <- as.numeric(.parse_or(parms["MS2PREC"]))
    if (anyNA(pmz))
        stop("Missing or non-numeric value(s) for 'MS2PREC'")
    if (any(names(parms) == "TOLERANCEMZ"))
        tolerance <- as.numeric(parms["TOLERANCEMZ"])
    if (is.na(tolerance))
        stop("Non-numeric value for 'TOLERANCEMZ'")
    if (any(names(parms) == "TOLERANCEPPM"))
        ppm <- as.numeric(parms["TOLERANCEPPM"])
    if (is.na(ppm))
        stop("Non-numeric value for 'TOLERANCEPPM'")
    if (length(pmz)) {
        ProcessingStep(filterPrecursorMzValues,
                       ARGS = list(mz = pmz, ppm = ppm, tolerance = tolerance))
    } else ProcessingStep(identity)
}

#' @author Johannes Rainer
#'
#' @importFrom MsCoreUtils ppm
#'
#' @importMethodsFrom Spectra containsNeutralLoss
#'
#' @noRd
.translate_condition_ms2nl <- function(...) {
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
    unlist(strsplit(gsub("^\\s+|\\s+$|\\(\\s*|\\s*\\)", "",
                         gsub("\\s+", " ", x)), split = " (OR|or) "))
}

.parse_query <- function(x, from = "filter", to = "where", split = TRUE) {
    res <- sub(paste0(".*?", from, "[[:space:]]*(.*?)[[:space:]]*(",
                      to, ".*|$)"),
               "\\1", x, ignore.case = TRUE)
    res[res == x] <- NA_character_
    if (split) {
        res <- unlist(
            strsplit(res, split = "[[:space:]]*(and|AND)[[:space:]]*"))
        res[nchar(res) > 0 & !is.na(res)]
    } else res
}

#' filter(s): everything between FILTER and end of line or WHERE (?).
#' individual filters are additionally split by `AND`.
#'
#' @author Johannes Rainer
#'
#' @noRd
.filter <- function(x) {
    .parse_query(x, "filter", "where", split = TRUE)
}

.FILTER_FUNCTIONS <- c(
    MS2MZ = ".translate_filter_ms2mz",
    MS1MZ = ".translate_filter_ms1mz"
)

.translate_filter_ms2mz <- function(...) {
    .translate_filter_mz_value(..., msLevel = 2L, value = "MS2MZ")
}

.translate_filter_ms1mz <- function(...) {
    .translate_filter_mz_value(..., msLevel = 1L, value = "MS1MZ")
}

#' Filter a `Spectra` using the filterMzValue function.
#'
#' @author Johannes Rainer
#'
#' @importFrom Spectra filterMzValues
#'
#' @noRd
.translate_filter_mz_value <- function(..., msLevel = 2L, value = "MS2MZ") {
    parms <- list(...)[[1L]]
    mz <- numeric(0)
    ppm <- 0
    tolerance <- 0
    if (any(names(parms) == value))
        mz <- as.numeric(.parse_or(parms[value]))
    if(anyNA(mz))
        stop("Missing or non-numeric value(s) for '", value, "'")
    if (any(names(parms) == "TOLERANCEMZ"))
        tolerance <- as.numeric(parms["TOLERANCEMZ"])
    if(is.na(tolerance))
        stop("Non-numeric value for 'TOLERANCEMZ'")
    if (any(names(parms) == "TOLERANCEPPM"))
        ppm <- as.numeric(parms["TOLERANCEPPM"])
    if(is.na(ppm))
        stop("Non-numeric value for 'TOLERANCEPPM'")
    if (length(mz))
        ProcessingStep(
            filterMzValues, ARGS = list(mz = mz, tolerance = tolerance,
                                        ppm = ppm, msLevel. = msLevel))
    else ProcessingStep(identity)
}
