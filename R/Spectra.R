#' @title Query a Spectra object using MassQL
#'
#' @aliases query query,Spectra-method
#'
#' @name query
#'
#' @description
#'
#' The `query` function allows to query and subset/filter a `Spectra` object
#' using a Mass Spec Query Language
#' [MassQL](https://mwang87.github.io/MassQueryLanguage_Documentation/)
#' expression.
#'
#' A MassQL query is expressed in the form `"QUERY <type of data> WHERE
#' <condition> AND <condition> FILTER <filter> AND <filter>"`, multiple
#' *conditions* and *filters* can be combined with logical *and* operations. In
#' the MassQL definition, *conditions* subsets the data to specific spectra
#' while *filter* restricts the data within a spectrum. Note that at present
#' MassQL *filters* are not supported. Also note that MassQL queries are
#' interpreted case insensitive in `SpectraQL`.
#'
#' See also the package vignette for more details.
#'
#' @section Type of data:
#'
#' The `"<type of data>"` allows to define which data should be extracted from
#' the selected spectra. MassQL defines *type of data* being `"MS1DATA"` or
#' `"MS2DATA"` to retrieve data from MS1 or MS2 scans. By default peak data will
#' be returned, but in addition, MASSQL defines additional functions that can
#' be applied to modify the data or select different data to be returned. In
#' addition *SpectraQL* defines the special type of data `"*"` which will return
#' the results as a `Spectra` object. *SpectraQL* supports:
#'
#' - `"*"`: select all data and return the data subset as a [Spectra()] object.
#' - `"MS1DATA"`: return the [peaksData()] from all selected **MS1** spectra,
#'   i.e. a `list` with two column matrices with the peaks' m/z and intensity
#'   values.
#' - `"MS2DATA"`: return the [peaksData()] from all selected **MS2** spectra,
#'   i.e. a `list` with two column matrices with the peaks' m/z and intensity
#'   values.
#' - `"scaninfo(MS1DATA)"`, `"scaninfo(MS2DATA)"`: return the [spectraData()]
#'   of all selected spectra.
#' - `"scansum(MS1DATA)"`, `"scansum(MS2DATA)"`: sum of the peak intensities of
#'   the selected spectra (TIC, or XIC if combined with `"FILTER"`).
#'
#' @section Conditions:
#'
#' Conditions define to which spectra the data set should be subsetted. A
#' *condition* will subset a `Spectra` object to selected spectra, but will not
#' (unlike *Filters*, see further below) filter peaks from a spectrum. Several
#' conditions can be combined with `"and"` (case insensitive). The syntax for a
#' condition is `"<condition> = <value>"`, e.g. `"MS2PROD = 144.1"`. Such
#' conditions can be further refined by additional expressions that allow for
#' example to define acceptable tolerances for m/z differences. `SpectraQL`
#' supports (case insensitive):
#'
#' - `"RTMIN"`: minimum retention time (in **seconds**).
#' - `"RTMAX"`: maximum retention time (in **seconds**).
#' - `"SCANMIN"`: the minimum scan number (acquisition number).
#' - `"SCANMAX"`: the maximum scan number (acquisition number).
#' - `"CHARGE"`: the charge for MS2 spectra.
#' - `"POLARITY"`: the polarity of the spectra (can be `"positive"`,
#'   `"negative"`, `"pos"` or `"neg"`, case insensitive).
#' - `"MS2PROD"` or `"MS2MZ"`: allows to select MS2 spectra that contain a peak
#'   with particular m/z value(s). See below for examples.
#' - `"MS2PREC"`: allows to select MS2 spectra with the defined precursor m/z
#'   value(s). See below for examples.
#' - `"MS1MZ"`: allows to select MS1 spectra containing peak(s) with the defined
#'   m/z value(s).
#' - `"MS2NL"`: allows to look for a neutral loss from precursor in MS2 spectra.
#'
#' All conditions involving m/z values allow to specify a mass accuracy using
#' the optional fields `"TOLERANCEMZ"` and `"TOLERANCEPPM"` that define the
#' absolute and m/z-relative acceptable difference in m/z values. One or both
#' fields can be attached to a *condition* such as
#' `"MS2PREC=100:TOLERANCEMZ=0.1:TOLERANCEPPM=20"` to select for example all
#' MS2 spectra with a precursor m/z equal to 100 accepting a difference of 0.1
#' and 20 ppm. Note that in contrast to MassQL, the default tolarance and ppm
#' is 0 for all calls.
#'
#' @section Filters:
#'
#' Filters subset the data within spectra, i.e. select which peaks within
#' spectra should be retrieved. *SpectraQL* supports the following filters:
#'
#' - `"MS1MZ"`: filters MS1 spectra keeping only peaks with matching m/z values
#'   (tolerance can be specified with `"TOLERANCEMZ"` and `"TOLERANCEPPM"` as
#'   for conditions).
#' - `"MS2MZ"`: filters MS2 spectra keeping only peaks with matching m/z values
#'   (tolerance can be specified with `"TOLERANCEMZ"` and `"TOLERANCEPPM"` as
#'   for conditions).
#'
#' @param x The `Spectra` object to query.
#'
#' @param query `character(1)` with the MassQL query.
#'
#' @param ... currently ignored.
#'
#' @return Depending on the `<type of data>` part of the MassQL query.
#'
#' @author Andrea Vicini, Johannes Rainer
#'
#' @examples
#'
#' ## Read a data file with MS1 and MS2 spectra
#' library(msdata)
#' library(Spectra)
#' fls <- dir(system.file("TripleTOF-SWATH", package = "msdata"),
#'     full.names = TRUE)
#' sps_dda <- Spectra(fls[1L])
#'
#' ## Subset to spectra measured between 300 and 400 seconds
#' query(sps_dda, "QUERY * WHERE RTMIN = 300 AND RTMAX = 400")
#'
#' ## To extract peaks data from MS1 or MS2 spectra use "MS1DATA" or "MS2DATA"
#' ## instead of *. Note also that queries are case-insensitive.
#' pks <- query(sps_dda, "query ms1data where rtmin = 300 and rtmax = 400")
#' pks
#' head(pks[[1L]])
#'
#' ## To select (MS2) spectra with a certain precursor m/z the MS2PREC condition
#' ## can be used. Below we extract all spectra with a precursor m/z of 99.9
#' ## accepting also a difference of 10ppm
#' query(sps_dda, "QUERY * WHERE MS2PREC = 99.967:TOLERANCEPPM=10")
#'
#' ## It is also possible to specify multiple precursor m/z values:
#' query(sps_dda, "QUERY * WHERE MS2PREC = (99.967 OR 428.88):TOLERANCEPPM=10")
#'
#' ## To select all MS1 spectra that contain a peak with a certain m/z we can
#' ## use the MS1MZ condition. Below we combine this with an absolute tolerance
#' ## using TOLERANCEMZ.
#' query(sps_dda, "QUERY * WHERE MS1MZ = 100:TOLERANCEMZ=1")
#'
#' ## Using MS2DATA in combination with MS1MZ will not return any spectra.
#' query(sps_dda, "QUERY MS2DATA WHERE MS1MZ = 100:TOLERANCEMZ=1")
#'
#' ## In contrast, do select MS2 spectra containing a peak with a certain m/z
#' ## we have to use the condition MS2PROD
#' query(sps_dda, "QUERY * WHERE MS2PROD = 100:TOLERANCEMZ=1")
#'
#' ## MS2MZ can be used as alternative to MS2PROD
#' query(sps_dda, "QUERY * WHERE MS2MZ = 100:TOLERANCEMZ=1")
#'
#' ## Select MS2 spectra containing a peak with neutral loss from
#' ## precursor of 100 allowing a m/z relative ppm tolerance of 5)
#' res <- query(sps_dda, "QUERY MS2DATA WHERE MS2NL=100:TOLERANCEPPM=5")
#'
#' ## Combine two different conditions: selection of spectra with positive
#' ## polarity and retention time greater than 200
#' res <- query(sps_dda, "QUERY * WHERE RTMIN = 200 AND POLARITY = Positive")
NULL

#' @importClassesFrom Spectra Spectra
#'
#' @importFrom methods is
#'
#' @exportMethod query
#'
#' @rdname query
setMethod("query", "Spectra", function(x, query = character(), ...) {
    if (!length(query))
        return(x)
    if (length(query) != 1L)
        stop("'query' is expected to be a single character string.",
             call. = FALSE)
    ## Filter the Spectra
    res <- .query_spectra(x, query, ...)
    ## Decide what data to return.
    .query_what(res, query, ...)
})

#' Parse all conditions from the query and filter/subset the `Spectra` object.
#'
#' @param x `Spectra`
#'
#' @param query `character(1)` with the MassQL query
#'
#' @return filtered `Spectra`
#'
#' @author Johannes Rainer
#'
#' @noRd
.query_spectra <- function(x, query = character(), ...) {
    flt <- .query_to_filters(.where(query))
    flt <- c(flt, .query_to_filters(.filter(query), MAP = .FILTER_FUNCTIONS,
                                    label = "Filter"))
    for (pstep in flt)
        x <- do.call(pstep@FUN, args = c(list(x), pstep@ARGS))
    x
}

#' Interpret the MassQL query to extract the requested type of data.
#'
#' @param x `Spectra` that was eventually filtered.
#'
#' @param query `character(1)` with the MassQL query.
#'
#' @return depending on the MassQL query.
#'
#' @author Johannes Rainer
#'
#' @noRd
.query_what <- function(x, query = character(), ...) {
    what <- .what(query)
    what <- .validate_what(what)
    .extract_what(x, what)
}
