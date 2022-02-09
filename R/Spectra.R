#' @title Query a Spectra object using MassQL
#'
#' @aliases query
#'
#' The `query` function allows to query and subset/filter a `Spectra` object
#' using a Mass Spec Query Language
#' [MassQL](https://mwang87.github.io/MassQueryLanguage_Documentation/)
#' expression.
#'
#' A MassQL query is expressed in the form `QUERY <type of data> WHERE
#' <condition> AND <condition> FILTER <filter> AND <filter>`, multiple
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
#' The `<type of data>` allows to define which data should be extracted from
#' the selected spectra. MassQL defines *type of data* being `MS1DATA` or
#' `MS2DATA` to retrieve MS1 or MS2 scans. In addition, functions can be applied
#' to these to modify (e.g. sum) the data. `SpectraQL` supports:
#'
#' - `*`: select all data and return the data subset as a [Spectra()] object.
#' - `MS1DATA`: return a [Spectra()] with all MS1 scans from the selected
#'   spectra.
#' - `MS2DATA`: return a [Spectra()] with all MS2 scans from the selected
#'   spectra.
#' - `scaninfo(MS1DATA)`, `scaninfo(MS2DATA)`: return the [spectraData()] of all
#'   selected spectra.
#' - `scansum(MS1DATA)`, `scaninfo(MS2DATA)`: sum of the peak intensities of
#'   the selected spectra.
#' - `scannum(MS1DATA)`, `scannum(MS2DATA)`: the scan number(s) of the selected
#'   spectra.
#'
#' @section Conditions:
#'
#' Conditions define to which spectra the data set should be subsetted. Several
#' conditions can be combined with `"and"` (case insensitive). The syntax for a
#' condition is `<condition> = <value>`, e.g. `MS2PROD = 144.1`. Such conditions
#' can be further refined by additional expressions that allow for example to
#' define acceptable tolerances for m/z differences. `SpectraQL` supports (case
#' insensitive):
#'
#' - `RTMIN`: minimum retention time (in **seconds**).
#' - `RTMAX`: maximum retention time (in **seconds**).
#' - `SCANMIN`: the minimum scan number (acquisition number).
#' - `SCANMAX`: the maximum scan number (acquisition number).
#' - `CHARGE`: the charge for MS2 spectra.
#' - `POLARITY`: the polarity of the spectra (can be `"positive"`, `"negative"`,
#'   `"pos"` or `"neg"`, case insensitive).
#' - `MS2PROD`: allows to select MS2 spectra that contain a peak with
#'   particular m/z value(s). See below for examples.
#' - `MS2PREC`: allows to select MS2 spectra with the defined precursor m/z
#'   value(s). See below for examples.
#' - `MS2MZ`: allows to select MS1 spectra containing peaks with the defined
#'   m/z.
#'
#' All conditions involving m/z values allow to specify a mass accuracy using
#' the optional fields `TOLERANCEMZ` and `TOLERANCEPPM` that define the absolute
#' and m/z-relative acceptable difference in m/z values. One or both fields can
#' be attached to a *condition* such as
#' `MS2PREC=100:TOLERANCEMZ=0.1:TOLERANCEPPM=20` to select for example all
#' MS2 spectra with a precursor m/z equal to 100 accepting a difference of 0.1
#' and 20 ppm. Note that in contrast to MassQL, the default tolarance and ppm
#' is 0 for all calls.
#'
#' @section Filters:
#'
#' Filters subset the data within spectra, i.e. select which peaks within
#' spectra should be retrieved. `SpectraQL` does not support filters yet.
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
#' @importClassesFrom Spectra Spectra
#'
#' @exportMethod query
#'
#' @examples
#'
#' library(msdata)
#' library(Spectra)
#' fls <- dir(system.file("TripleTOF-SWATH", package = "msdata"),
#'     full.names = TRUE)
#' sps_dda <- Spectra(fls[1L])
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
    flt <- .query_to_filters(query)
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
