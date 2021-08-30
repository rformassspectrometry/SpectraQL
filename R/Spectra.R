#' @title Query a Spectra object using MassQL
#'
#' @aliases query
#' 
#' The `query` function allows to query and filter a `Spectra` object using
#' a Mass Spec Query Language
#' [MassQL](https://mwang87.github.io/MassQueryLanguage_Documentation/)
#' expression.
#'
#' @param x The `Spectra` object to query.
#'
#' @param query `character(1)` with the MassQL query.
#'
#' @param ... currently ignored.
#' 
#' @importClassesFrom Spectra Spectra
#'
#' @author Johannes Rainer
#' 
#' @exportMethod query
setMethod("query", "Spectra", function(x, query = character(), ...) {
    if (length(query) != 1L)
        stop("'query' is expected to be a single character string.",
             call. = FALSE)
    ## Filter the Spectra
    res <- .query_spectra(x, query, ...)
    ## Decide what data to return.
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
}
