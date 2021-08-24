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
    ## Filter the Spectra
    res <- .query_spectra(x, query, ...)
    ## Decide what data to return.
})

.query_spectra <- function(x, query = character(), ...) {
    if (length(query) != 1L)
        stop("'query' is expected to be a single character string.",
             call. = FALSE)
    flt <- .query_to_filters(query)
    for (pstep in flt)
        x <- do.call(pstep@FUN, args = c(list(x), pstep@ARGS))
    x
}
