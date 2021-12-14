#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Create empty tibble
#'
#' This functions creates empty tibble with columns names specified
#' as parameter.
#'
#' @param column_names character vector with column names
#' @param column_types character vector with column types
#'
#' @return empty tibble object
#' @export
#'
#' @examples
#' empty_tibble(c("a", "b"), c("character", "integer"))
empty_tibble <- function(column_names, column_types) {
  args <- purrr::map(column_types, \(ct) {
    eval(rlang::parse_expr(glue::glue('{rlang::enexpr(ct)}()')))
  })
  names(args) <- column_names
  do.call(tibble::tibble, args)
}
