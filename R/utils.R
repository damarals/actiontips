#' Create empty tibble
#'
#' This functions creates empty tibble with columns names specified
#' as parameter.
#'
#' @param column_names character vector with column names
#' @param column_types character vector with column types
#'
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom glue glue
#' @importFrom rlang enexpr parse_expr
#'
#' @return empty tibble object
#' @export
#'
#' @examples
#' empty_tibble(c("a", "b"), c("character", "integer"))
empty_tibble <- function(column_names, column_types) {
  args <- map(column_types, \(ct) eval(parse_expr(glue('{enexpr(ct)}()'))))
  names(args) <- column_names
  do.call(tibble, args)
}
