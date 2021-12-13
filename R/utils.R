#' Create empty tibble
#'
#' This functions creates empty tibble with columns names specified
#' as parameter.
#'
#' @param column_names character vector with column names
#'
#' @importFrom tibble tibble
#' @importFrom purrr map
#'
#' @return empty tibble object
#' @export
#'
#' @examples
#' empty_tibble(c("a", "b"))
empty_tibble <- function(column_names) {
  args <- map(column_names, ~ character())
  names(args) <- column_names
  do.call(tibble, args)
}
