#' Tipper Raw JSON
#'
#' Returns raw json from specific action network tipper
#'
#' @param tipper_id The id of the tipper you want.
#'
#' @return a \code{list}
#' @export
#'
#' @examples
#' get_json(tipper_id = "184328")
get_json <- function(tipper_id) {
  suppressWarnings({
    tryCatch({
      u <- url(glue::glue('https://www.actionnetwork.com/picks/profile/{tipper_id}'), "rb")
      result <- rvest::read_html(u) %>%
        rvest::html_node(xpath = '//*[@id="__NEXT_DATA__"]') %>%
        rvest::html_text() %>%
        jsonlite::fromJSON()
      close(u)
      return(result)
    }, error = function(e) NULL)
  })
}

#' Tippers Table Summary
#'
#' Returns a tibble with id, name and others informations of each
#' tipper requested
#'
#' @param tippers_id A vector with all tippers id's you want information.
#' @param verbose Logical indication showing scrapping progress bar
#'
#' @return a \code{tibble} with 6 columns
#' \describe{
#'   \item{tipper_id}{unique id of the tipper}
#'   \item{name}{name of the tipper}
#'   \item{is_expert}{the tipper is an expert?}
#'   \item{is_author}{the tipper is an author of sport articles?}
#'   \item{is_verified}{the tipper is verified/famous in action network?}
#'   \item{num_followers}{total number of followers in action network}
#' }
#' @export
#'
#' @examples
#' get_tippers_table(tippers_id = c(184328, 184329))
get_tippers_table <- function(tippers_id = NA, verbose = FALSE) {
  column_names <- c("tipper_id", "name", "is_expert", "is_author",
                    "is_verified", "num_followers")
  column_types <- c("integer", "character", "logical", "logical",
                    "logical", "integer")
  furrr::future_map_dfr(tippers_id, function(tipper_id) {
    tryCatch({
      get_json(tipper_id) %>%
        purrr::pluck("props", "pageProps", "profile") %>%
        unlist() %>% tibble::enframe() %>%
        dplyr::filter(stringr::str_count(name, r"{\.}") <= 1) %>%
        tidyr::pivot_wider(dplyr::everything()) %>%
        dplyr::transmute(tipper_id = user_id, name, is_expert, is_author,
                         is_verified, num_followers = num_followers.total) %>%
        dplyr::mutate_at(c(1, 6), ~ as.integer(.)) %>%
        dplyr::mutate_at(2, ~ as.character(.)) %>%
        dplyr::mutate_at(3:5, ~ as.logical(.))
    }, error = function(e) empty_tibble(column_names, column_types))
  }, .progress = verbose)
}

#' Tippers Stats Table
#'
#' Returns a tibble with id, league, periods and W/L of tips
#'
#' @param tippers_id A vector with all tippers id's you want stats.
#' @param verbose Logical indication showing scrapping progress bar
#'
#' @return a \code{tibble} with 6 columns
#' \describe{
#'   \item{tipper_id}{unique id of the tipper}
#'   \item{league}{league name of the tip stats}
#'   \item{period}{period of sampling for calculate stats}
#'   \item{win}{count of wins in the specif league and time period}
#'   \item{loss}{count of losses in the specif league and time period}
#'   \item{count}{count of tips in the specif league and time period}
#' }
#' @export
#'
#' @examples
#' get_stats_table(tippers_id = c(184328, 184329))
get_stats_table <- function(tippers_id = NA, verbose = FALSE) {
  column_names <- c("tipper_id", "league", "period", "win", "loss", "count")
  column_types <- c("integer", "character", "character", "integer",
                    "integer", "integer")
  furrr::future_map_dfr(tippers_id, function(tipper_id) {
    tryCatch({
      get_json(tipper_id) %>%
        purrr::pluck("props", "pageProps", "profile") %>%
        unlist() %>% tibble::enframe() %>%
        dplyr::filter(stringr::str_detect(name, "user_id|pick_stats"),
                      stringr::str_detect(name, "user_id|win|loss|count"),
                      stringr::str_detect(name, "user_id|mlb|mlb|nba|nfl|nhl|ncaab"),
                      stringr::str_detect(name, "user_id|records"),
                      !stringr::str_detect(name, "today|yesterday|start|verified|[Pp]icks")) %>%
        tidyr::pivot_wider(dplyr::everything()) %>%
        dplyr::rename_all(list(~ stringr::str_remove(., "pick_stats.pick_stats."))) %>%
        tidyr::pivot_longer(-user_id, names_to = "name", values_to = "value") %>%
        dplyr::mutate(name = stringr::str_remove(name, ".records")) %>%
        tidyr::separate(name, into = c("league", "period", "stat"), sep = r"{\.}") %>%
        tidyr::pivot_wider(names_from = stat, values_from = value) %>%
        dplyr::rename(tipper_id = user_id) %>%
        dplyr::mutate_at(c(1, 4:6), ~ as.integer(.)) %>%
        dplyr::mutate_at(2:3, ~ as.character(.)) %>%
        dplyr::select(tipper_id, league, period, win, loss, count)
    }, error = function(e) empty_tibble(column_names, column_types))
  }, .progress = verbose)
}

#' Tippers Tips Table
#'
#' Returns a tibble with id, league, periods and W/L of tips
#'
#' @param tippers_id A vector with all tippers id's you want tips.
#' @param verbose Logical indication showing scrapping progress bar
#'
#' @return a \code{tibble} with 6 columns
#' \describe{
#'   \item{tip_id}{unique id of the tip}
#'   \item{tipper_id}{unique id of the tipper}
#'   \item{game_id}{unique id of the game where the tip is on}
#'   \item{created_at}{date where the tip has created}
#'   \item{updated_at}{date where the tip has updated}
#'   \item{league}{league name of the game}
#'   \item{tip_play}{the tip play}
#'   \item{tip_type}{the tip type}
#' }
#' @export
#'
#' @examples
#' get_tips_table(tippers_id = c(184328, 184329))
get_tips_table <- function(tippers_id = NA, verbose = FALSE) {
  column_names <- c("tip_id", "tipper_id", "game_id", "created_at",
                    "updated_at", "league", "tip_play", "tip_type")
  column_types <- c("integer", "integer", "integer", "character",
                    "character", "character", "character", "character")
  furrr::future_map_dfr(tippers_id, function(tipper_id) {
    tryCatch({
      get_json(tipper_id) %>%
        purrr::pluck("props", "pageProps", "profile", "picks") %>%
        dplyr::transmute(tip_id = id, tipper_id = user_id, game_id,
                         created_at, updated_at, league = league_name,
                         tip_play = play, tip_type = type) %>%
        dplyr::mutate_at(1:3, ~ as.integer(.)) %>%
        dplyr::mutate_at(4:8, ~ as.character(.))
    }, error = function(e) empty_tibble(column_names, column_types))
  }, .progress = verbose) %>% dplyr::mutate_at(4:5, ~ lubridate::ymd_hms(.))
}
