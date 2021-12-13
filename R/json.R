#' Tipper Raw JSON
#'
#' Returns raw json from specific action network tipper
#'
#' @param tipper_id The id of the tipper you want.
#'
#' @return a \code{list}
#' @export
#'
#' @importFrom rvest read_html html_node html_text
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @importFrom glue glue
#' @importFrom tibble enframe
#' @importFrom stringr str_count str_detect str_remove
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @importFrom dplyr mutate rename_all transmute everything
#' filter rename
#'
#' @examples
#' get_json(tipper_id = "184328")
get_json <- function(tipper_id) {
  tryCatch({
    glue('https://www.actionnetwork.com/picks/profile/{tipper_id}') |>
      read_html() |>
      html_node(xpath = '//*[@id="__NEXT_DATA__"]') |>
      html_text() |>
      fromJSON()
  }, error = function(e) NULL)
}

#' Tippers Table Summary
#'
#' Returns a tibble with id, name and others informations of each
#' tipper requested
#'
#' @param tippers_id A vector with all tippers id's you want information.
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
#' @importFrom purrr pluck map_dfr
#' @importFrom tibble enframe
#' @importFrom stringr str_count
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr transmute everything filter
#'
#' @examples
#' get_tippers_table(tippers_id = c(184328, 184329))
get_tippers_table <- function(tippers_id) {
  column_names <- c("tipper_id", "name", "is_expert", "is_author",
                    "is_verified", "num_followers")
  map_dfr(tippers_id, function(tipper_id) {
    tryCatch({
      get_json(tipper_id) |>
        pluck("props", "pageProps", "profile") |>
        unlist() |> enframe() |>
        filter(str_count(name, r"{\.}") <= 1) |>
        pivot_wider(everything()) |>
        transmute(tipper_id = user_id, name, is_expert, is_author,
                  is_verified, num_followers = num_followers.total)
    }, error = function(e) empty_tibble(column_names))
  })
}

#' Tippers Stats Table
#'
#' Returns a tibble with id, league, periods and W/L of tips
#'
#' @param tippers_id A vector with all tippers id's you want stats.
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
#' @importFrom purrr pluck map_dfr
#' @importFrom tibble enframe
#' @importFrom stringr str_detect str_remove
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @importFrom dplyr mutate everything
#' filter rename
#'
#' @examples
#' get_stats_table(tippers_id = c(184328, 184329))
get_stats_table <- function(tippers_id) {
  column_names <- c("tipper_id", "league", "period", "win", "loss", "count")
  map_dfr(tippers_id, function(tipper_id) {
    tryCatch({
      get_json(tipper_id) |>
        pluck("props", "pageProps", "profile") |>
        unlist() |> enframe() |>
        filter(str_detect(name, "user_id|pick_stats"),
               str_detect(name, "user_id|win|loss|count"),
               str_detect(name, "user_id|mlb|mlb|nba|nfl|nhl|ncaab"),
               str_detect(name, "user_id|records"),
               !str_detect(name, "today|yesterday|start|verified|picks")) |>
        pivot_wider(everything()) |>
        rename_all(list(~ str_remove(., "pick_stats.pick_stats."))) |>
        pivot_longer(-user_id, names_to = "name", values_to = "value") |>
        mutate(name = str_remove(name, ".records")) |>
        separate(name, into = c("league", "period", "stat"), sep = r"{\.}") |>
        pivot_wider(names_from = stat, values_from = value) |>
        rename(tipper_id = user_id)
    }, error = function(e) empty_tibble(column_names))
  })
}

#' Tippers Tips Table
#'
#' Returns a tibble with id, league, periods and W/L of tips
#'
#' @param tippers_id A vector with all tippers id's you want tips.
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
#' @importFrom purrr pluck map_dfr
#' @importFrom dplyr transmute
#'
#' @examples
#' get_tips_table(tippers_id = c(184328, 184329))
get_tips_table <- function(tippers_id) {
  column_names <- c("tip_id", "tipper_id", "game_id", "created_at",
                    "updated_at", "league", "tip_play", "tip_type")
  map_dfr(tippers_id, function(tipper_id) {
    tryCatch({
      get_json(tipper_id) |>
        pluck("props", "pageProps", "profile", "picks") |>
        transmute(tip_id = id, tipper_id = user_id, game_id,
                  created_at, updated_at, league = league_name,
                  tip_play = play, tip_type = type)
    }, error = function(e) empty_tibble(column_names))
  })
}
