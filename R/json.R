library(rvest)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(janitor)
library(jsonlite)
library(glue)

get_json <- function(tipper_id) {
  glue('https://www.actionnetwork.com/picks/profile/{tipper_id}') |>
    read_html() |>
    html_node(xpath = '//*[@id="__NEXT_DATA__"]') |>
    html_text() |>
    fromJSON()
}

get_tipper_table <- function(json) {
  json |>
    pluck("props", "pageProps", "profile") |>
    unlist() |> enframe() |>
    filter(str_count(name, r"{\.}") <= 1) |>
    pivot_wider(everything()) |>
    transmute(tipper_id = user_id, name, is_expert, is_author,
              is_verified, num_followers = num_followers.total)
}

get_stats_table <- function(json) {
  json |>
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
}

get_picks_table <- function(json) {
  json |>
    pluck("props", "pageProps", "profile", "picks") |>
    transmute(pick_id = id, tipper_id = user_id, game_id,
              created_at, updated_at, league = league_name,
              tip_play = play, tip_type = type)
}
