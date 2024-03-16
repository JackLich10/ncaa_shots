
# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(collapse)))
suppressMessages(suppressWarnings(library(purrr)))
suppressMessages(suppressWarnings(library(rvest)))
suppressMessages(suppressWarnings(library(readr)))

html <- rvest::read_html("https://stats.ncaa.org/contests/livestream_scoreboards")

live_games <- html %>%
  rvest::html_elements("#contentarea div .skipMask") %>%
  rvest::html_attr("href")

live_game_ids <- as.integer(unlist(stringr::str_extract_all(live_games, "\\d+")))

if (length(live_game_ids) == 0L) {
  quit(status = 0)
}

get_ncaa_shots <- function(game_id) {

  cat("GameId:", game_id, "..\n")

  # Game link
  url <- paste0("https://stats.ncaa.org/contests/livestream_scoreboards/", game_id, "/box_score")

  # Read HTML
  html <- rvest::read_html(url)

  # PBP
  plays <- html %>%
    rvest::html_elements("#contest_plays_data_table") %>%
    rvest::html_table() %>%
    purrr::pluck(1)

  if (is.null(plays)) {
    cat("No PBP available...\n")
    return(list(shots = data.frame(), pbp = data.frame()))
  }

  if (nrow(plays) == 0) {
    cat("No PBP available...\n")
    return(list(shots = data.frame(), pbp = data.frame()))
  }

  # Today's date
  today <- as.Date(as.POSIXct(format(Sys.Date()), tz = "EST"))

  # teams
  team_info <- html %>%
    rvest::html_elements(".grey_text .skipMask")

  team_ids <- team_info %>%
    rvest::html_attr("href") %>%
    stringr::str_extract("\\d{5,8}") %>%
    as.integer()

  team_names <- team_info %>%
    rvest::html_text()

  away_id <- team_ids[1]
  home_id <- team_ids[2]

  home_away_teams <- data.frame(
    full_name = team_names,
    team_id = c(away_id, home_id)
  )

  team_options <- html %>%
    rvest::html_elements("#team_select") %>%
    rvest::html_elements("option")

  teams <- data.frame(
    shot_team_id = as.integer(rvest::html_attr(team_options, "value")),
    team_name = rvest::html_text(team_options)
  ) %>%
    collapse::fsubset(!is.na(shot_team_id))
  plays$game_id <- game_id
  plays$home_id <- home_id
  plays$away_id <- away_id
  plays$game_date <- today

  # Shot text
  shot_text <- html %>%
    rvest::html_elements("script") %>%
    rvest::html_text() %>%
    stringr::str_subset("^\\n. addShot") %>%
    stringr::str_squish() %>%
    stringr::str_split(", ")

  # Parse shots
  shots <- data.frame()
  for (shot in seq_along(shot_text)) {
    L <- stringr::str_remove(shot_text[[shot]], "^addShot\\(")
    shots <- dplyr::bind_rows(shots, as.data.frame(t(L)))
  }

  colnames(shots)[which(colnames(shots) == "V1")] <- "loc_y"
  colnames(shots)[which(colnames(shots) == "V2")] <- "loc_x"
  colnames(shots)[which(colnames(shots) == "V3")] <- "shot_team_id"
  colnames(shots)[which(colnames(shots) == "V4")] <- "shot_outcome"
  colnames(shots)[which(colnames(shots) == "V5")] <- "play_id"
  colnames(shots)[which(colnames(shots) == "V6")] <- "description"

  shots_clean <- dplyr::select(shots, -dplyr::starts_with("V"))
  shots_clean$shot_team_id <- as.integer(shots_clean$shot_team_id)
  shots_clean$loc_x <- as.numeric(shots_clean$loc_x) / 2L
  shots_clean$loc_y <- as.numeric(shots_clean$loc_y) * 94L/100L
  shots_clean$shot_outcome <- as.logical(shots_clean$shot_outcome)

  shots_clean$loc_x <- dplyr::case_when(
    shots_clean$loc_y >= 47 ~ 50 - shots_clean$loc_x,
    shots_clean$loc_y < 47 ~ shots_clean$loc_x
  )
  shots_clean$loc_y <- dplyr::case_when(
    shots_clean$loc_y >= 47 ~ 94 - shots_clean$loc_y,
    shots_clean$loc_y < 47 ~ shots_clean$loc_y
  )
  shots_clean$game_id <- game_id
  shots_clean$home_id <- home_id
  shots_clean$away_id <- away_id
  shots_clean$game_date <- today

  cross <- dplyr::cross_join(home_away_teams, teams)
  cross$similarity <- stringdist::stringsim(a = cross$full_name, b = cross$team_name)
  team_dict <- data.frame()
  for (tm in teams$team_name) {
    team_dict <- dplyr::bind_rows(team_dict, dplyr::slice_max(collapse::fsubset(cross, team_name == tm & !full_name %in% team_dict$full_name), similarity, n = 1L))
  }
  team_dict$similarity <- NULL

  shots_clean <- dplyr::left_join(shots_clean, team_dict, by = "shot_team_id")
  # shots_clean$home_shot <- as.integer(shots_clean$team_id == shots_clean$home_id)

  return(list(shots = shots_clean, pbp = plays))
}

pbp <- data.frame()
shots <- data.frame()
for (gid in live_game_ids) {
  res <- get_ncaa_shots(game_id = gid)
  pbp <- dplyr::bind_rows(pbp, res[["pbp"]])
  shots <- dplyr::bind_rows(shots, res[["shots"]])
}

readr::write_csv(pbp, here::here(paste0("data/pbp", Sys.time(), ".csv")))
readr::write_csv(shots, here::here(paste0("data/shots", Sys.time(), ".csv")))
