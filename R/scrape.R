### Scrape NCAA game data

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(XML))

# Source helpers
source(here::here("R/utils.R"))

current_season <- get_current_season()

option_list <- list(
  make_option("--gender", type = "character", default = "mens", help = "either mens or womens, default mens"),
  make_option("--season", type = "integer", default = current_season, help = "season, default current_season")
)
opt <- parse_args(OptionParser(option_list = option_list))

cat(paste0("---", tools::toTitleCase(opt$gender), " NCAA game scrape---\n"))

# Get NCAA games
get_ncaa_games <- function(date = Sys.Date()-1L, gender) {

  sn_division_id <- dplyr::case_when(
    # 24-25 Mens
    gender == "mens" & date > as.Date("2024-05-01") & date <= as.Date("2025-05-01") ~ 18403L,
    # 24-25 Womens
    gender == "womens" & date > as.Date("2024-05-01") & date <= as.Date("2025-05-01") ~ 18423L,
    # 23-24 Mens
    gender == "mens" & date > as.Date("2023-05-01") & date <= as.Date("2024-05-01") ~ 18221L,
    # 23-24 Womens
    gender == "womens" & date > as.Date("2023-05-01") & date <= as.Date("2024-05-01") ~ 18220L,
    TRUE ~ 0L
  )

  if (sn_division_id == 0L) {
    cat(paste0("No season division ID found for ", gender, " ", date, "\n"))
    return(data.frame())
  }

  # Reads the html and pulls the table holding the scores
  url_text <- paste0("https://stats.ncaa.org/season_divisions/", sn_division_id, "/scoreboards?game_date=", date, "&conference_id=0&commit=Submit")
  file_url <- url(url_text, headers = c("User-Agent" = "My Custom User Agent"))
  html <- readLines(con = file_url, warn = FALSE)
  close(file_url)

  table <- XML::readHTMLTable(html)
  if (length(table) == 0) {
    cat(paste0("No games found for ", gender, " ", date, "\n"))
    return(data.frame())
  } else {
    table <- table[[1L]]
  }

  # The table is always read in the same messy way
  # Each game in the schedule starts on a row following pattern 1,6,11,etc. this gets all of those indices
  starting_rows <- (1:(nrow(table) / 5)) * 5 - 4

  # Pull game meta data from relevant part of the table
  game_date <- as.character(table$V1[starting_rows])

  if (as.Date(substr(game_date[1], 1, 10), format = "%m/%d/%Y") != date) {
    cat(paste0("No games found for ", gender, " ", date, "\n"))
    return(data.frame())
  }

  attendance <- as.character(table$V7[starting_rows])

  away_team <- as.character(table$V3[starting_rows])
  home_team <- as.character(table$V2[starting_rows + 3])

  home_score <- as.character(table$V3[starting_rows + 3])
  away_score <- as.character(table$V5[starting_rows])

  # sees if a box score is available for each game
  box_score_present <- as.character(table$V1[starting_rows + 4]) == "Box Score"

  # This searches for all game IDs on the schedule page, using links found in the html
  game_ids <- unlist(stringr::str_extract_all(html, "(?<=/contests/)\\d+(?=/box_score)"))
  game_ids <- game_ids[which(!game_ids %in% sn_division_id)]

  # Handle cancelled games with missing game ids, or if game is missing a box score
  id_found <- rep(NA, length(away_score))
  id_found[!away_score %in% c("Canceled", "Ppd") & box_score_present] <- game_ids

  # Also creates variable used to find if a game was held at a neutral side
  isNeutral <- table$V6[starting_rows] != ""

  # Informs user of how many games and games with a relevant ID were found
  cat(paste(date, "|", length(game_ids), "games found...\n"))

  # Clean team names (remove records, like "Rutgers (1-0)")
  # home_name = gsub(" [(][0-9]{1-2}\\-[0-9]{1-2}[)]","", home_team)
  home_name = gsub(" \\([0-9].+","", home_team) |> gsub(pattern = '\\#[0-9]{1,2} ', replacement = "")

  # away_name = gsub(" [(][0-9]{1-2}\\-[0-9]{1-2}[)]","", away_team)
  away_name = gsub(" \\([0-9].+","", away_team) |> gsub(pattern = '\\#[0-9]{1,2} ', replacement = "")

  # Create dataframe
  game_data <- data.frame(
    game_date = date,
    # start_time = substr(game_date, 12, 19),
    home_team = home_name,
    away_team = away_name,
    game_id = as.integer(id_found),
    home_score = as.integer(home_score),
    away_score = as.integer(away_score),
    attendance = as.integer(stringr::str_remove(attendance, ",")),
    neutral = as.integer(isNeutral)
  )

  return(game_data)
}

# Dates to scrape
start <- as.Date(paste0(opt$season-1L, "-11-01"))
if (opt$season == current_season) {
  dates <- as.Date(start:(Sys.Date() - 1L), origin = "1970-01-01")
} else {
  dates <- as.Date(start:as.Date(paste0(opt$season, "-04-20")), origin = "1970-01-01")
}

# Read current games data
current_games <- readr::read_csv(here::here(paste0("data/", opt$gender, "/games.csv")), col_types = readr::cols())

# Remove already scraped game dates
if (nrow(current_games) > 0L) {
  dates <- as.Date(setdiff(dates, unique(current_games$game_date)), origin = "1970-01-01")
}

# Scrape game schedules
games <- purrr::map_df(dates, function(date) {
  g <- get_ncaa_games(date = date, gender = opt$gender)
  Sys.sleep(0.5 + runif(n = 1L))
  return(g)
})

cat("Scraped", nrow(games), "new", tools::toTitleCase(opt$gender), "games...\n")

if (nrow(current_games) > 0L) {
  games <- dplyr::distinct(dplyr::bind_rows(games, current_games), game_id, .keep_all = TRUE)
}

# Write games
readr::write_csv(games, here::here(paste0("data/", opt$gender, "/games.csv")))

# Scrape NCAA shot locations
get_ncaa_shots <- function(game_id, gender) {
  cat("GameId:", game_id, "...\n")

  # Game link
  url <- paste0("https://stats.ncaa.org/contests/", game_id, "/box_score")

  # Read HTML
  html <- rvest::read_html(url)

  # # PBP
  # plays <- html %>%
  #   rvest::html_elements("#contest_plays_data_table") %>%
  #   rvest::html_table() %>%
  #   purrr::pluck(1)
  #
  # if (is.null(plays)) {
  #   cat("No PBP available...\n")
  #   return(list(shots = data.frame(), pbp = data.frame()))
  # }
  #
  # if (nrow(plays) == 0) {
  #   cat("No PBP available...\n")
  #   return(list(shots = data.frame(), pbp = data.frame()))
  # }

  # teams
  team_info <- rvest::html_elements(html, ".grey_text .skipMask")

  team_ids <- team_info %>%
    rvest::html_attr("href") %>%
    stringr::str_extract("\\d{5,8}") %>%
    as.integer()

  if (length(team_ids) != 2L) {
    cat(length(team_ids), "team_id(s) found for game_id", game_id, "...\n")
    bad <- try(readr::read_csv(here::here(paste0("data/", opt$gender, "/bad_shots.csv")), col_types = readr::cols()), silent = TRUE)
    if ("try-error" %in% class(bad)) {
      readr::write_csv(data.frame(game_id = game_id), here::here(paste0("data/", opt$gender, "/bad_shots.csv")))
    } else {
      readr::write_csv(dplyr::distinct(dplyr::bind_rows(bad, data.frame(game_id = game_id)), game_id), here::here(paste0("data/", opt$gender, "/bad_shots.csv")))
    }
    return(data.frame())
  }

  team_names <- rvest::html_text(x = team_info)

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
  # plays$game_id <- game_id
  # plays$home_id <- home_id
  # plays$away_id <- away_id

  # Shot text
  shot_text <- html %>%
    rvest::html_elements("script") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    stringr::str_subset("^addShot\\(") %>%
    stringr::str_remove_all("'") %>%
    stringr::str_split(" addShot\\(") %>%
    purrr::pluck(1L) %>%
    stringr::str_remove("^addShot\\(") %>%
    stringr::str_remove("\\);.*$") %>%
    stringr::str_replace_all(", Jr\\.", " Jr.") %>%
    stringr::str_replace_all(", Sr\\.", " Sr.") %>%
    stringr::str_replace_all(", II", " II") %>%
    stringr::str_replace_all(", III", " III") %>%
    stringr::str_replace_all(", IV", " IV") %>%
    stringr::str_replace_all("&#39;", "'") %>%
    stringr::str_replace_all("&amp;", "&") %>%
    stringr::str_split(", ")

  if (rlang::is_empty(shot_text)) {
    cat("No shots found for game_id ", game_id, "...\n")
    bad <- try(readr::read_csv(here::here(paste0("data/", opt$gender, "/bad_shots.csv")), col_types = readr::cols()), silent = TRUE)
    if ("try-error" %in% class(bad)) {
      readr::write_csv(data.frame(game_id = game_id), here::here(paste0("data/", opt$gender, "/bad_shots.csv")))
    } else {
      readr::write_csv(dplyr::distinct(dplyr::bind_rows(bad, data.frame(game_id = game_id)), game_id), here::here(paste0("data/", opt$gender, "/bad_shots.csv")))
    }
    return(data.frame())
  }

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

  shots_clean <- tidyr::separate(shots, col = V7, into = c("period", "player_id", NA), sep = " ", convert = TRUE) %>%
    dplyr::select(-dplyr::starts_with("V")) %>%
    tidyr::separate(col = description, into = c("time", "shooter"), sep = " : ")
  shots_clean$shot_team <- stringr::str_extract(shots_clean$shooter, "\\(([^)]+)\\)")
  shots_clean$shot_team <- stringr::str_remove_all(shots_clean$shot_team, "[()]")
  shots_clean$shooter <- stringr::str_remove(shots_clean$shooter, "^(made|missed) by ")
  shots_clean$shooter <- stringr::str_remove(shots_clean$shooter, "\\(([^)]+)\\)")
  shots_clean$score <- stringr::str_match(shots_clean$shooter, "(\\d+-\\d+)$")[,2L]
  shots_clean$shooter <- stringr::str_remove(shots_clean$shooter, paste0(" ", shots_clean$score))
  shots_clean <- tidyr::separate(shots_clean, col = score, into = c("away_score", "home_score"), sep = "-", convert = TRUE)
  shots_clean$shot_team_id <- as.integer(shots_clean$shot_team_id)
  shots_clean$loc_x <- as.numeric(shots_clean$loc_x) / 2L
  shots_clean$loc_y <- as.numeric(shots_clean$loc_y) * 94L/100L
  shots_clean$play_id <- as.numeric(shots_clean$play_id)
  shots_clean$shot_outcome <- as.logical(shots_clean$shot_outcome)
  shots_clean$shot_outcome <- ifelse(shots_clean$shot_outcome, "made", "missed")
  shots_clean$period <- as.integer(stringr::str_remove(shots_clean$period, "period_"))
  if (gender == "mens") {
    colnames(shots_clean)[which(colnames(shots_clean) == "period")] <- "half"
    shots_clean$half <- ifelse(shots_clean$half > 2L, 3L, shots_clean$half)
    shots_clean$half_secs_remaining <- round(60L * as.integer(substr(shots_clean$time, start = 5L, stop = 6L)) + as.integer(substr(shots_clean$time, start = 8L, stop = 9L))) # + as.integer(substr(shots_clean$time, start = 11L, stop = 12L))/100L)
    shots_clean$game_secs_remaining <- (2L-shots_clean$half)*20L*60L + shots_clean$half_secs_remaining
    # Fix for OT
    shots_clean$game_secs_remaining <- ifelse(!shots_clean$half %in% c(1, 2),
                                              shots_clean$half_secs_remaining,
                                              shots_clean$game_secs_remaining)

  } else {
    colnames(shots_clean)[which(colnames(shots_clean) == "period")] <- "qtr"
    shots_clean$half <- ifelse(shots_clean$qtr <= 2L, 1L, ifelse(shots_clean$qtr <= 4L, 2L, 3L))
    shots_clean$qtr_secs_remaining <- round(60L * as.integer(substr(shots_clean$time, start = 5L, stop = 6L)) + as.integer(substr(shots_clean$time, start = 8L, stop = 9L))) # + as.integer(substr(shots_clean$time, start = 11L, stop = 12L))/100L)
    shots_clean$half_secs_remaining <- ifelse(shots_clean$qtr %in% c(1L, 3L), 600L, 0L) + shots_clean$qtr_secs_remaining
    shots_clean$game_secs_remaining <- (4L-shots_clean$qtr)*10L*60L + shots_clean$qtr_secs_remaining
    # Fix for OT
    shots_clean$game_secs_remaining <- ifelse(!shots_clean$qtr %in% c(1L, 2L, 3L, 4L),
                                              shots_clean$qtr_secs_remaining,
                                              shots_clean$game_secs_remaining)
  }
  shots_clean$time <- NULL
  shots_clean$player_id <- as.integer(stringr::str_remove(shots_clean$player_id, "player_"))

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

  cross <- dplyr::cross_join(home_away_teams, teams)
  cross$similarity <- stringdist::stringsim(a = cross$full_name, b = cross$team_name)
  team_dict <- dplyr::slice_max(dplyr::slice_max(cross, order_by = similarity, by = team_name, n = 1L), order_by = similarity, by = team_id, n = 1L)
  if (length(unique(team_dict$shot_team_id)) == 1L) {
    team_dict <- dplyr::distinct(dplyr::slice_max(cross, order_by = similarity, by = team_name, n = 1L), shot_team_id, .keep_all = TRUE)
  }
  team_dict$similarity <- NULL

  shots_clean <- dplyr::left_join(shots_clean, team_dict, by = "shot_team_id")

  return(shots_clean)
}

# Read games data
games <- readr::read_csv(here::here(paste0("data/", opt$gender, "/games.csv")), col_types = readr::cols())
dict <- readr::read_csv(here::here("data/team_dict.csv"), col_types = readr::cols())
games_proc <- dplyr::inner_join(games, dplyr::rename(dict, home_id = espn_team_id), by = c("home_team" = "ncaa_team_name")) %>%
  dplyr::inner_join(dplyr::rename(dict, away_id = espn_team_id), by = c("away_team" = "ncaa_team_name")) %>%
  collapse::fsubset(!is.na(game_id))

# Read current shots data
current_shots <- readRDS(here::here(paste0("data/", opt$gender, "/shots.rds")))

# Read previously failed shots data
bad_games <- readr::read_csv(here::here(paste0("data/", opt$gender, "/bad_shots.csv")), col_types = readr::cols())

# Remove already scraped games
if (nrow(current_shots) > 0L) {
  gids <- setdiff(games_proc$game_id, unique(current_shots$game_id))
} else {
  gids <- unique(games_proc$game_id)
}
gids <- setdiff(gids, unique(bad_games$game_id))

# Scrape shots
shots <- purrr::map_df(gids, function(gid) {
  g <- get_ncaa_shots(game_id = gid, gender = opt$gender)
  Sys.sleep(0.75 + runif(n = 1L))
  return(g)
})

cat("Scraped", length(unique(shots$game_id)), "new", tools::toTitleCase(opt$gender), "games...\n")

if (nrow(current_shots) > 0L) {
  shots <- dplyr::distinct(dplyr::bind_rows(shots, current_shots), play_id, .keep_all = TRUE)
}

# Write shots
saveRDS(shots, here::here(paste0("data/", opt$gender, "/shots.rds")))
# readr::write_csv(shots, here::here(paste0("data/", opt$gender, "/shots.csv")))

quit(status = 0)
