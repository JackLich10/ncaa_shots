
# Libraries
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(readr))

# Dates to clean
good_dates <- paste0("(", paste0(seq(Sys.Date()-7, Sys.Date(), by = "day"), collapse = ")|("), ")")

for (type in c("shots", "pbp")) {

  # Files to clean
  files <- list.files(here::here(paste0("data/", type))) %>%
    stringr::str_subset(good_dates)

  df <- purrr::map_df(files, function(file) {
    df <- readr::read_csv(here::here(paste0("data/", type), file), col_types = readr::cols())
    df$time <- stringr::str_remove(file, "(shots)|(pbp)")
    df$time <- stringr::str_remove(df$time, "\\.csv$")
    return(df)
  })

  # Keep data with most rows by game
  to_keep <- df %>%
    dplyr::group_by(time, game_id) %>%
    dplyr::summarise(rows = dplyr::n(),
                     .groups = "drop") %>%
    dplyr::group_by(game_id) %>%
    dplyr::slice_max(rows) %>%
    dplyr::ungroup() %>%
    dplyr::select(game_id, time)

  df_clean <- dplyr::inner_join(df, to_keep, by = c("time", "game_id"))
  df_clean$time <- as.Date(substr(df_clean$time, start = 1, stop = 10))
  # To EST time from UTC
  df_clean$game_date <- dplyr::coalesce(df_clean$game_date, df_clean$time) - 5/24

  # Write out clean version
  readr::write_csv(df_clean, here::here(paste0("data/clean/", type, "/", Sys.time(), "")))
}

