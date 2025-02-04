# Utility functions

get_current_season <- function() {
  season <- ifelse(as.integer(substr(Sys.Date(), 6, 7)) >= 11,
                   as.integer(substr(Sys.Date(), 1, 4)) + 1,
                   as.integer(substr(Sys.Date(), 1, 4)))
  return(season)
}
