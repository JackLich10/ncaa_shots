# Utility functions

get_current_season <- function() {
  season <- ifelse(as.integer(substr(Sys.Date(), 6, 7)) >= 11,
                   as.integer(substr(Sys.Date(), 1, 4)) + 1,
                   as.integer(substr(Sys.Date(), 1, 4)))
  return(season)
}

create_chromote_session <- function() {
  session <- chromote::ChromoteSession$new()
  session$Emulation$setUserAgentOverride(
    userAgent = paste(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      "AppleWebKit/537.36 (KHTML, like Gecko)",
      "Chrome/124.0.0.0 Safari/537.36"
    )
  )
  session$Network$enable()
  session$Network$setExtraHTTPHeaders(headers = list(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9",
    "Cache-Control" = "no-cache",
    "Pragma" = "no-cache",
    "Upgrade-Insecure-Requests" = "1",
    "Referer" = "https://stats.ncaa.org/"
  ))

  return(session)
}

scrape_dynamic_tables <- function(url, session = NULL, pause_ms = 2000L, return_as_html = TRUE) {
  if (is.null(session)) {
    delete_session <- TRUE
    session <- create_chromote_session()
  } else {
    delete_session <- FALSE
  }

  get_rendered_html <- function(session, use_node_id = FALSE) {
    # Prefer Runtime.evaluate to avoid nodeId altogether (see §3)
    if (isTRUE(use_node_id)) {
      doc_id   <- session$DOM$getDocument()$root$nodeId
      html <- session$DOM$getOuterHTML(nodeId = doc_id)
      html_txt <- session$DOM$getOuterHTML(nodeId = doc_id)$outerHTML
    } else {
      res <- session$Runtime$evaluate("document.documentElement.outerHTML")
      html_txt <- res$result$value
    }
    return(html_txt)
  }

  page_nav <- try(session$Page$navigate(url), silent = TRUE)
  Sys.sleep(pause_ms / 1000L)

  if (class(page_nav)[1] == 'try-error') {
    page_nav <- try(session$Page$navigate(url), silent = TRUE)
    Sys.sleep(2 * pause_ms / 1000L)
  }

  table_success <- FALSE
  retries <- 0L
  while (!table_success && retries < 5L) {
    session$Runtime$evaluate('new Promise(r => {
      const sel = "table";                                // TODO: tighten this to the exact table selector
      (function check(){ document.querySelector(sel) ? r(1) : setTimeout(check, 200); })();
    })')

    html_txt <- get_rendered_html(session = session)
    page <- rvest::read_html(html_txt)
    tables <- page %>% rvest::html_table()
    if (length(tables) > 0L) {
      table_success <- TRUE
    } else {
      retries <- retries + 1L
      Sys.sleep(pause_ms / 1000L)
    }
  }

  if (isTRUE(delete_session)) {
    session$close()
  }

  if (isTRUE(return_as_html)) {
    return(page)
  } else {
    return(tables)
  }
}
