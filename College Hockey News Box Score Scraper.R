library(tidyverse)
library(rvest)
library(progress)

## Gets URLs for non-exhibition games since 2002-03
get_schedule <- function(league, season, ..., progress = TRUE) {
  
  if (any(season < 2003 | season > 2019)) {
    
    cat("\n")
    
    stop("\n\nEarliest season is 2003. Latest season is 2019\n\n")
    
  }
  
  else if (any(nchar(season) > 4) | any(!stringr::str_detect(season, "[0-9]{4,4}"))) {
    
    cat("\n")
    
    stop('\n\nMake sure your seasons are all 4-digit numbers
         \rlike 2003 (for 2002-03) and 2019 (for 2018-19)\n\n')
    
  }
  
  
  leagues <- league %>%
    as_tibble() %>%
    purrr::set_names("league") %>%
    mutate_all(toupper) %>%
    distinct()
  
  seasons <- season %>%
    as_tibble() %>%
    purrr::set_names("season") %>%
    distinct()
  
  mydata <- tidyr::crossing(leagues, seasons)
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_schedule() [:bar] :percent ETA: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    
    cat("\n")
    
    pb$tick(0)
    
  }
  
  .get_schedule <- function(league, season, ...) {
    
    if (league == "NCAA") {  
      
      url <- str_c("https://www.collegehockeynews.com/schedules/?season=", season - 1, season, sep = "")

    }
    
    else {stop("League not available. Sorry!")}
    
    seq(0.1, 0.4, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    page <- url %>% read_html()
    
    url_prefix <- case_when(league == "NCAA" ~ "https://www.collegehockeynews.com")
    
    season <- str_c(season - 1, str_sub(season, 3, 4), sep = "-")
    
    game_urls <- page %>%
      html_nodes(".b a") %>%
      html_attr("href") %>%
      str_c(url_prefix, .) %>%
      as_tibble() %>%
      set_names("url")
    
    schedule <- game_urls %>%
      mutate(league = league) %>%
      mutate(season = season)
    
    if (progress) {pb$tick()}
    
    return(schedule)
    
  }
  
  schedule_data <- map2_dfr(mydata[["league"]], mydata[["season"]], .get_schedule)
  
  cat("\n")
  
  return(schedule_data)
  
}

## Gets URLs for games from 2016-17 to 2018-19
## Change as necessary
games <- get_schedule("NCAA", 2017:2019)

## Gets Box Score Scoring Summaries for URLs (retrieved from get_schedule())
get_box_score <- function(..., progress = TRUE) {
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_box_score() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(...), show_after = 0) 
    
    cat("\n")
    
    pb$tick(0)}
  
  .get_box_score <- function(url, league, season, ...) {
    
    seq(0.1, 0.4, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    page <- url %>% read_html()
    
    time <- page %>%
      html_nodes("#scoring td:nth-child(6)") %>%
      html_text() %>%
      as_tibble() %>%
      purrr::set_names("time")
    
    teams <- page %>%
      html_nodes("#scoring .hscore td:nth-child(1) , #scoring .vscore td:nth-child(1)") %>%
      html_text() %>%
      as_tibble() %>%
      purrr::set_names("team")
    
    game_strength <- page %>%
      html_nodes("#scoring td:nth-child(2)") %>%
      html_text() %>%
      as_tibble() %>%
      purrr::set_names("game_strength")
    
    goals <- page %>%
      html_nodes("#scoring td:nth-child(4)") %>%
      html_text() %>%
      as_tibble() %>%
      purrr::set_names("goal") %>%
      mutate(goal = str_split(goal, "\\s\\([0-9]{1,}\\)", simplify = TRUE, n = 2)[,1])
    
    assists <- page %>%
      html_nodes("#scoring td:nth-child(5)") %>%
      html_text() %>%
      as_tibble() %>%
      purrr::set_names("assists") %>%
      mutate(primary_assist = str_split(assists, ",", simplify = TRUE, n = 2)[,1]) %>%
      mutate(secondary_assist = str_split(assists, ",\\s", simplify = TRUE, n = 2)[,2]) %>%
      select(primary_assist, secondary_assist)
    
    box_score_data <- time %>%
      bind_cols(teams) %>%
      bind_cols(game_strength) %>%
      bind_cols(goals) %>%
      bind_cols(assists) %>%
      mutate(league = league) %>%
      mutate(season = season) %>%
      mutate(game_url = url) %>%
      mutate_all(str_squish) %>%
      mutate_all(~na_if(., ""))
    
    if (progress) {pb$tick()}
    
    return(box_score_data)
    
  }
  
  persistently_get_box_score <- elite::persistently(.get_box_score, max_attempts = 10, wait_seconds = 0.0001)
  
  try_get_box_score <- function(url, league, season, ...) {
    
    tryCatch(persistently_get_box_score(url, league, season, ...), 
             
             error = function(e) {
               print(e) 
               print(url)
               data_frame()},
             
             warning = function(w) {
               print(w) 
               print(url)
               data_frame()})
  }
  
  
  all_box_score_data <- pmap_dfr(..., try_get_box_score)
  
  cat("\n")
  
  return(all_box_score_data)
  
}

## Gets scoring data for games from 2016-17 to 2018-19
scoring_data <- get_box_score(games)
