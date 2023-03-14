#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(rvest)
library(xml2)
library(purrr)
library(furrr)
library(stringr)
library(progressr)
library(lubridate)

url <- "https://taz.de/sitemap-index.xml"
sitemaps <- read_html(url) |> 
  html_elements("loc") |> 
  html_text2()

sitemaps <- sitemaps[grepl("2022|2021|2020", sitemaps)]

# Retrieve article urls:
get_article_urls <- function(sitemap, p) {
  p()
  tryCatch(
    expr = {
      urls <- sitemap |>
        read_html() |> 
        html_elements("url") |> 
        html_element("loc") |> 
        html_text2()
      if (rlang::is_empty(urls)) return()
      urls
    },
    error = function(e) return()
  )
}

# If the following syntax is unfamiliar, take a look at:
# furrr: https://furrr.futureverse.org/
# progressr: https://furrr.futureverse.org/articles/progress.html
plan(multisession, workers = parallel::detectCores())

handlers(
  handler_progress(
    format = "[:bar] Remaining: :eta"
  )
)

with_progress({
  p <- progressor(steps = length(sitemaps))
  article_urls <- sitemaps |> future_map(~ get_article_urls(.x, p = p))
})

article_urls <- do.call(c, article_urls)

# Functions for retrieval from html:
check2 <- function(x) ifelse(rlang::is_empty(x), NA_character_, x)

get_title <- function(src) {
  src |> 
    html_element("title") |> 
    html_text2() |> 
    check2()
}

get_meta <- function(src, query) {
  src |> 
    html_elements(xpath = paste0('//*[@name="', query, '"]')) |>
    xml_attr("content") |> 
    check2()
}

get_date <- function(src) {
  src |> 
    html_elements(xpath = '//li[@class="date"]') |> 
    xml_attr("content") |> 
    check2()
}

# Sometimes, the date is not included in the meta, but is more freely
# written in a div (e.g. "Monday, 28.11.2020, 18:59"). This function 
# tries to still extract it. 
get_date2 <- function(src) {
  src |> 
    html_elements("div") |> 
    html_elements(xpath = '//*[@class="dateLocWrapper"]') |> 
    html_text2() |> 
    check2()
}

patterns <- c(
  '//p[@class="article first odd"]',
  '//p[@class="article first odd initial"]',
  '//p[@class="article even"]', 
  '//p[@class="article odd"]',
  '//p[@class="article last even"]',
  '//p[@class="article last odd"]'
)

get_body <- function(src) {
  src |> 
    html_elements(xpath = paste(patterns, collapse = " | ")) |>
    html_text2() |> 
    paste(collapse = " ") |> 
    check2()
}

# Full scrape
scrape_article <- function(url) {
  article <- read_html(httr::GET(url))
  
  out <- data.frame(
    url         = url,
    title       = get_title(article),
    date        = get_date(article),
    date_alt    = NA_character_,
    description = get_meta(article, "description"),
    keywords    = get_meta(article, "keywords"),
    author      = get_meta(article, "author"),
    body        = get_body(article),
    error       = NA_character_
  ) 

  if (is.na(out$date))
    out$date_alt <- get_date2(article)
  
  out
}

# This is what to do if an error occurs:
error_handler <- function(url, error_obj) {
  # log url & error message:
  msg <- as.character(error_obj$message)
  df <- data.frame(url = url, error = msg)

  # fill everything else with NA:
  to_fill <- c(
    "title", "author", "date", "date_alt",
    "description", "keywords", "paywall", "body"
  )
  df[, to_fill] <- NA_character_

  df
}

# ...putting it together:
scrape_safely <- function(url, p) {
  p() 
  
  tryCatch(
    expr = scrape_article(url), 
    error = \(e) error_handler(url = url, error_obj = e)
  )
}

with_progress({
  p <- progressor(steps = length(article_urls))
  taz_full <- article_urls |> future_map(\(.x) scrape_safely(.x, p = p))
})

taz_full <- tibble(do.call(rbind, taz_full))

# Unifying date column from the freely extracted dates. Ideally,
# this would be dealt with in get_date2, a rewrite would be appreciated.
taz_full <- taz_full |> 
  mutate(
    date_alt = map_chr(date_alt, ~ {
      .x |>   
        str_remove_all("[a-zA-Z]|,") |> 
        trimws()
    }),
    time = trimws(str_extract(date_alt, "\\s(.*?)$")),
    date_alt = map_chr(date_alt, ~ {
      .x |> 
        str_remove("\\s(.*?)$") |> 
        str_replace_all("\\.", "-") |> 
        lubridate::dmy() |> 
        as.character()
    }),
    date_alt = ifelse(
      !is.na(date_alt),
      paste0(date_alt, "T", time, ":00+01:00"),
      date_alt
    ),
    date = ifelse(is.na(date), date_alt, date)
  ) |> 
  select(-c(date_alt, time)) |> 
  distinct()

# Also cleaning titles:
taz_full <- taz_full |> mutate(title = str_remove(title, " - taz.de$"))

write.csv(taz_full, "taz_full.csv")
