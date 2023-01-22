setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
set.seed(123) # not really necessary, only to avoid warnings.
full_scrape <- function(url, p) {
  p()
  Sys.sleep(runif(1))
  tryCatch(
    expr = {
      article <- rvest::read_html(httr::GET(url))
      
      data.frame(
        url         = url,
        date        = get_date(article),
        date_alt    = NA_character_,
        title       = get_title(article),
        author      = get_meta(article, "author"),
        description = get_meta(article, "description"),
        keywords    = get_meta(article, "keywords"),
        body        = get_body(article),
        error       = NA_character_
      ) |> 
        mutate(date_alt = ifelse(is.na(date), get_date2(article), date_alt))
    },
    error = function(e) {
      e <- as.character(e[1])
      if (length(e) > 1)
        e <- paste(e, collapse = " ")
      
      data.frame(
        url         = url,
        date        = NA_character_,
        date_alt    = NA_character_,
        title       = NA_character_,
        author      = NA_character_,
        description = NA_character_,
        keywords    = NA_character_,
        body        = NA_character_,
        error       = e
      )
    }
  )
}

with_progress({
  p <- progressor(steps = length(article_urls))
  taz_full <- article_urls |> future_map(~ full_scrape(.x, p = p))
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
