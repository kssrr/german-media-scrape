# Scraping all Spiegel Online articles from 01/2020 to 12/2022. 

library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(furrr)
library(progressr)

url <- "https://www.spiegel.de/sitemap.xml"
patterns <- "sitemap-2022-|sitemap-2021-|sitemap-2020-"

sitemaps <- 
  url |> 
  read_html() |> 
  html_elements("loc") |> 
  html_text2() |> 
  (\(.x) .x[grepl(patterns, .x)])()

# Pull article urls from sitemaps:
get_urls <- function(sitemap, p) {
  p()
  
  sitemap |> 
    read_html() |> 
    html_elements("url") |> 
    map_chr(\(.x) html_text2(html_elements(.x, "loc")))
}

plan(multisession, workers = parallel::detectCores())

handlers(
  handler_progress(
    format = "[:bar] Remaining: :eta"
  )
)

with_progress({
  p <- progressor(steps = length(sitemaps))
  article_urls <- sitemaps |> future_map(\(.x) get_urls(.x, p = p))
})

article_urls <- do.call(c, article_urls)

# Functions for getting metadata & body from article html:
guard <- function(x) ifelse(rlang::is_empty(x), NA_character_, x)

check_paywall <- function(html)
  grepl('paywall":\\{"attributes":\\{"is_active":true\\}', html) 

get_title <- function(src) {
  src |> 
    html_element("title") |> 
    html_text2() |> 
    guard()
}

get_body <- function(html) {
  html |> 
    html_elements("p") |>  
    html_text2() |>  
    head(-2) |>  
    paste(collapse = " ") |> 
    guard()
}

# author, date, description & news_keywords:
get_meta <- function(html, query) {
  path <- paste0('//*[@name="', query, '"]')
  
  html |>  
    html_elements(xpath = path) |>  
    xml_attr("content") |> 
    guard()
}

# Full scrape:

scrape_article <- function(url) {
  article <- read_html(httr::GET(url))
  
  out <- data.frame(
    url         = url,
    title       = get_title(article),
    date        = get_meta(article, "date"),
    description = get_meta(article, "description"),
    keywords    = get_meta(article, "keywords"),
    author      = get_meta(article, "author"),
    paywall     = check_paywall(article),
    body        = NA_character_,
    error       = NA_character_
  ) 
  
  # retrieve article body if no paywall:
  out$body <- ifelse(out$paywall, out$body, get_body(article))
  
  out
}

# This is what to do if an error occurs:
error_handler <- function(url, error_obj) {
  # log url & error message:
  msg <- as.character(error_obj$message)
  df <- data.frame(url = url, error = msg)

  # fill everything else with NA:
  to_fill <- c(
    "title", "author", "date", "description",
    "keywords", "paywall", "body"
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
  spiegel <- future_map(article_urls, \(.x) scrape_safely(.x, p = p))
})

spiegel <- tibble(do.call(rbind, spiegel))

spiegel |> 
  # drop deleted articles:
  mutate(
    body = ifelse(
      grepl("Sie können den Artikel leider nicht mehr aufrufen.", body),
      NA_character_,
      body
    )
  ) |> 
  write.csv("spiegel.csv")
