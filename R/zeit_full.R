#!/usr/bin/env Rscript

library(xml2)
library(dplyr)
library(tidyr)
library(rvest)
library(purrr)
library(furrr)
library(stringr)
library(progressr)

url <- "https://www.zeit.de/gsitemaps/index.xml"
patterns <- "2020|2021|2022"

sitemaps <- url |> 
  httr::GET() |> 
  rvest::read_html() |> 
  html_elements("loc") |> 
  html_text2() |> 
  grep(patterns, x = _, value = TRUE)

# Get article urls:
get_article_urls <- function(x, p) {
  p()
  
  x |> 
    httr::GET() |> 
    read_html() |> 
    html_elements("url") |> 
    html_element("loc") |> 
    html_text2()
}

plan(multisession, workers = parallel::detectCores())

handlers(
  handler_progress(
    format = "[:bar] Remaining: :eta"
  )
)

with_progress({
  p <- progressor(steps = length(sitemaps))
  article_urls <- sitemaps |> future_map(\(.x) get_article_urls(.x, p = p))
})

article_urls <- do.call(c, article_urls)

# Functions for retrieval from html:
guard <- function(x) ifelse(rlang::is_empty(x), NA_character_, x)

get_meta <- function(src, query) {
  src |> 
    html_elements(xpath = paste0('//*[@name="', query, '"]')) |>
    xml_attr("content") |> 
    guard()
}

get_title <- function(src) {
  src |> 
    html_element("title") |> 
    html_text2() |> 
    guard()
}

get_author <- function(src) {
  src |> 
    html_elements(xpath = '//a[@rel="author"]') |> 
    html_text2() |> 
    guard()
}

check_paywall <- function(src) {
    patterns <- c('"paywall": "paid"', '"paywall": "register"')
    grepl(paste0(patterns, collapse = "|"), src)
}

get_body <- function(src) {
  out <- src |> 
    html_elements(xpath = '//p[@class="paragraph article__item"]') |> 
    html_text2() |> 
    guard()
  
  if (length(out) > 1) out <- paste(out, collapse = " ")
    
  out
}

get_author <- function(src) {
  src |> 
    html_elements(xpath = '//a[@rel="author"]') |> 
    html_text2() |> 
    guard()
}

# Full scrape: 
# "news" articles are mostly only agency copy-pastes...? 
# We could probably exclude them...
#length(article_urls[!grepl("news", article_urls)])

# This will be executed for every article url:
scrape_article <- function(url) {
  article <- read_html(httr::GET(url))
  
  out <- data.frame(
    url         = url,
    title       = get_title(article),
    date        = get_meta(article, "date"),
    description = get_meta(article, "description"),
    keywords    = get_meta(article, "keywords"),
    author      = get_author(article),
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
  zeit_full <- future_map(article_urls, \(.x) scrape_safely(.x, p = p))
})

zeit_full <- tibble(do.call(rbind, zeit_full))
write.csv(zeit_full, "zeit_full.csv")
