#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(xml2)
library(dplyr)
library(tidyr)
library(rvest)
library(purrr)
library(furrr)
library(stringr)
library(parallel)
library(progressr)

url <- "https://www.faz.net/sitemap-index.xml"

exclude <- c(
  "rhein-main-", "race-to-feed-the-world-", "fotografie-", "-apps-", 
  "-reise-", "-karriere-", "-video-", "-bilder-", "-themen-",
  "-mondlandung-"
)

sitemaps <- url |> 
  read_html() |> 
  html_elements("loc") |>  
  html_text2() |> 
  (\(.x) .x[grepl("-artikel-", .x)])() |> 
  (\(.x) .x[!grepl(paste0(exclude, collapse = "|"), .x)])()

# Sitemaps are not named or sorted by date, so we need to check article dates 
# manually using the lastmod. This procedure is very "rough" and can probably
# be optimized.

get_article_urls <- function(url, p) {
  p()
  
  # Read sitemap html:
  src <- read_html(url)
  
  # Check dates for all articles in sitemap:
  dates <- src |> 
    html_elements("url") |> 
    html_elements("lastmod") |> 
    html_text2()
  
  # Exit if specified dates not found:
  if (!TRUE %in% grepl("2022|2021|2020", dates)) return()
  
  # Otherwise return the urls for articles that have the specified date:
  urlset <- src |> html_elements("url")
  link <- urlset |> 
    html_element("loc") |> 
    html_text2()
  date <- urlset |> 
    html_elements("lastmod") |> 
    html_text2()
  articles <- data.frame(cbind(link, date))
  articles <- articles |> filter(grepl("2022|2021|2020", date))
  
  articles$link
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

# Function to guard against empty return values when scraping:
guard <- function(x) {
  if (rlang::is_empty(x)) return(NA_character_)
  x
}

# Functions for retrieval from article HTML:
get_meta <- function(src, query) {
  src |> 
    html_elements(xpath = paste0('//*[@name="', query, '"]')) |>
    xml_attr("content") |> 
    guard()
}

get_author <- function(src) {
  out <- src |> 
    html_elements(".aut-Teaser") |>  
    html_attr("data-author-name")
  
  if (rlang::is_empty(out)) {
    out <- src |> 
      html_elements(xpath = '//*[@name="author"]') |>
      xml_attr("content")
  }
  
  out |> guard()
}

get_title <- function(src) {
  out <- src |> 
    html_element("title") |> 
    html_text2() |> 
    guard()
}

check_paywall <- function(src) grepl("paywall:'true'", src) 

get_date <- function(src) {
  out <- src |> 
    html_elements(xpath = '//time[@class="atc-MetaTime"]') |> 
    xml_attr("datetime") |> 
    guard()
}

get_body <- function(src) {
  out <- src |> 
    html_text2() |>  
    str_extract('"articleBody":"(.*)\",\"dateP') |>  
    mgsub::mgsub(
      pattern = c('.*:\"', '\",\"dateP$'),
      replacement = rep("", 2)
    ) |> 
    guard()
}

# Full scrape: ~~~~

scrape_article <- function(url) {
  article <- read_html(httr::GET(url))
  
  out <- data.frame(
    url         = url,
    title       = get_title(article),
    date        = get_date(article),
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
  faz_full <- future_map(article_urls, \(.x) scrape_safely(.x, p = p))
})

faz_full <- tibble(do.call(rbind, faz_full))
write.csv(faz_full, "faz_full.csv")
