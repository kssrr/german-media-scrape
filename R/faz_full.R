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

check_paywall <- function(src) {
  if (grepl("paywall:'true'", src)) 
    TRUE
  
  FALSE
}

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

full_scrape <- function(url, p) {
  p()
  tryCatch(
    expr = {
      article <- rvest::read_html(url)
      
      out <- data.frame(
        url         = url,
        date        = get_date(article),
        title       = get_title(article),
        author      = get_author(article),
        description = get_meta(article, "description"),
        keywords    = get_meta(article, "keywords"),
        paywall     = check_paywall(article),
        body        = NA_character_,
        error       = NA_character_
      )
      out |> 
        mutate(body = ifelse(paywall, body, get_body(article)))
    },
    error = function(e) {
      e <- as.character(e[1])
      if (length(e) > 1)
        e <- paste(e, collapse = " ")
      
      data.frame(
        url         = url,
        date        = NA_character_,
        title       = NA_character_,
        author      = NA_character_,
        description = NA_character_,
        keywords    = NA_character_,
        paywall     = NA_character_,
        body        = NA_character_,
        error       = e
      )
    }
  )
}

with_progress({
  p <- progressor(steps = length(article_urls))
  faz_full <- article_urls |> future_map(\(.x) full_scrape(.x, p = p))
})

faz_full <- tibble(do.call(rbind, faz_full))
write.csv(faz_full, "faz_full.csv")
