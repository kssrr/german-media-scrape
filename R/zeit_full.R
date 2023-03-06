setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(rvest)
library(xml2)
library(stringr)
library(purrr)
library(furrr)
library(progressr)

url <- "https://www.zeit.de/gsitemaps/index.xml"
patterns <- "2020|2021|2022"

sitemaps <- url |> 
  httr::GET() |> 
  rvest::read_html() |> 
  html_elements("loc") |> 
  html_text2() |> 
  (\(.x) .x[grepl(patterns, .x)])()

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

check_paywall <- function(src) 
  ifelse(grepl('"paywall": "paid"|"paywall": "register"', src), TRUE, FALSE)

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
#length(article_urls[!grepl("news", article_urls)])

full_scrape <- function(url, p) {
  p()
  tryCatch(
    expr = {
      article <- read_html(httr::GET(url))
      
      data.frame(
        url         = url,
        title       = get_title(article),
        description = get_meta(article, "description"),
        keywords    = get_meta(article, "keywords"),
        author      = get_author(article),
        paywall     = check_paywall(article),
        body        = NA_character_,
        error       = NA_character_
      ) |> 
        mutate(body = ifelse(paywall, NA_character_, get_body(article)))
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
  zeit_full <- article_urls |> future_map(\(.x) full_scrape(.x, p = p))
})

zeit_full <- tibble(do.call(rbind, zeit_full))
write.csv(zeit_full, "zeit_full.csv")
