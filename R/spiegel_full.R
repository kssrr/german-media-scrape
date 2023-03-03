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

check_paywall <- function(html) { 
  if (grepl('paywall":\\{"attributes":\\{"is_active":true\\}', html)) 
    TRUE
  
  FALSE
}

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
full_scrape <- function(url, p) {
  p() 
  
  tryCatch(
    expr = {
      article <- read_html(url)
      
      data.frame(
        url = url,
        date = get_meta(article, "date"),
        title = get_title(article),
        description = get_meta(article, "description"),
        keywords = get_meta(article, "news_keywords"),
        author = get_meta(article, "author"),
        paywall = check_paywall(article),
        error = NA_character_
      ) |> 
        mutate(body = ifelse(paywall, NA_character_, get_body(article)))
    },
    error = function(e) {
      e <- as.character(e[1])
      if (length(e) > 1)
        e <- paste(e, collapse = " ")
      
      data.frame(
        url = url,
        date = NA_character_,
        title = NA_character_,
        description = NA_character_,
        keywords = NA_character_,
        author = NA_character_,
        paywall = NA_character_,
        error = e,
        body = NA_character_
      )
    }
  )
}

with_progress({
  p <- progressor(steps = length(sitemaps))
  spiegel <- article_urls[1:50] |> future_map(\(.x) full_scrape(.x, p = p))
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
