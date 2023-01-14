setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(rvest)
library(xml2)
library(purrr)
library(stringr)
library(parallel)
library(pbmcapply)

url <- "https://taz.de/sitemap-index.xml"
sitemaps <- read_html(url) |> 
  html_elements("loc") |> 
  html_text2()

sitemaps <- sitemaps[grepl("2022|2021|2020", sitemaps)]

pb <- progress::progress_bar$new(
  format = "  Processing [:bar] :percent Elapsed: :elapsed Left: :eta",
  total = length(sitemaps),
)
article_urls <- lapply(
  sitemaps,
  function(url) {
    pb$tick()
    tryCatch(
      expr = {
        links <- url |>
          read_html() |> 
          html_elements("url") |> 
          html_element("loc") |> 
          html_text2()
        
        if (rlang::is_empty(links)) 
          return()
        else
          return(links)
      },
      error = function(e) return()
    )
  }
)

article_urls <- as.character(do.call(c, article_urls))

get_title <- function(src) {
  out <- src |> 
    html_element("title") |> 
    html_text2()
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

get_meta <- function(src, query) {
  out <- src |> 
    html_elements(xpath = paste0('//*[@name="', query, '"]')) |>
    xml_attr("content")
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

get_date <- function(src) {
  out <- src |> 
    html_elements(xpath = '//li[@class="date"]') |> 
    xml_attr("content")
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
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
  out <- src |> 
    html_elements(xpath = paste(patterns, collapse = " | ")) |>
    html_text2() |> 
    paste(collapse = " ")
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

# Full scrape
cores <- detectCores()
taz_full <- pbmclapply(
  article_urls,
  function(url) {
    tryCatch(
      expr = {
        article <- rvest::read_html(url)
        
        data.frame(
          url         = url,
          date        = get_meta(article, "date"),
          title       = get_title(article),
          author      = get_meta(article, "author"),
          description = get_meta(article, "description"),
          keywords    = get_meta(article, "keywords"),
          body        = get_body(article),
          error       = NA_character_
        )
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
          body        = NA_character_,
          error       = NA_character_
        )
      }
    )
  },
  mc.cores = cores,
  mc.style = "ETA"
)

taz_full <- tibble(do.call(rbind, taz_full))
write.csv(taz_full, "taz_full.csv")
