setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(rvest)
library(xml2)
library(stringr)
library(pbmcapply)

url <- "https://www.zeit.de/gsitemaps/index.xml"

sitemaps <- url |> 
  httr::GET() |> 
  rvest::read_html() |> 
  html_elements("loc") |> 
  html_text2()

sitemaps <- sitemaps[grepl("2020|2021|2022", sitemaps)]

cores <- detectCores()
article_urls <- pbmclapply(
  sitemaps, \(x) {
    x |> 
      httr::GET() |> 
      read_html() |> 
      html_elements("url") |> 
      html_element("loc") |> 
      html_text2()
  },
  mc.cores = cores, 
  mc.style = "ETA"
)
article_urls <- do.call(c, article_urls)

# Functions for retrieval from html:
get_meta <- function(src, query) {
  out <- src |> 
    html_elements(xpath = paste0('//*[@name="', query, '"]')) |>
    xml_attr("content")
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

get_title <- function(src) {
  out <- src |> 
    html_element("title") |> 
    html_text2()
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

get_author <- function(src) {
  out <- src |> 
    html_elements(xpath = '//a[@rel="author"]') |> 
    html_text2()
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

check_paywall <- function(src) {
  if (grepl('"paywall": "open",', src))
    FALSE
  else
    TRUE
}

get_body <- function(src) {
  out <- src |> 
    html_elements(xpath = '//p[@class="paragraph article__item"]') |> 
    html_text2()
  
  if (length(out) > 1)
    out <- paste(out, collapse = " ")
    
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

# AUTHOR?

zeit_full <- pbmclapply(
  article_urls[1:100],
  function(url) {
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
  },
  mc.cores = cores,
  mc.style = "ETA"
)
zeit_full <- tibble(do.call(rbind, zeit_full))

# TESTING

test <- read_html(httr::GET("https://www.zeit.de/politik/ausland/2023-01/schottland-selbstbestimmungsgesetz-transgender-london"))
test |> 
  html_elements(xpath = '//a[@rel="author"]') |> 
  html_text2()
length(article_urls[!grepl("news", article_urls)])
