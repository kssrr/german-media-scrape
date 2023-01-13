# Scraping all Spiegel Online articles from 01/2020 to 12/2022. 

# If you are on R version 4.2+:
#devtools::install_github("kvnkuang/pbmcapply", ref = "dev")

library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(parallel)
library(pbmcapply)

src <- read_html("https://www.spiegel.de/sitemap.xml")

# Get sitemaps:
sitemaps <- src %>% 
  html_elements("loc") %>% 
  html_text2() %>% 
  .[grepl("sitemap-2022-|sitemap-2021-|sitemap-2020-", .)]

sitemaps <- sitemaps %>% 
  tibble("sitemap_url" = `.`) %>% 
  mutate(
    sitemaps = str_extract(sitemap_url, "sitemap-(.*?)\\.xml"),
    sitemaps = mgsub::mgsub(sitemaps, c("sitemap-", "\\.xml"), rep("", 2))
  )

# Get articles from each sitemap:
safe_extract <- function(node, keyword) {
  out <- node %>% 
    html_elements(keyword) %>% 
    html_text2()
  
  if (rlang::is_empty(out)) out <- NA_character_
  return(out)
}

get_urls <- function(sitemap) {
  sitemap %>%
    read_html() %>% 
    html_elements("url") %>% 
    map_chr(~ html_text2(html_elements(.x, "loc"))) %>% 
    tibble("url" = `.`) %>% 
    cbind(., sitemap)
}

# Get article links from sitemaps:
cores <- detectCores()
links <- pbmclapply(
  sitemaps$sitemap_url,
  get_urls,
  mc.cores = cores,
  mc.style = "ETA"
)
links <- tibble(do.call(rbind, links))

# Functions for getting metadata & body from article html:
check_paywall <- function(html) { 
  if(grepl('paywall":\\{"attributes":\\{"is_active":true\\}', html)) {
    return(1L)
  } else {
    return(0L)
  }
}

get_body <- function(html) {
  body <- html %>% 
    html_elements("p") %>% 
    html_text2() %>% 
    head(-2) %>% 
    paste(., collapse = " ")
  if (rlang::is_empty(body)) body <- NA_character_
  return(as.character(body))
}

get_author <- function(html) {
  author <- html %>% 
    html_elements(xpath = '//*[@name="author"]') %>% 
    xml_attr("content")
  if (rlang::is_empty(author)) author <- NA_character_
  return(author)
}

get_date <- function(html) {
  date <- html %>% 
    html_elements(xpath = '//*[@name="date"]') %>% 
    xml_attr("content")
  
  if (rlang::is_empty(date)) date <- NA_character_
  return(date)
}

get_description <- function(html) {
  description <- html %>% 
    html_elements(xpath = '//*[@name="description"]') %>% 
    xml_attr("content")
  
  if (rlang::is_empty(description)) description <- NA_character_
  return(description)
}

get_keywords <- function(html) {
  keys <- html %>% 
    html_elements(xpath = '//*[@name="news_keywords"]') %>% 
    xml_attr("content")
  
  if (rlang::is_empty(keys)) keys <- NA_character_
  return(keys)
}

# Full scrape:
# Parallelization across CPU cores. Catches errors to not interrupt the lengthy 
# scrape; error messages are recorded in `spiegel_full$error`.

# NOTE: This parallelization across cores does not work on Windows, because Windows
# (unlike macOS and Linux) does not support forking. See here for a "hack" to cir-
# cumvent this:
# https://www.r-bloggers.com/2014/07/implementing-mclapply-on-windows-a-primer-on-embarrassingly-parallel-computation-on-multicore-systems-with-r/

spiegel_full <- pbmclapply(
  links$url,
  function(url) {
    tryCatch(
      # Expression to run normally:
      expr = {
        article <- rvest::read_html(url)
        
        out <- data.frame(
          url         = url,
          date        = get_date(article),
          author      = get_author(article),
          description = get_description(article),
          keywords    = get_keywords(article),
          paywall     = check_paywall(article),
          error       = NA_character_
        )
        
        out <- out %>% # get article body if there is no paywall
          mutate(
            body = ifelse(
              paywall == 0,
              get_body(article),
              NA_character_
            )
          )
        return(out)
      },
      # If an error is caught:
      error = function(e) {
        e <- as.character(e[1])
        if (length(e) > 1)
          e <- paste(e, collapse = " ")
        
        out <- data.frame(
          url         = url,
          date        = NA_character_,
          author      = NA_character_,
          description = NA_character_,
          keywords    = NA_character_,
          paywall     = NA_character_,
          error       = e,
          body        = NA_character_
        )
        return(out)
      }
    )
  },
  mc.cores = cores,
  mc.style = "ETA" # show estimated time to completion
)

spiegel_full <- tibble(do.call(rbind, spiegel_full))
write.csv(spiegel_full, "spiegel_full.csv")
