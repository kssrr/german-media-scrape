# Scraping retroactively since 01/2022

library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

src <- read_html("https://www.spiegel.de/sitemap.xml")

sitemaps <- src %>% 
  html_elements("loc") %>% 
  html_text2() %>% 
  .[grepl("sitemap-2022-", .)] # get only 2022

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

# Get article urls for all sitemaps:
pb <- progress::progress_bar$new(total = length(sitemaps$sitemap_url))
links <- lapply(
  sitemaps$sitemap_url,
  function (x) {
    pb$tick()
    get_urls(x)
  }
)
links <- tibble(do.call(rbind, links))

# Functions for getting metadata & body:
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

# Full scrape
pb <- progress::progress_bar$new(total = length(links$url))
spiegel_full <- lapply(
  links$url,
  function(url) {
    pb$tick()
    article <- rvest::read_html(url)
    
    out <- data.frame(
      url = url,
      date = get_date(article),
      author = get_author(article),
      description = get_description(article),
      keywords = get_keywords(article),
      paywall = check_paywall(article)
    )
   
    out <- out %>% 
      mutate(
        body = ifelse(
          paywall == 0,
          get_body(article),
          NA_character_
        )
      )
    return(out)
  }
)

spiegel_full <- tibble(do.call(rbind, spiegel_full))
write.csv(spiegel_full, "spiegel_full.csv")
