#! /usr/bin/env Rscript

# Script to scrape news articles & metadata from welt.de. Excludes sponsored 
# embedded advertisement posts and some other rubrics.

library(dplyr)
library(rvest)
library(purrr)
library(xml2)

url <- "https://www.welt.de/"

articles <- 
  url %>% 
  read_html() %>% 
  html_elements(".o-teaser__link--is-headline")

title <- xml_attr(articles, "title")
link <- xml_attr(articles, "href")
articles <- as.data.frame(cbind(title, link))

exclude <- c("TRUTH FACTS", "Die besten Bilder des Tages")

articles <- 
  articles %>% 
  filter(!grepl("sponsored", link) & !title %in% exclude) %>% 
  mutate(link = paste0("https://www.welt.de", link))

#----
html <- rvest::read_html(articles$link[[1]])

xml_names <- html %>% 
  html_elements("meta") %>% 
  xml_attr("name")
xml_content <- html %>% 
  html_elements("meta") %>% 
  xml_attr("content")
metas <- as.data.frame(cbind(xml_names, xml_content))

# works for description, news_keywords & date:
get_metadata <- function(url, query) {
  html <- rvest::read_html(url)
  
  xml_names <- html %>% 
    html_elements("meta") %>% 
    xml_attr("name")
  xml_content <- html %>% 
    html_elements("meta") %>% 
    xml_attr("content")
  metas <- as.data.frame(cbind(xml_names, xml_content))
  
  return(
    metas %>% 
      filter(xml_names == query) %>% 
      pull(xml_content)
  )
}

# author is specially embedded:
get_author <- function(url) {
  html <- rvest::read_html(url)
  
  return(
    html %>% 
      html_elements(".c-author__link") %>%
      html_text2() %>% 
      paste0(., collapse = ", ")
  )
}

get_body <- function(url) {
  html <- rvest::read_html(url)
  
  return(
    html %>% 
      html_elements("p") %>% 
      html_text2() %>% 
      head(-2) %>% 
      paste0(., collapse = " ")
  )
}

check_paywall <- function(url) {
  pw <- 
    rvest::read_html(url) %>% 
    html_text() %>% 
    stringr::str_extract("isAccessibleForFree(.*?),")
  
  if (grepl("False", pw)) return(1L) else return(0L)
}

# check paywall & retrieve metadata:
articles <- 
  articles %>% 
  mutate(
    author = map_chr(link, get_author),
    description = map_chr(link, ~ get_metadata(.x, "description")),
    keywords = map_chr(link, ~ get_metadata(.x, "news_keywords")),
    date = map_chr(link, ~ get_metadata(.x, "date")),
    paywall = map_int(link, check_paywall),
    timestamp = Sys.time(),
    body = NA_character_
  )

articles <- articles %>% mutate(author = stringr::str_remove(author, "^, "))

# Retrieve article body if no paywall:
for (i in seq_len(nrow(articles))) {
  print(paste0("Processing ", i, "/", nrow(articles)))
  if (articles$paywall[[i]] == 1) {
    articles$body[[i]] <- articles$body[[i]]
  } else {
    articles$body[[i]] <- get_body(articles$link[[i]])
  }
}

# Export
time <- Sys.time() %>% as.character() %>% stringr::str_replace(., " ", "_")
name <- paste0("welt_", time, ".csv")
write.csv(articles, paste0("../data/", name))
