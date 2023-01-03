#! /usr/bin/env Rscript

# Retrieve article metadata & bodies from the ZEIT website.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(rvest)
library(xml2)

url <- "https://www.zeit.de/index"

exclude <- c(
  "Direkt zum Inhalt springen", "exklusive Zeit Artikel",
  "Nachrichten auf ZEIT ONLINE", "Kommentare anzeigen",
  "Zurück nach oben", "ZEIT ONLINE Stimmen: Hier kommen Sie zu Wort"
)

page <- 
  url %>% 
  httr::GET(url = .) %>% 
  rvest::read_html()

title <- 
  page %>% 
  html_elements("article") %>% 
  html_elements("a") %>% 
  xml_find_all('//*[@title and @href]') %>% 
  xml_attr("title")

link <- 
  page %>% 
  html_elements("article") %>% 
  html_elements("a") %>% 
  xml_find_all('//*[@title and @href]') %>% 
  xml_attr("href")

articles <- 
  as.data.frame(cbind(title, link)) %>% 
  filter(!title %in% exclude & grepl("https://www.zeit.de/", link)) %>% 
  distinct()

# Retrieving metadata & body:
get_description <- function(src) {
  out <- 
    src %>% 
    html_elements(xpath = '//*[@name="description"]') %>% 
    xml_attr("content")
  
  if (rlang::is_empty(out)) out <- NA_character_
  
  return(out)
}

get_keywords <- function(src) {
  out <- 
    src %>% 
    html_elements(xpath = '//*[@name="keywords"]') %>% 
    xml_attr("content")
  
  if (rlang::is_empty(out)) out <- NA_character_
  
  return(out)
}

get_publication_date <- function(src) {
  out <- 
    src %>% 
    html_elements(xpath = '//*[@name="date"]') %>% 
    xml_attr("content")
  
  if (rlang::is_empty(out)) out <- NA_character_
  
  return(out)
}

get_author <- function(src) {
  out <- 
    src %>% 
    html_elements(xpath = '//*[@rel="author"]') %>% 
    html_text2()
  
  # If no author:
  if (rlang::is_empty(out)) out <- NA_character_
  
  # If multiple authors:
  if (length(out) > 1) out <- paste(out, collapse = ", ")
  
  return(out)
}

get_paywall <- function(src) {
  if (grepl('"paywall": "open"', src)) 
    return(0L)
  else 
    return(1L)
}

get_body <- function(src) {
  out <- 
    src %>% 
    html_elements(xpath = '//*[@class="paragraph article__item"]') %>% 
    html_text2() %>% 
    paste(collapse = "")
  
  if (rlang::is_empty(out)) out <- NA_character_
  
  return(out)
}

test <- httr::GET(articles$link[[1]]) %>% rvest::read_html()

# Pre-allocate output:
articles <- 
  articles %>% 
  mutate(
    description = NA_character_,
    keywords    = NA_character_, 
    date        = NA_character_,
    author      = NA_character_,
    paywall     = NA_character_,
    body        = NA_character_
  )

for (i in seq_along(articles$link)) {
  print(paste0("Retrieving ", i, "/", length(articles$link)))
  
  page_src <- httr::GET(articles$link[[i]]) %>% rvest::read_html()
  
  articles$description[[i]] <- get_description(page_src)
  articles$keywords[[i]]    <- get_keywords(page_src)
  articles$date[[i]]        <- get_publication_date(page_src)
  articles$author[[i]]      <- get_author(page_src)
  articles$paywall[[i]]     <- get_paywall(page_src)
  
  if (articles$paywall[[i]] == 0) {
    articles$body[[i]] <- get_body(page_src)
  }
}

# Add timestamp:
articles <- articles %>% mutate(timestamp = Sys.time())

# Export:
time <- 
  Sys.time() %>% 
  as.character() %>% 
  stringr::str_replace(" ", "_")

name <- paste0("zeit_", time, ".csv")
write.csv(articles, paste0("../data/", name))

