#! /usr/bin/env Rscript

# Retrieves all article metadata & bodies from the FAZ website.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(rvest)
library(xml2)

url <- "https://www.faz.net/aktuell/"
faz <- url %>% read_html()

articles <- faz %>% html_elements(".tsr-Base_ContentLink")
title <- articles %>% xml_attr("title")
link <- articles %>% xml_attr("href")
paywall <- articles %>% xml_attr("data-is-premium")

articles <- 
  cbind(title, link, paywall) %>% 
  as.data.frame() %>% 
  mutate(
    paywall = case_when(
      paywall == "true" ~ 1L,
      paywall == "false" ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>% 
  # Try to drop embedded ads, podcasts & duplicates:
  filter(grepl("https://www.faz.net/", link) | is.na(link)) %>%
  filter(!grepl("[Ii]m Test:", title) & !grepl("^F\\.A\\.Z\\.", title)) %>%
  distinct()

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
  
  # Case of no assigned keywords:
  if (rlang::is_empty(out)) out <- NA_character_
  
  return(out)
}

get_author <- function(src) {
  out <- 
    src %>% 
    html_elements(".aut-Teaser") %>% 
    html_attr("data-author-name")
  
  # Case of no author:
  if (rlang::is_empty(out)) {
    out <- 
      src %>% 
      html_text2() %>% 
      stringr::str_extract('"author":\\{"@type":"Person","name":"(.*?)",') %>% 
      stringr::str_remove('.*name":"') %>% 
      stringr::str_remove('",$')
  }
  
  # Case of multiple authors:
  if (length(out) > 1) {
    out <- paste(out, collapse = ", ")
  }
  
  return(out)
}

get_publication_date <- function(src) {
  out <- 
    src %>% 
    html_text2() %>% 
    stringr::str_extract('first-publication":"(.*?)"') %>% 
    stringr::str_remove('.*:\"') %>% 
    stringr::str_remove('\"$')
  
  if (rlang::is_empty(out)) out <- NA_character_
  
  return(out)
}

get_body <- function(src) {
  src %>% 
    html_text2() %>% 
    stringr::str_extract('"articleBody":"(.*)\",\"dateP') %>% 
    mgsub::mgsub(
      c('.*:\"', '\",\"dateP$'),
      rep("", 2)
    )
}

#--------------------------------------------------------------------

# Pre-allocate output:
articles <- 
  articles %>% 
  mutate(
    description = NA_character_,
    keywords    = NA_character_,
    author      = NA_character_,
    date        = NA_character_,
    body        = NA_character_
  )

for (i in seq_along(articles$link)) {
  print(paste0("Retrieving ", i, "/", length(articles$link)))
  
  if (is.na(articles$link[[i]])) next
  
  page_src <- rvest::read_html(articles$link[[i]])
  
  articles$description[[i]] <- get_description(page_src)
  articles$keywords[[i]]    <- get_keywords(page_src)
  articles$author[[i]]      <- get_author(page_src)
  articles$date[[i]]        <- get_publication_date(page_src)
  
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

name <- paste0("faz_", time, ".csv")
write.csv(articles, paste0("../data/", name))
