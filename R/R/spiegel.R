#! /usr/bin/env Rscript

# Retrieves all articles with metadata and body (where there is no paywall) from
# the Spiegel news website (spiegel.de). 

library(rvest)
library(xml2)
library(dplyr)
library(purrr)

url <- "https://www.spiegel.de/"

spiegel <- 
  url %>% 
  rvest::read_html() 

headlines <- 
  spiegel %>% 
  html_elements("article") %>% 
  xml_attr("aria-label")

attributes <- 
  spiegel %>% 
  html_elements("a")

titles <- attributes %>% html_attr("title")
links <- attributes %>% html_attr("href")

attributes <- as.data.frame(cbind(titles, links))

articles <- 
  attributes %>% 
  filter(titles %in% headlines) %>% 
  distinct() %>% 
  tidyr::drop_na()

get_metainfo <- function(url, query) {
  force(url)
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

check_paywall <- function(url) {
  html <- rvest::read_html(url) 
  if(grepl('paywall":\\{"attributes":\\{"is_active":true\\}', html)) {
    return(1L)
  } else {
    return(0L)
  }
}

get_body <- function(url) {
  html <- rvest::read_html(url)
  body <- 
    html %>% 
    html_elements("p") %>% 
    html_text2() %>% 
    head(-2) %>% 
    paste(., collapse = " ")
  return(as.character(body))
}

# Retrieve description, keywords etc. (may take a while to run):
articles <- 
  articles %>% 
  mutate(
    description = map_chr(links, ~ get_metainfo(.x, "description")),
    keywords = map_chr(links, ~ get_metainfo(.x, "news_keywords")),
    date = map_chr(links, ~ get_metainfo(.x, "date")),
    paywall = map_int(links, check_paywall),
    timestamp = Sys.time(),
    body = NA_character_
  ) %>% 
  select(timestamp, everything())

# Retrieve body if no paywall:
for (i in seq_len(nrow(articles))) {
  print(paste0("Processing ", i, "/", nrow(articles)))
  if (articles$paywall[[i]] == 1) {
    articles$body[[i]] <- articles$body[[i]]
  } else {
    articles$body[[i]] <- get_body(articles$links[[i]])
  }
}

# Export
time <- Sys.time() %>% as.character() %>% stringr::str_replace(., " ", "_")
name <- paste0("spiegel_", time, ".csv")
write.csv(articles, paste0("../data/", name))
