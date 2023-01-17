#!/usr/bin/env Rscript 
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(purrr)
library(rvest)
library(xml2)
library(pbmcapply)

# https://www.sueddeutsche.de/sitemapapp/sitemap/article/2010/32

base_url <- "https://www.sueddeutsche.de/sitemapapp/sitemap/article/"
all <- expand.grid(as.character(2020:2022), as.character(1:20)) %>% arrange(Var1)
sitemaps <- paste0(base_url, all$Var1, "/", all$Var2)

get_urls <- function(sitemap) {
  out <- sitemap |> 
    html_elements("url") |> 
    html_element("loc") |> 
    html_text2()
  if(rlang::is_empty(out)) return(NA_character_)
  return(out)
}

pb <- progress::progress_bar$new(
  format = "Trying [:bar] :percent in :eta",
  total = length(sitemaps)
)

article_links <- map(
  sitemaps,
  function(url) {
    pb$tick()
    tryCatch(
      expr = {
        url |> 
          rvest::read_html() |> 
          get_urls()
      },
      error = function(e) return()
    )
  }
)

article_links <- do.call(c, article_links)

get_title <- function(src) {
  out <- src |> 
    html_element("span") |>
    html_element(xpath = '//*[@data-manual="title"]') |> 
    html_text2()
  if(rlang::is_empty(out)) return(NA_character_)
  return(out)
}

get_meta <- function(src, query) {
  out <- src |> 
    html_elements("meta") |> 
    html_element(xpath = paste0('//*[@name="', query, '"]')) |> 
    html_attr("content")
  if(rlang::is_empty(out)) return(NA_character_)
  return(out)
}

get_date <- function(src) {
  out <- src |> 
    html_elements("time") |> 
    xml_attr("datetime")
  if(rlang::is_empty(out)) return(NA_character_)
  return(out)
}

check_paywall <- function(src) {
  if (grepl('"page.paywall_shown":true', html_text(test2)))
    return(1L)
  else
    return(0L)
}

get_body <- function(src) {
  out <- src |> 
    html_elements("p") |> 
    html_element(xpath = '//*[@data-manual="paragraph"]') |> 
    html_text2() |> 
    paste(collapse = " ")
  if(rlang::is_empty(out)) return(NA_character_)
  return(out)
}

cores <- detectCores()
full <- pbmclapply(
  article_links,
  function(url) {
    tryCatch(
      expr = {
        article <- rvest::read_html(url)
        
        out <- data.frame(
          url         = url,
          date        = get_date(article),
          title       = get_title(article),
          author      = get_meta(article, "author"),
          description = get_meta(article, "description"),
          keywords    = get_meta(article, "keywords"),
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
      error = function(e) {
        e <- as.character(e[1])
        if (length(e) > 1)
          e <- paste(e, collapse = " ")
        
        out <- data.frame(
          url         = url,
          date        = NA_character_,
          title       = NA_character_,
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
  mc.style = "ETA"
)

full <- tibble(do.call(rbind, full))
write.csv(full, "sueddeutsche_full.csv")
