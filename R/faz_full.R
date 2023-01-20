#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(rvest)
library(xml2)
library(purrr)
library(stringr)
library(parallel)
library(pbmcapply)

url <- "https://www.faz.net/sitemap-index.xml"

sitemaps <- url %>%
  read_html() %>% 
  html_elements("loc") %>% 
  html_text2()

exclude <- c(
  "rhein-main-", "race-to-feed-the-world-", "fotografie-", "-apps-", 
  "-reise-", "-karriere-", "-video-", "-bilder-", "-themen-",
  "-mondlandung-"
)

sitemaps <- sitemaps[grepl("-artikel-", sitemaps)]
sitemaps <- sitemaps[!grepl(paste0(exclude, collapse = "|"), sitemaps)]

# Sitemaps are not named or sorted by date, so we need to check article dates 
# manually using the lastmod. This procedure is very "rough" and can probably
# be optimized. 
cores <- detectCores()
article_urls <- pbmclapply(
  sitemaps,
  function(url) {
    src <- read_html(url)
    dates <- src |> 
      html_elements("url") |> 
      html_elements("lastmod") |> 
      html_text2()
    
    if (!TRUE %in% grepl("2022|2021|2020", dates))
      return()
    
    urlset <- src |> html_elements("url")
    link <- urlset |> 
      html_element("loc") |> 
      html_text2()
    date <- urlset |> 
      html_elements("lastmod") |> 
      html_text2()
    articles <- data.frame(cbind(link, date))
    articles <- articles |> filter(grepl("2022|2021|2020", date))
    return(articles$link)
  },
  mc.cores = cores,
  mc.style = "ETA"
)

article_urls <- do.call(c, article_urls)

# Functions for retrieval from article HTML:
get_meta <- function(src, query) {
  out <- src |> 
    html_elements(xpath = paste0('//*[@name="', query, '"]')) |>
    xml_attr("content")
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

get_author <- function(src) {
  out <- src |> 
    html_elements(".aut-Teaser") |>  
    html_attr("data-author-name")
  
  if (rlang::is_empty(out)) {
    out <- src |> 
      html_elements(xpath = '//*[@name="author"]') |>
      xml_attr("content")
  }
  
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

check_paywall <- function(src) {
  if (grepl("paywall:'true'", src)) 
    return(TRUE)
  else
    return(FALSE)
}

get_date <- function(src) {
  out <- src |> 
    html_elements(xpath = '//time[@class="atc-MetaTime"]') |> 
    xml_attr("datetime")
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

get_body <- function(src) {
  out <- src |> 
    html_text2() |>  
    str_extract('"articleBody":"(.*)\",\"dateP') |>  
    mgsub::mgsub(
      c('.*:\"', '\",\"dateP$'),
      rep("", 2)
    )
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

# Full scrape: ~~~~

cores <- detectCores()
faz_full <- pbmclapply(
  article_urls,
  function(url) {
    Sys.sleep(runif(1))
    tryCatch(
      expr = {
        article <- rvest::read_html(url)
        
        out <- data.frame(
          url         = url,
          date        = get_date(article),
          title       = get_title(article),
          author      = get_author(article),
          description = get_meta(article, "description"),
          keywords    = get_meta(article, "keywords"),
          paywall     = check_paywall(article),
          body        = NA_character_,
          error       = NA_character_
        )
        out <- out |> 
          mutate(body = ifelse(paywall, body, get_body(article)))
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
          error       = NA_character_
        )
      }
    )
  },
  mc.cores = cores,
  mc.style = "ETA"
)

faz_full <- tibble(do.call(rbind, faz_full))
write.csv(faz_full, "faz_full.csv")