setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(rvest)
library(xml2)
library(purrr)
library(stringr)
library(parallel)
library(lubridate)
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
article_urls <- lapply(sitemaps, function(url) {
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
})

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

# Sometimes, the date is not included in the meta, but is more freely
# written in a div (e.g. "Monday, 28.11.2020, 18:59"). This function 
# tries to still extract it. 
get_date2 <- function(src) {
  out <- src |> 
    html_elements("div") |> 
    html_elements(xpath = '//*[@class="dateLocWrapper"]') |> 
    html_text2()
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
    Sys.sleep(runif(1))
    tryCatch(
      expr = {
        article <- rvest::read_html(httr::GET(url))
        
        data.frame(
          url         = url,
          date        = get_date(article),
          date_alt    = NA_character_,
          title       = get_title(article),
          author      = get_meta(article, "author"),
          description = get_meta(article, "description"),
          keywords    = get_meta(article, "keywords"),
          body        = get_body(article),
          error       = NA_character_
        ) |> 
          mutate(date_alt = ifelse(is.na(date), get_date2(article), date_alt))
      },
      error = function(e) {
        e <- as.character(e[1])
        if (length(e) > 1)
          e <- paste(e, collapse = " ")
        
        data.frame(
          url         = url,
          date        = NA_character_,
          date_alt    = NA_character_,
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

# Unifying date column from the freely extracted dates. Ideally,
# this would be dealt with in get_date2, a rewrite would be appreciated.
taz_full <- taz_full |> 
  mutate(
    date_alt = map_chr(date_alt, ~ {
      .x |>   
        str_remove_all("[a-zA-Z]|,") |> 
        trimws()
    }),
    time = trimws(str_extract(date_alt, "\\s(.*?)$")),
    date_alt = map_chr(date_alt, ~ {
      .x |> 
        str_remove("\\s(.*?)$") |> 
        str_replace_all("\\.", "-") |> 
        lubridate::dmy() |> 
        as.character()
    }),
    date_alt = ifelse(
      !is.na(date_alt),
      paste0(date_alt, "T", time, ":00+01:00"),
      date_alt
    ),
    date = ifelse(is.na(date), date_alt, date)
  ) |> 
  select(-c(date_alt, time)) |> 
  distinct()

# Also cleaning titles:
taz_full <- taz_full |> mutate(title = str_remove(title, " - taz.de$"))

write.csv(taz_full, "taz_full.csv")
