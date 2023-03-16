# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(stringr)
library(progressr)

# Retrieve sitemaps:
url <- "https://www.welt.de/sitemaps/sitemap/sitemap.xml"
patterns <- "2020|2021|2022"

# the url contains a list of download links for compressed folders holding the
# actual sitemaps. Thus, we need to first download & unzip the archives:

# Retrieving download links:
sitemap_urls <- 
  url |> 
  read_html() |> 
  html_elements("loc") |> 
  html_text2() |> 
  grep(patterns, x = _, value = TRUE)

sitemap_names <- 
  sitemap_urls |> 
  str_remove("https://www.welt.de/sitemaps/") |> 
  str_replace_all("/", "_")

# Create directory to store them:
if (dir.exists("welt_sitemaps")) {
  warning("Directory exists, overwriting...")
  unlink("welt_sitemaps/", recursive = T)
}
dir.create("welt_sitemaps/")

# Function for parallelizing the downloads: 

multi_download <- function(file_remote,       # url
                           file_local,        # destination
                           total_con = 1000L, # max concurrent connections
                           host_con  = 1000L, # max conc. cons per host
                           print = TRUE) {    # print status of request 
  # create pool:
  pool <- curl::new_pool(
    total_con = total_con,
    host_con = host_con
  )
  
  # function performed on successful request
  save_download <- function(req)
    writeBin(req$content, file_local[file_remote == req$url])
  
  # set up asynchronous calls:
  walk(
    file_remote,
    \(.x) curl::curl_fetch_multi(.x, done = save_download, pool = pool)
  )
  
  # all created requests are performed here:
  out <- curl::multi_run(pool = pool)
  
  if (print) print(out)
  
}

# Downloading:
multi_download(
  file_remote = sitemap_urls,
  file_local = paste0("welt_sitemaps/", sitemap_names)
)

# Unzip:
archives <- list.files("welt_sitemaps/")
paths <- paste0("welt_sitemaps/", archives)
walk(paths, \(.x) R.utils::gunzip(.x, overwrite = T))

# mapper functions:
map_progress <- function(x, f, parallel = FALSE) {

  if (parallel)
    mapper_fun <- furrr::future_map
  else
    mapper_fun <- purrr::map

  f_progress <- function(..., p) {
    p()
    f(...)
  }

  with_progress({
    p <- progressor(steps = length(x))
    out <- mapper_fun(x, \(.x) f_progress(.x, p = p))
  })

  out
  
}

handlers(
  handler_progress(
    format = "[:bar] Remaining: :eta"
  )
)

# Read:
# This can not be parallelized, since xml2-objects can not be exported
# to other R processes because they hold external pointers unique to the
# R process they were created on.
# https://stackoverflow.com/questions/55810140/parallel-processing-xml-nodes-with-r#55843269
files <- paste0("welt_sitemaps/", list.files("welt_sitemaps/"))
xml_list <- map_progress(files, xml2::read_html)

# Get article links:
get_article_urls <- function(src) {
  src |>
    html_elements("loc") |>
    html_text2()
}

article_urls <-
  xml_list |>
  map_progress(get_article_urls) |>
  reduce(c) |>
  as.character()

# Drop topic collections:
article_urls <- article_urls[!grepl("/themen/", article_urls)]

# Function for guarding against empty returns:
guard <- function(x) ifelse(rlang::is_empty(x), NA_character_, x)

# Functions for extracting data & metadata from the html:
get_keywords <- function(html) {
  html |>
    html_elements(xpath = '//*[@name="keywords"]') |> 
    xml_attr("content") |> 
    guard()
}

get_title <- function(html) {
  html |> 
    html_element("title") |> 
    html_text2() |> 
    guard()
}

get_description <- function(html) {
  html |>
    html_elements(xpath = '//*[@name="description"]') |> 
    xml_attr("content") |> 
    guard()
}

get_author <- function(html) {
  out <- html |>
    html_elements(".c-author__link") |>
    html_text2() |> 
    paste0(collapse = ", ") |> 
    guard()
  
  if (grepl("^,", out)) 
    out <- str_remove(out, "^, ")
  
  out
}

get_date <- function(html) {
  html |>
    html_elements(xpath = '//*[@name="date"]') |> 
    xml_attr("content") |> 
    guard()
}

check_paywall <- function(html) {
  pw <- html |>
    html_text() |>
    stringr::str_extract("isAccessibleForFree(.*?),")

  grepl("False", pw)
}

get_body <- function(html) {
  html |>
    html_elements("p") |>
    html_text2() |>
    head(-2) |> 
    paste0(collapse = " ") |> 
    guard()
}

# Full scrape:
scrape_article <- function(url) {
  article <- read_html(url)
  
  article <- read_html(url)
  
  out <- data.frame(
    url         = url,
    date        = get_date(article),
    title       = get_title(article),
    author      = get_author(article),
    description = get_description(article),
    keywords    = get_keywords(article),
    paywall     = check_paywall(article),
    body        = NA_character_, 
    error       = NA_character_
  ) 

  out$body <- ifelse(out$paywall, out$body, get_body(article))

  out
}

error_handler <- function(url, error_obj) {
  msg <- as.character(error_obj$message)
  out <- data.frame(url = url, error = msg)

  to_fill <- c(
    "title", "author", "date", "description",
    "keywords", "paywall", "body"
  )
  out[, to_fill] <- NA_character_

  out
}

scrape_safely <- function(url) {
  tryCatch(
    expr = scrape_article(url),
    error = \(e) error_handler(url = url, error_obj = e)
  )
}

# Starting parallel session:
plan(multisession, workers = parallel::detectCores())

welt <- map_progress(article_urls, scrape_safely, parallel = TRUE)

welt |> 
  bind_rows() |>
  tibble() |> 
  write.csv("welt.csv")
