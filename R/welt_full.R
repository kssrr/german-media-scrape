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
  (\(.x) .x[grepl(patterns, .x)])()

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
  save_download <- function(req) {
    writeBin(req$content, file_local[file_remote == req$url])
  }
  
  # set up asynchronous calls:
  invisible(
    lapply(
      file_remote, function(f) 
        curl::curl_fetch_multi(f, done = save_download, pool = pool)
    )
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

# Read:
files <- paste0("welt_sitemaps/", list.files("welt_sitemaps/"))
xml_list <- map(files, read_html)

# Get article links:
xml_list <- map(xml_list, \(.x) html_text2(html_elements(.x, "loc")))
article_urls <- as.character(do.call(c, xml_list))

# Function for guarding against empty returns:
guard <- function(x) ifelse(rlang::is_empty(x), NA_character_, x)

# Functions for extracting data & metadata from the html:
get_keywords <- function(html) {
  html |>
    html_elements(xpath = '//*[@name="keywords"]') |> 
    xml_attr("content") |> 
    guard()
}

get_title <- function(src) {
  src |> 
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
    html_text2()
  out <- paste0(out, collapse = ", ")
  if (rlang::is_empty(out)) 
    out <- NA_character_
  if (grepl("^,", out)) 
    out <- str_remove(out, "^, ")
  return(out)
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
  
  if (grepl("False", pw))
    TRUE
  
  FALSE
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
full_scrape <- function(url, p) {
  p()
  
  tryCatch(
    expr = {
      article <- read_html(url)
      
      out <- data.frame(
        url         = url,
        date        = get_date(article),
        title       = get_title(article),
        author      = get_author(article),
        description = get_description(article),
        keywords    = get_keywords(article),
        paywall     = check_paywall(article),
        error       = NA_character_
      ) |> 
        mutate(body = ifelse(paywall, NA_character_, get_body(article)))
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
        error       = e
      )
    }
  )
}

plan(multisession, workers = parallel::detectCores())

handlers(
  handler_progress(
    format = "[:bar] Remaining: :eta"
  )
)

with_progress({
  p <- progressor(steps = length(article_urls))
  welt <- article_urls |> future_map(\(.x) full_scrape(.x, p = p))
})

welt |> 
  bind_rows() |> View()
  write.csv("welt.csv")
