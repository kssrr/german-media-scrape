# Scraping WELT retroactively since 01/2022:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# If you are on R version 4.2+:
#devtools::install_github("kvnkuang/pbmcapply", ref = "dev")

library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(parallel)
library(pbmcapply)

# Retrieve sitemaps:
url <- "https://www.welt.de/sitemaps/sitemap/sitemap.xml"

# The url is only a list of links to compressed .xml.gz downloads,
# so we need to retrieve the actual sitemaps first:
sitemap_urls <- url %>% 
  read_html() %>% 
  html_elements("loc") %>% 
  html_text2() %>% 
  .[grepl("2020|2021|2022", .)]

sitemap_names <- sitemap_urls %>% 
  str_remove("https://www.welt.de/sitemaps/") %>% 
  str_replace_all("/", "_")

if (dir.exists("welt_sitemaps")) {
  warning("Directory exists, overwriting...")
  unlink("welt_sitemaps/", recursive = T)
}
dir.create("welt_sitemaps/")

# Function for parallelizing downloads: 
multi_download <- function(file_remote,       # url
                           file_local,        # destination
                           total_con = 1000L, # max concurrent connections
                           host_con  = 1000L, # max conc. cons per host
                           print = TRUE) {    # print status of request 
  # create pool
  pool <- curl::new_pool(total_con = total_con,
                         host_con = host_con)
  
  # function performed on successful request
  save_download <- function(req) {
    writeBin(req$content, file_local[file_remote == req$url])
  }
  # setup async calls
  invisible(
    lapply(
      file_remote, function(f) 
        curl::curl_fetch_multi(f, done = save_download, pool = pool)
    )
  )
  # all created requests are performed here
  out <- curl::multi_run(pool = pool)
  
  if (print) print(out)
  
}

multi_download(
  file_remote = sitemap_urls,
  file_local = paste0("welt_sitemaps/", sitemap_names)
)

# Unzip:
archives <- list.files("welt_sitemaps/")
paths <- paste0("welt_sitemaps/", archives)
walk(paths, ~ R.utils::gunzip(.x, overwrite = T))

# Read:
files <- list.files("welt_sitemaps/") %>% paste0("welt_sitemaps/", .)
xml_list <- map(files, read_html)

# Get article links:
xml_list <- map(xml_list, ~ html_text2(html_elements(.x, "loc")))
article_urls <- as.character(do.call(c, xml_list))

# Functions for extracting data & metadata from the html:
get_keywords <- function(html) {
  out <- html |>
    html_elements(xpath = '//*[@name="keywords"]') |> 
    xml_attr("content")
  if (rlang::is_empty(out)) out <- NA_character_
  return(out)
}

get_title <- function(src) {
  out <- src |> 
    html_element("title") |> 
    html_text2()
  if (rlang::is_empty(out)) return(NA_character_)
  return(out)
}

get_description <- function(html) {
  out <- html |>
    html_elements(xpath = '//*[@name="description"]') |> 
    xml_attr("content")
  if (rlang::is_empty(out)) out <- NA_character_
  return(out)
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
  out <- html |>
    html_elements(xpath = '//*[@name="date"]') |> 
    xml_attr("content")
  if (rlang::is_empty(out)) out <- NA_character_
  return(out)
}

check_paywall <- function(html) {
  pw <- html |>
    html_text() |>
    stringr::str_extract("isAccessibleForFree(.*?),")
  
  if (grepl("False", pw)) return(1L) else return(0L)
}

get_body <- function(html) {
  out <- html |>
    html_elements("p") |>
    html_text2() |>
    head(-2)
  out <- paste0(out, collapse = " ")
  if (rlang::is_empty(out)) out <- NA_character_
  return(out)
}

# Full scrape:
# pbmclapply() and mclapply() don't work on Windows. See here for a "hack":
# https://www.r-bloggers.com/2014/07/implementing-mclapply-on-windows-a-primer-on-embarrassingly-parallel-computation-on-multicore-systems-with-r/

cores <- detectCores()
# pbmclapply = progress-bar multi-core lapply
full <- pbmclapply(
  article_urls,
  function(x) {
    tryCatch(
      # Expression to execute "normally":
      expr = {
        article <- read_html(x)
        out <- data.frame(
          url         = x,
          date        = get_date(article),
          title       = get_title(article),
          author      = get_author(article),
          description = get_description(article),
          keywords    = get_keywords(article),
          paywall     = check_paywall(article),
          error       = NA_character_
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
      },
      # Function to execute if an error is caught:
      error = function(e) {
        e <- as.character(e[1])
        
        if (length(e) > 1)
          e <- paste(e, collapse = " ")
        
        out <- data.frame(
          url         = x,
          date        = NA_character_,
          title       = NA_character_,
          author      = NA_character_,
          description = NA_character_,
          keywords    = NA_character_,
          paywall     = NA_character_,
          body        = NA_character_,
          error       = e
        )
        return(out)
      }
    )
  },
  mc.cores = cores,
  mc.style = "ETA" # show estimated time to completion
)

full <- tibble(do.call(rbind, full))
write.csv(full, "welt_full.csv")
