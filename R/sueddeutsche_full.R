#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(purrr)
library(rvest)
library(xml2)
library(furrr)
library(progressr)

# Sitemaps are numbered by year, but there is no
# obvious convention, so we are guessing numbers:
base_url <- "https://www.sueddeutsche.de/sitemapapp/sitemap/article/"
all <- expand.grid(as.character(2020:2022), as.character(1:20)) |>  arrange(Var1)
sitemaps <- paste0(base_url, all$Var1, "/", all$Var2)

get_urls <- function(sitemap) {
  out <- 
    sitemap |> 
    html_elements("url") |> 
    html_element("loc") |> 
    html_text2()
  
  if (rlang::is_empty(out)) return(NA_character_)
  out
}

try_sitemaps <- function(url, p) {
  p()
  
  tryCatch(
    expr ={
      url |> 
        rvest::read_html() |> 
        get_urls()
    },
    error = function(e) return()
  )
}

plan(multisession, workers = parallel::detectCores())

handlers(
  handler_progress(
    format = "[:bar] Remaining: :eta"
  )
)

with_progress({
  p <- progressor(steps = length(sitemaps))
  article_urls <- sitemaps |> future_map(\(.x) try_sitemaps(.x, p = p))
})

article_urls <- do.call(c, article_urls)

# There is a problem with SZ
# where return values get multiplied
# for no obvious reasons. Guarding
# against this, as well as empty
# returns:
guard <- function(x) {
  if (rlang::is_empty(x))
    return(NA_character_)
  else if (length(x) > 1)
    return(x[1])
  
  x
}

# Functions for information retrieval 
get_title <- function(src) {
  src |> 
    html_element("span") |>
    html_element(xpath = '//*[@data-manual="title"]') |> 
    html_text2() |> 
    guard()
}

get_meta <- function(src, query) {
  src |> 
    html_elements("meta") |> 
    html_element(xpath = paste0('//*[@name="', query, '"]')) |> 
    html_attr("content") |> 
    guard()
}

get_date <- function(src) {
  src |> 
    html_elements("time") |> 
    xml_attr("datetime") |> 
    guard()
}

check_paywall <- function(src) {
  if (grepl('"page.paywall_shown":true', html_text(src)))
    TRUE
  FALSE
}

get_body <- function(src) {
  src |>
    html_elements("p") |> 
    html_element(xpath = '//*[@data-manual="paragraph"]') |> 
    (\(.x) .x[1])() |> 
    html_text2()
}

full_scrape <- function(url, p) {
  p()
    
  tryCatch(
    expr = {
      article <- rvest::read_html(url)
        
      data.frame(
        url         = url,
        date        = get_date(article),
        title       = get_title(article),
        author      = get_meta(article, "author"),
        description = get_meta(article, "description"),
        keywords    = get_meta(article, "keywords"),
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
        error       = e,
        body        = NA_character_
      )
    }
  )
}

with_progress({
  p <- progressor(steps = length(article_urls))
  sueddeutsche <- article_urls |> future_map(\(.x) full_scrape(.x, p = p))
})

sueddeutsche |> 
  bind_rows() |>
  write.csv("sueddeutsche.csv")
