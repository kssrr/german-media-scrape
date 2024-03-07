library(xml2)
library(dplyr)
library(purrr)
library(furrr)
library(rvest)
library(stringr)

index <- "https://www.bild.de/sitemap-index.xml"

sitemaps <- 
  index |> 
  read_html() |> 
  html_elements("loc") |> 
  html_text2() |> 
  str_subset("(?<=index-)202[0-2]")

find_articles <- function(sitemap) {
  sitemap |> 
    read_html() |> 
    html_elements("loc") |> 
    html_text2()
}

plan(multisession, workers = parallel::detectCores() - 1)

article_urls <- 
  sitemaps |> 
  future_map(find_articles, .progress = TRUE) |> 
  reduce(c)

# bild.de/bild-plus content ist scheinbar immer hinter einer 
# paywall & hat komische embedded-advertisement-Seiten die man
# sich nochmal genauer ansehen müsste:
article_urls <- article_urls[!grepl("bild-plus", article_urls)]

# Bei BILD ist fast alles in einem JSON-string verpackt:

get_json <- function(src) {
  response <- TRUE
  content <- ""
  
  try({
    content <- 
      src |> 
      html_element(xpath = '//script[@type="application/ld+json"]') |> 
      html_text2() |> 
      rjson::fromJSON() 
  })
  
  if (!is.list(content)) {
    response <- FALSE
  }
  
  list(response = response, content = content)
}

get_body <- function(src) {
  src |> 
    html_element("body") |> 
    html_element(xpath = '//*[@class="article-body"]') |> 
    html_text() |> 
    guard()
}


has_paywall <- function(src) {
  src |> 
    html_elements(xpath = '//meta[@name="ob:extras"]') |> 
    html_attr("content") |> 
    str_extract("(?<=isPremium=).*") |> 
    # This one is evil:
    str_to_upper() |> 
    as.logical()
}

scrape_article <- function(src) {
  json <- get_json(src)
  
  if (!json$response) {
    return()
  }
  
  json <- json$content
  
  article <- tibble(
    paywall = has_paywall(src),
    title = json$headline,
    description = json$description,
    keywords = json$keywords,
    # ^ manche haben keine keywords, aber da R hier scheinbar keinen 
    # Fehler wirft, einfach nicht anfassen
    date = json$datePublished,
    author = json$author[[1]]$name,
    body = NA_character_
  )
  
  if (!article$paywall) {
    article$body <- get_body(src)
  }
  
  article
}

scrape_safely <- function(article_url) {
  tryCatch(
    expr = {
      html <- read_html(article_url)
    },
    error = function(e) {
      cat(
        glue::glue("An error occured when fetching {url}:\n"), 
        as.character(e$message)
      )
      return()
    }
  )
  
  out <- scrape_article(html)
  out$url <- article_url
  out
}

# Falls nötig:
#plan(multisession, workers = parallel::detectCores() - 1)

articles <- future_map(article_urls, scrape_safely, .progress = TRUE)
articles <- articles |> list_rbind()

articles |> readr::write_csv("bild_full.csv")

has_paywall(read_html(article_urls[174]))
