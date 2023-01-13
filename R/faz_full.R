# NOTE: this script is still a work in progress!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
# manually.
cores <- detectCores()
article_urls <- pbmclapply(
  sitemaps,
  function(url) {
    src <- read_html(url)
    dates <- test |> 
      html_elements("url") |> 
      html_elements("lastmod") |> 
      html_text2()
    
    if (!TRUE %in% grepl("2022|2021|2020", dates))
      return()
    
    urlset <- test |> html_elements("url")
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
