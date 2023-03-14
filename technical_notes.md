# Walkthrough of the scripts

The scripts utilize sitemaps to scrape articles from the archives of the news websites. Sitemaps are provided by the websites themselves and are usually linked in their `robots.txt`-file. For example, at [](https://www.zeit.de/robots.txt), we find: `Sitemap: https://www.zeit.de/gsitemaps/index.xml`. The URL leads to an archive that contains references to the actual sitemaps:

```{xml}
<sitemapindex>
<sitemap>
<loc>
https://www.zeit.de/gsitemaps/index.xml?date=2023-03-10&unit=days&period=1
</loc>
</sitemap>
<sitemap>
<loc>
https://www.zeit.de/gsitemaps/index.xml?date=2023-03-13&unit=days&period=1
</loc>
</sitemap>
<sitemap>
<loc>
https://www.zeit.de/gsitemaps/index.xml?date=2023-03-14&unit=days&period=1
</loc>
</sitemap>
</sitemapindex>
```

First, we retrieve this archive of sitemaps:

```{R}
# Load necessary packages first:
library(xml2)
library(dplyr)
library(tidyr)
library(rvest)
library(purrr)
library(furrr)
library(stringr)
library(progressr)

# This is the link to the sitemap archive:
url <- "https://www.zeit.de/gsitemaps/index.xml"
patterns <- "2020|2021|2022" # these are the years we want to look for

sitemaps <- url |> 
  httr::GET() |> 
  # read the xml:
  rvest::read_html() |> 
  # extract all elements with "loc"-tags:
  html_elements("loc") |> 
  html_text2() |> 
  # look for our patterns (= years)
  grep(patterns, x = _, value = TRUE)
```

Now, we have a character vector holding the URLs of the actual sitemaps:

```{R}
head(sitemaps)
#> [1] "https://www.zeit.de/gsitemaps/index.xml?date=2020-01-01&unit=days&period=1"
#> [2] "https://www.zeit.de/gsitemaps/index.xml?date=2020-01-02&unit=days&period=1"
#> [3] "https://www.zeit.de/gsitemaps/index.xml?date=2020-01-03&unit=days&period=1"
#> [4] "https://www.zeit.de/gsitemaps/index.xml?date=2020-01-04&unit=days&period=1"
#> [5] "https://www.zeit.de/gsitemaps/index.xml?date=2020-01-05&unit=days&period=1"
#> [6] "https://www.zeit.de/gsitemaps/index.xml?date=2020-01-06&unit=days&period=1"
```
