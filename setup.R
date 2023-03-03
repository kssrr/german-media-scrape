# You can use this script to download all the code without git & install the 
# dependencies.

# Please make sure to have R (and preferrably RStudio) installed on your PC before
# proceeding.

# Packages (will pull a lot of dependencies):

pkgs <- c(
  "dplyr", "rvest", "tidyr", "httr", "xml2", 
  "purrr", "furrr", "progressr", "stringr"
)

invisible(
  lapply(pkgs, \(.x) require(.x, character.only = TRUE))
)

# Getting the scripts from the repository:
download.file(
  url = "https://github.com/kssrr/german-media-scrape/archive/refs/heads/master.zip", 
  destfile = "media_scrape_master.zip"
)

unzip(zipfile = "media_scrape_master.zip")

if (!dir.exists("german-media-scrape-master/data"))
  dir.create("german-media-scrape-master/data")
