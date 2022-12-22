# Script to set up the scraping process on any computer. Will download the
# current development version.

# Please make sure to have R (and preferrably RStudio) installed on your PC before
# proceeding.

# Packages (will pull a lot of dependencies):
require("dplyr")
require("rvest")
require("tidyr")
require("rvest")
require("httr")
require("xml2")
require("purrr")

# Getting the scripts from the repository:
download.file(
  url = "Github-repo Zip download url", 
  destfile = "media_scrape_master.zip"
)

unzip(zipfile = "media_scrape_master.zip")
