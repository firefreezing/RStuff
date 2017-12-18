library(rvest)
library(tidyverse)
library(stringr)


# Example 1 - use html_table to directly scrape table ---------------------

# the main body in this page is a table
url <- "https://www.basketball-reference.com/leagues/NBA_2018_games.html"

# read the xml
web_1 <- read_html(url)

# use html_table to directly parse the table
tbl <- html_table(web_1) %>% .[[1]]

# dat <- html_nodes(webpage, ".right , .left")
# str(dat)
# tbl <- html_text(dat)


# Example 2 - faculty name from UTK math dept -----------------------------

url_2 <- "http://www.math.utk.edu/people/faculty.php"

web_2 <- read_html(url_2)

node_2 <- html_nodes(web_2, "br+ h6")

dat_faculty_list <- html_text(node_2)

dat_faculty_list %>% str_trim("both")

# References --------------------------------------------------------------

# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/


