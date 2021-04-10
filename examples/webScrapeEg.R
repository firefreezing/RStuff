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



# Example 3 - scrape my fantasy bball daily standing from Yahoo -----------

# some error here when calling read_html: invalid multibyte string

url_3 <- "https://basketball.fantasysports.yahoo.com/nba/26132/standings"

web_3 <- read_html(url_3)

# Sys.getlocale()
# 
# # [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
# 
# Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

node_3 <- html_nodes(web_3, ".Mt\(25px\):nth-child(5)")

dat_faculty_list <- html_text(node_2)

dat_faculty_list %>% str_trim("both")



# Example 4 Scrape data from CMS 2017 fact sheet ----------------------------

url_fs <- "https://www.cms.gov/Newsroom/MediaReleaseDatabase/Fact-sheets/2017-Fact-sheets.html"

web_fs <- read_html(url_fs)

fs_list <- html_table(web_fs) %>% .[[1]]

enrl_list <- fs_list %>%
  filter(str_detect(Title, "Weekly Enrollment Snapshot.+")) %>%
  arrange(Date) %>%
  mutate(Week = 1:length(Date)) %>%
  filter(Week > 2)


url <- "https://www.cms.gov/Newsroom/MediaReleaseDatabase/Fact-sheets/2017-Fact-Sheet-items/"

url <- str_c(url, enrl_list$Date, ".html")

parse_tbl <- function(url, week){
  
  web <- read_html(url)

  tbl <- html_table(web) %>% .[[2]] %>%
    set_names(c("state", "cumul_plan_cnt")) %>%
    .[-1, ] %>%
    mutate(week = week, 
           cumul_plan_cnt = str_replace_all(cumul_plan_cnt, ",", "") %>% as.numeric)
  
  return(tbl)  
}

state_plan_cnt <- map2_df(url, enrl_list$Week, parse_tbl) 


 # References --------------------------------------------------------------

# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
# https://stackoverflow.com/questions/28418770/using-rvest-or-httr-to-log-in-to-non-standard-forms-on-a-webpage
# https://rpubs.com/ryanthomas/webscraping-with-rvest
# Rselenium: https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html
# Rselenium tutorial: https://ropensci.org/tutorials/rselenium_tutorial/


