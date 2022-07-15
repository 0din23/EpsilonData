finviz_news_scraper <- function(){
  require(dplyr)

  url <- "https://finviz.com/news.ashx"
  data <- xml2::read_html(url)
  tables <- data %>%
    rvest::html_nodes("table") %>%
    rvest::html_table()

  ## processing news -------------------------------------------------------------
  NEWS <- tables[[9]][,-1] %>%
    mutate("type" = "news")
  colnames(NEWS)<- c("date", "headline", "type")
  NEWS[stringr::str_detect(NEWS$date, pattern = "AM") | stringr::str_detect(NEWS$date, pattern = "PM"),"date"] <- Sys.Date() %>% as.character()
  NEWS$date[is.na(NEWS$date == Sys.Date())] <- NEWS$date[is.na(NEWS$date == Sys.Date())] %>%
    lapply(., function(x){

      date <- stringr::str_split(string = x, pattern = "-",n = 2) %>% unlist()
      month <- case_when(
        date[1] == "Jan" ~ "01",
        date[1] == "Feb" ~ "02",
        date[1] == "Mar" ~ "03",
        date[1] == "Apr" ~ "04",
        date[1] == "May" ~ "05",
        date[1] == "Jun" ~ "06",
        date[1] == "Jul" ~ "07",
        date[1] == "Aug" ~ "08",
        date[1] == "Sep" ~ "09",
        date[1] == "Oct" ~ "10",
        date[1] == "Nov" ~ "11",
        date[1] == "Dec" ~ "12"
      )
      year <- Sys.Date() %>% as.character() %>% substr(x=., 1,4)
      date <- paste0(year, "-", month, "-", date[2])
      return(date)

    }) %>%
    unlist()

  ## processing blogs ------------------------------------------------------------
  BLOGS <- tables[[10]][,-1] %>%
    mutate("type" = "blog")
  colnames(BLOGS)<- c("date", "headline", "type")

  BLOGS[stringr::str_detect(BLOGS$date, pattern = "AM") | stringr::str_detect(BLOGS$date, pattern = "PM"),"date"] <- Sys.Date() %>% as.character()
  BLOGS$date[is.na(BLOGS$date == Sys.Date())] <- BLOGS$date[is.na(BLOGS$date == Sys.Date())] %>%
    lapply(., function(x){

      date <- stringr::str_split(string = x, pattern = "-",n = 2) %>% unlist()
      month <- case_when(
        date[1] == "Jan" ~ "01",
        date[1] == "Feb" ~ "02",
        date[1] == "Mar" ~ "03",
        date[1] == "Apr" ~ "04",
        date[1] == "May" ~ "05",
        date[1] == "Jun" ~ "06",
        date[1] == "Jul" ~ "07",
        date[1] == "Aug" ~ "08",
        date[1] == "Sep" ~ "09",
        date[1] == "Oct" ~ "10",
        date[1] == "Nov" ~ "11",
        date[1] == "Dec" ~ "12"
      )
      year <- Sys.Date() %>% as.character() %>% substr(x=., 1,4)
      date <- paste0(year, "-", month, "-", date[2])
      return(date)

    }) %>%
    unlist()

  # bind and process further -----------------------------------------------------
  data <- rbind(BLOGS, NEWS) %>%
    mutate("scrape_date" = Sys.Date() %>% as.character())
  return(data)
}
