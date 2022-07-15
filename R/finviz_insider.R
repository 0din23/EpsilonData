finviz_insider_scraper <- function(){
  url <- "https://finviz.com/insidertrading.ashx"
  data <- xml2::read_html(url)
  tables <- data %>%
    rvest::html_nodes("table") %>%
    rvest::html_table()
  trades <- tables[[7]][-1,]
  colnames(trades) <- c("symbol", "owner", "relationship", "date", "transaction", "cost", "amount", "total_amount", "total_value", "F4_filling")

  trades$date <- trades$date %>%
    lapply(., function(x){

      date <- stringr::str_split(string = x, pattern = " ",n = 2) %>% unlist()
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

  trades$F4_filling <- trades$F4_filling %>%
    lapply(., function(x){

      x <-  substr(x, 1, 6)
      date <- stringr::str_split(string = x, pattern = " ",n = 2) %>% unlist()
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
  return(trades)
}
