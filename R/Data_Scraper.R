# Functions to scrape the data or get it through an API

#' Scrape / Load one TS
#'
#' @param Quelle Quelle der Daten
#' @param KEY Key to identify the TS
#' @param NAME Optionaly a Name fro the DB
#' @param FROM Optionaly a Start Date
#' @param TO Optionaly a End Date
#' @param Reporting_Lag Optionaly a reporting lag
#' @return A time Series as a df
Data_Scraper <- function(QUELLE, KEY, NAME = NULL, FROM = NULL, TO=NULL, Reporting_Lag=NULL){

  ## Innit
  require(data.table)

  ## Load TS
  if(QUELLE == "DBB"){
    ts <- pdfetch::pdfetch_BUNDESBANK(KEY)
    ts <- ts %>%
      data.frame() %>%
      dplyr::mutate(date = rownames(ts)) %>%
      dplyr::select(date, all_of(KEY))
    if(!is.null(NAME)){
      colnames(ts) <- c("date", NAME)
    }
  } else if(QUELLE == "Yahoo"){
    ts <- tidyquant::tq_get(KEY)
  } else if(QUELLE == "FRED"){
    ts <- tidyquant::tq_get(KEY, "economic.data") %>%
      select(symbol, date, price)
  } else if(QUELLE == "BOE"){
  }

  ## Process TS
  if(!is.null(Reporting_Lag)){
    ts$date <- as.character(Reporting_Lag + as.Date(ts$date))
  }
  if(!is.null(FROM)){
    ts <- ts %>%
      dplyr::filter(as.Date(date) >= as.Date(FROM))
  }
  if(!is.null(TO)){
    ts <- ts %>%
      dplyr::filter(as.Date(date) <= as.Date(TO))
  }
  ts
}

#' Wrapper for Data_Scraper which takes a data.frame
#'
#' @param df a data.frame with columns relating to the parameters above
#' @return A time Series as a df in long format
Data_Scraper_df <- function(df){

  ## Innit
  require(data.table)

  lapply(1:nrow(df),function(x){
    Data_Scraper(QUELLE = df$QUELLE[x],
               KEY = df$KEY[x],
               NAME = df$NAME[x],
               FROM = df$FROM[x],
               TO = df$TO[x],
               Reporting_Lag = df$Reporting_Lag[x]
    )
  }) %>%
    data.table::rbindlist(., fill=TRUE)

}

