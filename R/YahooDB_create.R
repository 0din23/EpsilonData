# A Function to use a ticker Catalogue to create a DB for the 0din Capital Infrastructure from scratch

#' Calculates a bunch of transformations on OHLC long format data
#'
#' @param DB_NAME Name for the resulting database
#' @param DB_DIR Directory in which to put the database
#' @param DB_TC Ticker Catalogue File
#' @return Nothing but creates a DB in a directory

YahooDB_Create <- function(DB_NAME, DB_DIR){

  # innit ----------------------------------------------------------------------
  require(dplyr)
  require(tidyquant)
  require(EpsilonUtility)

  # Connect to DB
  db_file <-paste0(DB_DIR, "/", DB_NAME, ".db")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db_file)

  # Metadata -------------------------------------------------------------------
  MData <- tq_index("SP500") %>%
    rbind(.,
          tq_index("SP400"),
          tq_index("SP600")
    ) %>%
    select(-c(shares_held, weight)) %>%
    distinct()

  # TS Table -------------------------------------------------------------------
  TICKER <- MData %>% pull(symbol) %>% unique()
  steps <- floor(length(TICKER) / 10)
  for(k in 1:10){
    if(k == 1){
      StockTS <- yfR::yf_get(
        tickers = TICKER[1:steps],
        first_date = "1900-01-01",
        thresh_bad_data = 0
      ) %>%
        select(date = "ref_date", symbol="ticker", open = "price_open",
               high="price_high", low="price_low", close= "price_close",
               volume, adjusted = "price_adjusted")

    } else if(k == 10) {
      w <- c((k*steps + 1):length(TICKER))
      data <- yfR::yf_get(
        tickers = TICKER[w],
        first_date = "1900-01-01",
        thresh_bad_data = 0
      ) %>%
        select(date = "ref_date", symbol="ticker", open = "price_open",
               high="price_high", low="price_low", close= "price_close",
               volume, adjusted = "price_adjusted")
      StockTS <- StockTS %>%
        rbind(.,
              data)

    } else {
      w <- c(((k-1)*steps):(k*steps))
      data <- yfR::yf_get(
        tickers = TICKER[w],
        first_date = "1900-01-01",
        thresh_bad_data = 0
      ) %>%
        select(date = "ref_date", symbol="ticker", open = "price_open",
               high="price_high", low="price_low", close= "price_close",
               volume, adjusted = "price_adjusted")
      StockTS <- StockTS %>%
        rbind(.,
              data)

    }
  }
  ticker <- StockTS %>% pull(symbol) %>% unique()
  new_ticker <- TICKER[!(TICKER %in% ticker)]
  data <- yfR::yf_get(
    tickers = new_ticker,
    first_date = "1900-01-01",
    thresh_bad_data = 0
  ) %>%
    select(date = "ref_date", symbol="ticker", open = "price_open",
           high="price_high", low="price_low", close= "price_close",
           volume, adjusted = "price_adjusted")
  StockTS <- StockTS %>%
    rbind(.,
          data)

  ticker <- StockTS %>% pull(symbol) %>% unique()
  new_ticker <- TICKER[!(TICKER %in% ticker)]

  # Table to save the creation date --------------------------------------------
  update_TABLE <- data.frame(
    "date"=Sys.Date()
  )

  # Write to DB ----------------------------------------------------------------
  MData <- MData %>%
    filter(!(symbol %in% new_ticker))
  RSQLite::dbWriteTable(conn, "Meta_TABLE", MData, append = FALSE, overwrite=TRUE)
  RSQLite::dbWriteTable(conn, "StockTS_TABLE", StockTS, append = FALSE, overwrite=TRUE)
  RSQLite::dbWriteTable(conn, "Update_TABLE", update_TABLE, append = FALSE, overwrite=TRUE)
  print(cat("Created:", DB_NAME, "\nIn Folder:", DB_DIR))
}




