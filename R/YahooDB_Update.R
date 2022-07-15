# A Function to use a ticker Catalogue to create a DB for the 0din Capital Infrastructure from scratch
#'
#' @param DB_NAME Name for the resulting database
#' @param DB_DIR Directory in which to put the database
#' @return Nothing but creates a DB in a directory

YahooDB_Update <- function(DB_NAME, DB_DIR){

  # innit ----------------------------------------------------------------------
  require(dplyr)
  require(tidyquant)
  require(EpsilonUtility)

  # Connect to DB
  db_file <-paste0(DB_DIR, "/", DB_NAME, ".db")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db_file)

  # Metadata -------------------------------------------------------------------
  update_table <- dplyr::tbl(conn, "Update_TABLE") %>%
    data.frame()
  start_date <- update_table[1,1] %>% as.Date()
  TICKER <- dplyr::tbl(conn, "Meta_TABLE") %>%
    pull(symbol) %>%
    unique()

  # TS Table -------------------------------------------------------------------
  steps <- floor(length(TICKER) / 10)
  for(k in 1:10){
    if(k == 1){
      StockTS <- yfR::yf_get(
        tickers = TICKER[1:steps],
        first_date = start_date,
        thresh_bad_data = 0
      ) %>%
        select(date = "ref_date", symbol="ticker", open = "price_open",
               high="price_high", low="price_low", close= "price_close",
               volume, adjusted = "price_adjusted")

    } else if(k == 10) {
      w <- c((k*steps + 1):length(TICKER))
      data <- yfR::yf_get(
        tickers = TICKER[w],
        first_date = start_date,
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
        first_date = start_date,
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
    first_date = start_date,
    thresh_bad_data = 0
  ) %>%
    select(date = "ref_date", symbol="ticker", open = "price_open",
           high="price_high", low="price_low", close= "price_close",
           volume, adjusted = "price_adjusted")
  StockTS <- StockTS %>%
    rbind(.,
          data)

  # Table to save the creation date --------------------------------------------
  update_TABLE <- data.frame(
    "date"=Sys.Date() %>% as.character()
  )
  StockTS <- StockTS %>%
    mutate(date = as.character(date))

  # Write to DB ----------------------------------------------------------------
  RSQLite::dbWriteTable(conn, "StockTS_TABLE", StockTS, append = TRUE, overwrite= FALSE)
  RSQLite::dbWriteTable(conn, "Update_TABLE", update_TABLE, append = FALSE, overwrite=TRUE)
  print(cat("Updated:", DB_NAME))
}




