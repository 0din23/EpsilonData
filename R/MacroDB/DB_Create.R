# A Function to use a ticker Catalogue to create a DB for the 0din Capital Infrastructure from scratch

#' Calculates a bunch of transformations on OHLC long format data
#'
#' @param DB_NAME Name for the resulting database
#' @param DB_DIR Directory in which to put the database
#' @param DB_TC Ticker Catalogue File
#' @return Nothing but creates a DB in a directory

DB_Create <- function(DB_NAME, DB_DIR, DB_TC){

  # innit ----------------------------------------------------------------------
  require(dplyr)

  # Load Ticker Catalogue
  mData <- readxl::read_xlsx(DB_TC)

  # Connect to DB
  db_file <-paste0(DB_DIR, "/", DB_NAME, ".db")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db_file)

  # Metadata -------------------------------------------------------------------
  RSQLite::dbWriteTable(conn, "Meta_TABLE", mData, append = FALSE, overwrite=TRUE)

  # TS Table -------------------------------------------------------------------
  ## Asset Prices
  mAssets <- mData %>%
    filter(as.logical(ASSET)) %>%
    mutate(FROM = "1900-01-01",
           TO = Sys.Date()) %>%
    select(QUELLE, KEY, NAME,FROM, TO)
  dfAssets <- Data_Scraper_df(mAssets) %>%
    mutate(date = as.character(date))
  RSQLite::dbWriteTable(conn, "Asset_Price_TS", dfAssets, append = FALSE, overwrite=TRUE)

  ## Indicators
  mIndicators <- mData %>%
    filter(!as.logical(ASSET)) %>%
    mutate(FROM = "1900-01-01",
           TO = Sys.Date()) %>%
    select(QUELLE, KEY, NAME, FROM, TO)
  dfIndicators <- Data_Scraper_df(mIndicators) %>%
    mutate(date = as.character(date))
  RSQLite::dbWriteTable(conn, "Indicator_TS", dfIndicators, append = FALSE, overwrite=TRUE)

  ## Code for Aggregate Indicators Like Beta dispersion, number of positive Stocks, usw.
  ###############
  # PLATZHALTER #
  ###############

  # Update Table ---------------------------------------------------------------
  dfUpdate <- data.frame(
    "date"=Sys.Date() %>% as.character()
  )
  RSQLite::dbWriteTable(conn, "Update_TABLE", dfUpdate, append = FALSE, overwrite=TRUE)
  print(cat("Created:", DB_NAME, "\nIn Folder:", DB_DIR))
}
