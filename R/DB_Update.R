# A Function to use a ticker Catalogue to create a DB for the 0din Capital Infrastructure from scratch

#' Calculates a bunch of transformations on OHLC long format data
#'
#' @param DB_FILE Path to DB
#' @param DB_TC Ticker Catalogue File
#' @return Nothing but creates a DB in a directory

DB_Update <- function(DB_NAME, DB_DIR, DB_TC){

  # innit ----------------------------------------------------------------------

  # Connect to DB
  db_file <-paste0(DB_DIR, "/", DB_NAME, ".db")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db_file)

  # Get Update Table
  update_table <- dplyr::tbl(conn, "Update_TABLE") %>%
    data.frame()

  # Load Ticker Catalogue and check if something is new
  mData_old <- dplyr::tbl(conn, "Meta_Table") %>%
    data.frame()
  mData_new <- readxl::read_xlsx(DB_TC)
  new <- mData_new %>%
    rbind(., mData_old) %>%
    group_by_all %>%
    filter(n() == 1)
  RSQLite::dbWriteTable(conn, "Meta_TABLE", mData_new, append = FALSE, overwrite=TRUE)

  ## Asset Prices
  mAssets <- mData_new %>%
    filter(as.logical(ASSET)) %>%
    mutate(FROM = if_else(KEY %in% new$KEY, "1900-01-01",
                          update_table$Last_Update[2]),
           TO = Sys.Date()) %>%
    select(QUELLE, KEY, NAME,FROM, TO)

  dfAssets <- Data_Scraper_df(mAssets) %>%
    mutate(date = as.character(date))

  RSQLite::dbWriteTable(conn, "Asset_Price_TS", dfAssets, append = TRUE, overwrite=FALSE)

  ## Indicators
  mIndicators <- mData %>%
    filter(!as.logical(ASSET)) %>%
    mutate(FROM = if_else(KEY %in% new$KEY, "1900-01-01",
                          update_table$Last_Update[3]),
           TO = Sys.Date()) %>%
    select(QUELLE, KEY, NAME, FROM, TO)

  dfIndicators <- Data_Scraper_df(mIndicators) %>%
    mutate(date = as.character(date))

  RSQLite::dbWriteTable(conn, "Indicator_TS", dfIndicators, append = TRUE, overwrite=FALSE)

  # Update Table ---------------------------------------------------------------
  dfUpdate <- data.frame(
    "Table_Name"=c("Meta_Table", "Asset_Price_TS", "Indicator_TS"),
    "Last_Update"=as.character(rep(Sys.Date(), ))
  )
  RSQLite::dbWriteTable(conn, "Update_TABLE", dfUpdate, append = FALSE, overwrite=TRUE)

  print(cat("Updated:", DB_NAME, "\nIn Folder:", DB_DIR))

}
