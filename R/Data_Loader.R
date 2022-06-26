Epsilon_Data_Loader <- function(ticker, db_type, MacroDB = MacroDB_FILE,
                                StockDB = StockDB_FILE){

  if(db_type == "stock"){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), StockDB)
    data <- dplyr::tbl(conn, "StockTS_TABLE") %>%
      filter(symbol %in% ticker) %>%
      data.frame()
  } else if(db_type == "macro"){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), MacroDB)
    data <- dplyr::tbl(conn, "Indicator_TS") %>%
      filter(symbol %in% ticker) %>%
      data.frame()
  }
  return(data)
}
