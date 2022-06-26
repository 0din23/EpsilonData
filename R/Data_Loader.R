Epsilon_Data_Loader <- function(ticker, db_type, MacroDB = MacroDB_FILE,
                                StockDB = StockDB_FILE){

  if(db_type == "stock"){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), StockDB)
    data <- dplyr::tbl(conn, "StockTS_TABLE") %>%
      data.frame() %>%
      filter(symbol %in% ticker)
  } else if(db_type == "macro"){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), MacroDB)
    data <- dplyr::tbl(conn, "Update_TABLE") %>%
      data.frame() %>%
      filter(symbol %in% ticker)
  }
  return(data)
}
