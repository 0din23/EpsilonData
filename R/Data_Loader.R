Epsilon_Data_Loader <- function(ticker, db_type, MacroDB = MacroDB_FILE,
                                StockDB = StockDB_FILE){

  require(dplyr)
  if(db_type == "stock"){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), StockDB)
    data <- dplyr::tbl(conn, "StockTS_TABLE") %>%
      dplyr::filter(symbol %in% ticker) %>%
      data.frame()
  } else if(db_type == "macro"){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), MacroDB)
    data <- dplyr::tbl(conn, "Indicator_TS") %>%
      dplyr::filter(symbol %in% ticker) %>%
      data.frame()
  } else if(db_type == "etf"){
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), MacroDB)
    data <- dplyr::tbl(conn, "Asset_Price_TS") %>%
      dplyr::filter(symbol %in% ticker) %>%
      data.frame()
  }
  RSQLite::dbDisconnect(conn = conn)
  return(data)
}
