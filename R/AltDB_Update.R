# A Function to update the alternative data db
#'
#' @param DB_NAME Name for the resulting database
#' @param DB_DIR Directory in which to put the database
#' @return Nothing but creates a DB in a directory

AltDB_Update <- function(DB_NAME, DB_DIR){

  # innit ----------------------------------------------------------------------
  require(dplyr)
  require(tidyquant)
  require(EpsilonUtility)

  # Connect to DB
  db_file <-paste0(DB_DIR, "/", DB_NAME, ".db")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db_file)

  # 13F Fillings ---------------------------------------------------------------
  SEC_13F <- get_13F_fillings() %>%
    data.table::rbindlist(., fill = TRUE)
  RSQLite::dbWriteTable(conn, "13F_TABLE", SEC_13F, append=TRUE, overwrite=FALSE)

  SEC_13F <- tbl(conn, "13F_TABLE") %>%
    data.frame() %>%
    distinct()
  RSQLite::dbWriteTable(conn, "13F_TABLE", SEC_13F, append=FALSE, overwrite=TRUE)

  # finviz news ----------------------------------------------------------------
  finviz_news <- finviz_news_scraper()
  RSQLite::dbWriteTable(conn, "FV_NEWS_TABLE", finviz_news, append = TRUE, overwrite=FALSE)

  finviz_news <- tbl(conn, "FV_NEWS_TABLE") %>%
    data.frame() %>%
    distinct()
  RSQLite::dbWriteTable(conn, "FV_NEWS_TABLE", finviz_news, append=FALSE, overwrite=TRUE)

  # finviz insider -------------------------------------------------------------
  finviz_insider <- finviz_insider_scraper()
  RSQLite::dbWriteTable(conn, "FV_INSIDER_TABLE", finviz_insider, append = TRUE, overwrite=FALSE)

  finviz_insider <- tbl(conn, "FV_INSIDER_TABLE") %>%
    data.frame() %>%
    distinct()
  RSQLite::dbWriteTable(conn, "FV_INSIDER_TABLE", finviz_insider, append=FALSE, overwrite=TRUE)

  # Table to save the creation date --------------------------------------------
  update_TABLE <- data.frame(
    "date"=Sys.Date() %>% as.character()
  )
  RSQLite::dbWriteTable(conn, "Update_TABLE", update_TABLE, append = FALSE, overwrite=TRUE)

  # finish up ------------------------------------------------------------------
  print(cat("Updated:", DB_NAME, "\nIn Folder:", DB_DIR))
}




