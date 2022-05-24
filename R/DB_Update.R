# A Function to use a ticker Catalogue to create a DB for the 0din Capital Infrastructure from scratch

#' Calculates a bunch of transformations on OHLC long format data
#'
#' @param DB_FILE Path to DB
#' @param DB_TC Ticker Catalogue File
#' @return Nothing but creates a DB in a directory

DB_Update(DB_NAME, DB_DIR, DB_TC){

  # innit ----------------------------------------------------------------------

  # Load Ticker Catalogue and check if something is new
  mData_old <- dplyr::tbl(conn, "Update_TABLE") %>%
    data.frame()
  mData_new <- readxl::read_xlsx(DB_TC)

  # Connect to DB
  db_file <-paste0(DB_DIR, "/", DB_NAME, ".db")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),db_file)

  # Get Update Table
  update_table <- dplyr::tbl(conn, "Update_TABLE") %>%
    data.frame()

  # Metadata -------------------------------------------------------------------

  # Update Table ---------------------------------------------------------------

  # TS Table -------------------------------------------------------------------





}
