## helper function
get13F <- function(ii, tmp){

  ## meta information
  item_pub_date <- tmp[ii, "item_pub_date"] %>% as.character() # publication date
  item_link <- tmp[ii, "item_link"] %>% as.character() # link to filling
  item_title <- tmp[ii, "item_title"] %>% as.character() # fund name

  ## extract url
  filing <- xml2::read_html(item_link)
  TBLS <- xml2::xml_find_all(filing, "//table") %>%
    rvest::html_table()
  PATH <- TBLS[[2]][["File"]] %>% as.character()
  url <- paste0(item_link, PATH)

  ## get 13F
  Sys.sleep(rnorm(1,mean = 3,sd = 1))

  pg <- xml2::read_html(url)
  N <- pg %>%
    rvest::html_nodes("infotable") %>%
    length()

  DATA <- lapply(as.list(1:N), function(ii){
    xml2::as_list(
      xml2::xml_child(
        xml2::xml_child(
          xml2::xml_child(pg, 1),
          1),
        ii)) %>%
      unlist() %>%
      t() %>%
      as.data.frame()
  }) %>%
    data.table::rbindlist(., fill = TRUE) %>%
    as.data.frame() %>%
    mutate(
      "value" = as.numeric(value) * 1000,
      "shrsorprnamt.sshprnamt" = shrsorprnamt.sshprnamt %>% as.numeric(),
      "votingauthority.none" = votingauthority.none %>% as.numeric(),
      "votingauthority.shared" = votingauthority.shared %>% as.numeric(),
      "votingauthority.sole" = votingauthority.sole %>% as.numeric(),
      "item_pub_date" = item_pub_date,
      "fund" = item_title,
      "item_link" = item_link
    )

  return(DATA)
}

## main function ###############################################################
get_13F_fillings <- function(){

  ## get CUSIP data
  CUSIPS <- jsonlite::fromJSON(
    jsonlite::read_json("http://www.stockfries.com/static/json_df.txt")
  )
  df_cusips <- CUSIPS[["data"]] %>%
    as.data.frame()
  names(df_cusips) <- CUSIPS[["columns"]]

  urlRSS <- "https://sec.report/Form/13F-HR.rss"
  tmp <- tidyRSS::tidyfeed(urlRSS)
  tmp <- tmp %>% as.data.frame()
  tmp$item_title <- gsub(" - 13F-HR Quarterly Report", "",tmp$item_title)


  ## get Data
  ALL <- lapply(1:nrow(tmp),function(x){
    get13F(ii =  x, tmp = tmp)
  })

  return(ALL)

}


