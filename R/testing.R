DB_DIR = "C:/0cap_2/Infrastructure/Ressources/"
DB_NAME = "YahooDB_1"

YahooDB_Create(DB_NAME = DB_NAME,
               DB_DIR = DB_DIR)
YahooDB_Update(DB_NAME = DB_NAME,
               DB_DIR = DB_DIR)

devtools::install_github('msperlin/yfR')
library(yfR)
data <- yfR::yf_get(tickers = c("AAPL", "F"),
                    first_date = "1900-01-01",
                    thresh_bad_data = 0
                    )
