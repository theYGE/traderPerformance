library(BatchGetSymbols)

loadSP500Index <- function(){
  tickers = c('^GSPC')

  fromDate <- "2019-01-01"
  toDate <- Sys.Date()

  index <- BatchGetSymbols(tickers = tickers, first.date = fromDate, last.date = toDate, do.cache = FALSE)
  index <- index$df.tickers
}

loadCompanyData <- function(ticker) {
  tickers = c(ticker)

  fromDate <- "2019-01-01"
  toDate <- Sys.Date()

  index <- BatchGetSymbols(tickers = tickers, first.date = fromDate, last.date = toDate, do.cache = FALSE)
  index <- index$df.tickers
}

# loadSP500 <- function(){
#   SP500 <- read.csv(file = "SP500_Names", header = TRUE, stringsAsFactors = FALSE)
# }
#
# loadSP500Control <- function(){
#   control <- read.csv(file = "SP500_Control", header = TRUE, stringsAsFactors = FALSE)
# }
#
# loadSP500Tickers <- function(){
#   tickers <- read.csv(file = "SP500_Tickers", header = TRUE, stringsAsFactors = FALSE)
# }