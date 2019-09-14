library(BatchGetSymbols)
source("load_data.R")
library(lubridate)
source("returns.R")


# sp500Index = loadSP500Index()



testValues <- read.csv("Test.csv", header=TRUE, sep=",")
testValues$ref.date = as.Date(testValues$ref.date)

values_Rd <- calculateIntradayReturn(testValues)
values_Rn <- calculateOvernightReturn(testValues)

cumulative_Rd = calculateCumulativeReturn(values_Rd)
cumulative_Rn = calculateCumulativeReturn(values_Rn)

cumulative = calculateDailyCumulativeReturn(cumulative_Rd, cumulative_Rn)

uniqueMonths = unique(values_Rd$month)
for (monthVal in uniqueMonths) {
  # print(values_Rd[values_Rd$month == monthVal,])
  Rd_cumulative <- calculateReturn_RdCumulative(values_Rd[values_Rd$month == monthVal,])
  print(monthVal)
  print(Rd_cumulative)
}


# Rd_cumulative <- calculateReturn_RdCumulative(values_Rd)
# values_Rd$month <- month(as.POSIXlt(values_Rd$date, format="%Y/%m/%d"))



dates <- c(values_Rd$date)

dates(1)

whichmedian <- function(x) which.min(abs(x - median(x)))
x = 0
x = whichmedian(c(1,5,2))


SP500Values = na.omit(SP500Values)

dateSpecific <- SP500Values[SP500Values$ref.date == "2019-01-02",]
x = nrow(dateSpecific)
t = x == 0

dates = seq(as.Date("2019-01-01"), Sys.Date(), by = 'day')

sumPriceOpen <- sum(dateSpecific$price.open)
sumPriceClose <- sum(dateSpecific$price.close)

# SP5500Head <- read.csv(file="SP500.csv", header=TRUE, sep=",", colClasses = "character")
# tickers <- c(SP5500Head$company)
# 
# # tickers = c('^GSPC')
# 
# first.date <- as.Date('2019-01-01')
# last.date <- Sys.Date()
# 
# index <- BatchGetSymbols(tickers = tickers, first.date = first.date, last.date = last.date, do.cache = FALSE, do.complete.data = TRUE)
# write.csv(index, file = "SP500Data")
# index <- index$df.tickers
# write.csv(index, file = "SP500Data.csv")
# 
# print(l.out$df.control)
# print(l.out$df.tickers)
# 
# SP500Head <- read.csv(file="SP500.csv", header=TRUE, sep=",", colClasses = "character")
# row <- SP500Head[SP500Head$tickers == "3M Company",]
# cell <- row$company
# value <- cell$company
# str <- "X" + "Y"
