# calculateAverageReturn <- function(stock, tickers) {
#   dates <- c()
#   returns <- c()
#   
#   SP500Values <- read.csv("SP500Data.csv")
#   
#   SP500Values = na.omit(SP500Values)
#   
#   datesSeq = seq(as.Date("2019-01-01"), Sys.Date(), by = 'day')
#   
#   startDate <- as.Date("2019-01-01")
#   endDate <- Sys.Date()
#   
#   while (startDate <= endDate) {
#     
#     charDate <- as.character(startDate)
#     dateSpecific <- SP500Values[SP500Values$ref.date == charDate,]
#     
#     startDate <- startDate + 1
#     
#     if (nrow(dateSpecific) == 0) {
#       next
#     }
#     
#     sumOfReturns <- 0
#     numOfReturns <- 0
#     
#     for (i in 1:nrow(dateSpecific)) {
#       
#       row <- dateSpecific[i,]
#       return <- (row$price.close/row$price.open - 1) * 100
#       
#       sumOfReturns <- sumOfReturns + return
#       numOfReturns <- numOfReturns + 1
#     }
#     
#     averageReturn <- sumOfReturns / numOfReturns
#     
#     dates <- append(dates, startDate)
#     returns <- append(returns, averageReturn)
#   }
# 
#   # print(length(returns))
#   # print(length(dates))
#   
#   result <- data.frame("date" = dates, "return" = returns)
# }
# 
# calculateCumulativeReturn <- function(stock) {
#   dates <- c()
#   returns <- c()
#   
#   firstIndex <- 1
#   lastIndex <- nrow(stock)
#   
#   openPrice <- stock[1,]$price.open
#   
#   for (counter in seq(firstIndex, lastIndex)) {
#     closePrice <- stock[counter,]$price.close
#     date <- stock[counter, ]$ref.date
#     
#     cumulativeReturn <- ((closePrice - openPrice)/openPrice)*100
#     
#     dates <- append(dates, date)
#     returns <- append(returns, cumulativeReturn)
#   }
#   
#   result <- data.frame("date" = dates, "cumulativeReturn" = returns)
# }
# 
# calculateReturn_TodayToLastMorning <- function(SP500Index){
#   
#   dates <- c()
#   returns <- c()
#   
#   firstIndex <- 2
#   lastIndex <- nrow(SP500Index)
#   
#   for (counter in seq(firstIndex, lastIndex)) {
#     todayOpen <- SP500Index[counter,]$price.open
#     yesterdayOpen <- SP500Index[counter-1,]$price.open
#     date <- SP500Index[counter, ]$ref.date
#     returnValue <- ((todayOpen - yesterdayOpen)/yesterdayOpen)*100
#     
#     dates <- append(dates, date)
#     returns <- append(returns, returnValue)
#   }
#   
#   result <- data.frame("date" = dates, "return" = returns)
# }
# 
# calculateReturn_TodayOpenClosePrice <- function(SP500Index){
#   
#   dates <- c()
#   returns <- c()
#   
#   firstIndex <- 1
#   lastIndex <- nrow(SP500Index)
#   
#   for (counter in seq(firstIndex, lastIndex)) {
#     todayOpen <- SP500Index[counter,]$price.open
#     todayClose <- SP500Index[counter,]$price.close
#     date <- SP500Index[counter, ]$ref.date
#     print(date)
#     returnValue <- ((todayClose - todayOpen)/todayOpen)*100
#     
#     dates <- append(dates, date)
#     returns <- append(returns, returnValue)
#   }
#   
#   # print(length(dates))
#   result <- data.frame("date" = dates, "return" = returns)
# }
# 
calculateIntradayReturn <- function(companyStocks){

  dates <- c()
  returns <- c()

  firstIndex <- 1
  lastIndex <- nrow(companyStocks)

  for (counter in seq(firstIndex, lastIndex)) {
    
    todayOpen <- companyStocks[counter,]$price.open
    todayClose <- companyStocks[counter,]$price.close
    
    date <- companyStocks[counter, ]$ref.date
    return <- ((todayClose - todayOpen)/todayOpen) 
    
    dates <- append(dates, date)
    returns <- append(returns, return)
  }
  
  result <- data.frame("date" = dates, "return" = returns)
}

calculateOvernightReturn <- function(companyStocks){

  dates <- c()
  returns <- c()

  firstIndex <- 1
  lastIndex <- nrow(companyStocks)

  for (counter in seq(firstIndex, lastIndex)) {
    
    todayOpen <- companyStocks[counter,]$price.open
    
    if (counter == 1) {
      yesterdayClose <- todayOpen
    } else {
      yesterdayClose <- companyStocks[counter-1,]$price.close
    }
    
    date <- companyStocks[counter, ]$ref.date
    return <- ((todayOpen - yesterdayClose)/yesterdayClose)
    
    dates <- append(dates, date)
    returns <- append(returns, return)
  }

  result <- data.frame("date" = dates, "return" = returns)
}

calculateCumulativeReturn <- function(return){


  cumulativeReturn = 1

  firstIndex <- 1
  lastIndex <- nrow(return)
  
  returns <- c()

  for (counter in seq(firstIndex, lastIndex)) {
    cumulativeReturn <- cumulativeReturn * (return[counter,]$return + 1)
    returns <- append(returns, cumulativeReturn)
  }
  
  result <- data.frame("date" = return$date, "return" = returns)
}

calculateDailyCumulativeReturn <- function(return_Rd, return_Rn) {
  dates <- c()
  returns <- c()
  
  firstIndex <- 1
  lastIndex <- nrow(return_Rd)
  
  for (counter in seq(firstIndex, lastIndex)) {
    
    dayReturn <- return_Rd[counter,]$return
    nightReturn <- return_Rn[counter,]$return
    date <- return_Rd[counter, ]$date
    
    return <- dayReturn*nightReturn
    
    dates <- append(dates, date)
    returns <- append(returns, return)
  }
  
  result <- data.frame("date" = dates, "return" = returns)
}
