buyAndSellMedianProfitableStock <- function() {
  datesToReturn <- c()
  returns <- c()
  
  SP500Values <- read.csv("SP500Data.csv")
  
  mainReturn <- 0
  
  dates <- SP500Values$ref.date
  dates <- dates[!duplicated(dates)]
  
  for (index in 2:length(dates)) {
    
    date <- dates[index-1]
    charDate <- as.character(date)
    dateSpecific <- SP500Values[SP500Values$ref.date == charDate,]
    
    dateSpecificReturns <- c()
    
    for (i in 1:nrow(dateSpecific)) {
      
      row <- dateSpecific[i,]
      return <- (row$price.close - row$price.open )
      
      dateSpecificReturns <- append(dateSpecificReturns, return)
    }
    
    medianIndex = which.min(abs(dateSpecificReturns - median(dateSpecificReturns)))
    print(medianIndex)
    ticker = dateSpecific[medianIndex, ]$ticker
    
    date <- dates[index]
    charDate <- as.character(date)
    dateSpecific <- SP500Values[SP500Values$ref.date == charDate,]
    row <- dateSpecific[dateSpecific$ticker == ticker, ]
    
    stockProfit <- row$price.close - row$price.open
    mainReturn <- mainReturn + stockProfit
    
    datesToReturn <- append(datesToReturn, as.Date(charDate))
    returns <- append(returns, mainReturn)
    
  }
  
  result <- data.frame("date" = datesToReturn, "return" = returns)
}

buyAndSellLeastProfitableStock <- function() {
  datesToReturn <- c()
  returns <- c()
  
  SP500Values <- read.csv("SP500Data.csv")
  
  mainReturn <- 0
  
  dates <- SP500Values$ref.date
  dates <- dates[!duplicated(dates)]
  
  for (index in 2:length(dates)) {
    
    date <- dates[index-1]
    charDate <- as.character(date)
    dateSpecific <- SP500Values[SP500Values$ref.date == charDate,]
    
    dateSpecificReturns <- c()
    
    for (i in 1:nrow(dateSpecific)) {
      
      row <- dateSpecific[i,]
      return <- (row$price.close - row$price.open )
      
      dateSpecificReturns <- append(dateSpecificReturns, return)
    }
    
    minIndex = which.min(dateSpecificReturns)
    ticker = dateSpecific[minIndex, ]$ticker
    
    date <- dates[index]
    charDate <- as.character(date)
    dateSpecific <- SP500Values[SP500Values$ref.date == charDate,]
    row <- dateSpecific[dateSpecific$ticker == ticker, ]
    
    stockProfit <- row$price.close - row$price.open
    mainReturn <- mainReturn + stockProfit
    
    datesToReturn <- append(datesToReturn, as.Date(charDate))
    returns <- append(returns, mainReturn)
    
  }
  
  result <- data.frame("date" = datesToReturn, "return" = returns)
}

buyAndSellMostProfitableStock <- function() {
  datesToReturn <- c()
  returns <- c()
  
  SP500Values <- read.csv("SP500Data.csv")
  
  mainReturn <- 0
  
  dates <- SP500Values$ref.date
  dates <- dates[!duplicated(dates)]
  
  for (index in 2:length(dates)) {
    
    date <- dates[index-1]
    charDate <- as.character(date)
    dateSpecific <- SP500Values[SP500Values$ref.date == charDate,]
    
    dateSpecificReturns <- c()
    
    for (i in 1:nrow(dateSpecific)) {
      
      row <- dateSpecific[i,]
      return <- (row$price.close - row$price.open )
      
      dateSpecificReturns <- append(dateSpecificReturns, return)
    }
    
    maxIndex = which.max(dateSpecificReturns)
    ticker = dateSpecific[maxIndex, ]$ticker
    
    date <- dates[index]
    charDate <- as.character(date)
    dateSpecific <- SP500Values[SP500Values$ref.date == charDate,]
    row <- dateSpecific[dateSpecific$ticker == ticker, ]
    
    stockProfit <- row$price.close - row$price.open
    mainReturn <- mainReturn + stockProfit
    
    datesToReturn <- append(datesToReturn, as.Date(charDate))
    returns <- append(returns, mainReturn)
    
  }
  
  result <- data.frame("date" = datesToReturn, "return" = returns)
}