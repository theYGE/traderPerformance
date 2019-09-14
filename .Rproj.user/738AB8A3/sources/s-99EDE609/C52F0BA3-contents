calculateAverageReturn <- function(stock, tickers) {
  dates <- c()
  returns <- c()
  
  SP500Values <- read.csv("SP500Data.csv")
  
  SP500Values = na.omit(SP500Values)
  
  datesSeq = seq(as.Date("2019-01-01"), Sys.Date(), by = 'day')
  
  startDate <- as.Date("2019-01-01")
  endDate <- Sys.Date()
  
  while (startDate <= endDate) {
    
    charDate <- as.character(startDate)
    dateSpecific <- SP500Values[SP500Values$ref.date == charDate,]
    
    startDate <- startDate + 1
    
    if (nrow(dateSpecific) == 0) {
      next
    }
    
    sumOfReturns <- 0
    numOfReturns <- 0
    
    for (i in 1:nrow(dateSpecific)) {
      
      row <- dateSpecific[i,]
      return <- (row$price.close/row$price.open - 1) * 100
      
      sumOfReturns <- sumOfReturns + return
      numOfReturns <- numOfReturns + 1
    }
    
    averageReturn <- sumOfReturns / numOfReturns
    
    dates <- append(dates, startDate)
    returns <- append(returns, averageReturn)
  }

  # print(length(returns))
  # print(length(dates))
  
  result <- data.frame("date" = dates, "return" = returns)
}

calculateCumulativeReturn <- function(stock) {
  dates <- c()
  returns <- c()
  
  firstIndex <- 1
  lastIndex <- nrow(stock)
  
  openPrice <- stock[1,]$price.open
  
  for (counter in seq(firstIndex, lastIndex)) {
    closePrice <- stock[counter,]$price.close
    date <- stock[counter, ]$ref.date
    
    cumulativeReturn <- ((closePrice - openPrice)/openPrice)*100
    
    dates <- append(dates, date)
    returns <- append(returns, cumulativeReturn)
  }
  
  result <- data.frame("date" = dates, "cumulativeReturn" = returns)
}

calculateReturn_TodayToLastMorning <- function(SP500Index){
  
  dates <- c()
  returns <- c()
  
  firstIndex <- 2
  lastIndex <- nrow(SP500Index)
  
  for (counter in seq(firstIndex, lastIndex)) {
    todayOpen <- SP500Index[counter,]$price.open
    yesterdayOpen <- SP500Index[counter-1,]$price.open
    date <- SP500Index[counter, ]$ref.date
    returnValue <- ((todayOpen - yesterdayOpen)/yesterdayOpen)*100
    
    dates <- append(dates, date)
    returns <- append(returns, returnValue)
  }
  
  result <- data.frame("date" = dates, "return" = returns)
}

calculateReturn_TodayOpenClosePrice <- function(SP500Index){
  
  dates <- c()
  returns <- c()
  
  firstIndex <- 1
  lastIndex <- nrow(SP500Index)
  
  for (counter in seq(firstIndex, lastIndex)) {
    todayOpen <- SP500Index[counter,]$price.open
    todayClose <- SP500Index[counter,]$price.close
    date <- SP500Index[counter, ]$ref.date
    print(date)
    returnValue <- ((todayClose - todayOpen)/todayOpen)*100
    
    dates <- append(dates, date)
    returns <- append(returns, returnValue)
  }
  
  # print(length(dates))
  result <- data.frame("date" = dates, "return" = returns)
}

calculateReturn_Rd <- function(SP500Index){
  
  dates <- c()
  returns <- c()
  
  firstIndex <- 1
  lastIndex <- nrow(SP500Index)
  
  for (counter in seq(firstIndex, lastIndex)) {
    todayOpen <- SP500Index[counter,]$price.open
    todayClose <- SP500Index[counter,]$price.close
    date <- SP500Index[counter, ]$ref.date
    returnValue <- ((todayClose - todayOpen)/todayOpen)
    print(date)
    dates <- append(dates, date)
    returns <- append(returns, returnValue)
  }
  
  # print(length(dates))
  result <- data.frame("date" = dates, "return" = returns)
}

calculateReturn_Rn <- function(SP500Index){
  
  dates <- c()
  returns <- c()
  
  firstIndex <- 1
  lastIndex <- nrow(SP500Index)
  
  for (counter in seq(firstIndex, lastIndex)) {
    print(counter)
    todayOpen <- SP500Index[counter,]$price.open
    if (counter == 1) {
      print('Equals')
      yesterdayClose <- todayOpen
    } else {
      yesterdayClose <- SP500Index[counter-1,]$price.close
    }
    date <- SP500Index[counter, ]$ref.date
    returnValue <- ((todayOpen - yesterdayClose)/yesterdayClose)
    print(date)
    print(returnValue)
    print(yesterdayClose)
    print(todayOpen)
    dates <- append(dates, date)
    returns <- append(returns, returnValue)
  }
  
  # print(length(dates))
  result <- data.frame("date" = dates, "return" = returns)
}

calculateReturn_RdCumulative <- function(returns_Rd){
  
  # dates <- c()
  # returns <- c()
  return = 1
  print(returns_Rd)
  
  firstIndex <- 1
  lastIndex <- nrow(returns_Rd)
  
  for (counter in seq(firstIndex, lastIndex)) {
    return <- return * (returns_Rd[counter,]$return + 1)
  }
  
  return <- return - 1
  result <- return
}
