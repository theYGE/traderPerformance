library(shiny)
library(dplyr)
library(BatchGetSymbols)
library(ggplot2)
library(reshape2)

source("load_data.R")
source("returns.R")
source("utils.R")
source("strategies.R")

server <- function(input, output, session) {
  
  sp500Index = loadSP500Index()
  
  SP500Head <- read.csv(file="SP500.csv", header=TRUE, sep=",", colClasses = "character")

  updateSelectInput(session, "companyInput", choices = SP500Head$tickers)
  
  output$BuyMedianProfitableStock <- renderPlot({
    x = 1
    returns <- buyAndSellMedianProfitableStock()
    print(returns)
    
    p <- ggplot(data = returns, aes(x = date, y = return)) +
      geom_line(color = "#0077FF", size = 1)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold", 
                                              size=14, angle=45))
    p <- p + xlab("Date") + ylab("Profit in $")
    
    p + ggtitle("S&P 500 Stocks Each Day Buying/Selling Stock That Was Median Profitable Yesterday") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$BuyLeastProfitableStock <- renderPlot({

    returns <- buyAndSellLeastProfitableStock()
    print(returns)
    
    p <- ggplot(data = returns, aes(x = date, y = return)) +
      geom_line(color = "#0077FF", size = 1)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold", 
                                              size=14, angle=45))
    p <- p + xlab("Date") + ylab("Profit in $")
    
    p + ggtitle("S&P 500 Stocks Each Day Buying/Selling Stock That Was Least Profitable Yesterday") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$BuyMostProfitableStock <- renderPlot({
    x = 1
    
    returns <- buyAndSellMostProfitableStock()
    print(returns)

    p <- ggplot(data = returns, aes(x = date, y = return)) +
      geom_line(color = "#0077FF", size = 1)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold", 
                                              size=14, angle=45))
    p <- p + xlab("Date") + ylab("Profit in $")
    
    p + ggtitle("S&P 500 Stocks Each Day Buying/Selling Stock That Was Most Profitable Yesterday") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$AverageReturn <- renderPlot({
    averageReturns <- calculateAverageReturn(sp500Index, c(SP500Head$company))
    # print(averageReturns)
    
    p <- ggplot(data = averageReturns, aes(x = date, y = return)) +
      geom_line(color = "#0077FF", size = 1)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold", 
                                              size=14, angle=45))
    p <- p + xlab("Date") + ylab("Return in %")
    
    p + ggtitle("S&P 500 Stocks Average Return") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$CumulativeReturn <- renderPlot({
    
    cumulativeReturns <- calculateCumulativeReturn(sp500Index)
    
    p <- ggplot(data = cumulativeReturns, aes(x = date, y = cumulativeReturn)) +
      geom_line(color = "#0077FF", size = 1)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold", 
                                              size=14, angle=45))
    p <- p + xlab("Date") + ylab("Return in %")
    
    p + ggtitle("S&P 500 Index Cumulative Return") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$CompanyStockPrices <- renderPlot({
    companyName <- input$companyInput
    
    if (companyName == "") {
      print("Empty")
      return (NULL)
    }
    
    companyTicker <- getTickerForCompany(companyName, SP500Head)
    values <- loadCompanyData(companyTicker)
    
    p <- ggplot(data = values, aes(x = ref.date, y = price.open)) +
      geom_line(color = "#0077FF", size = 2)
    
    p <- p + theme(axis.text.x = element_text(face="bold",
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold",
                                              size=14, angle=45))
    p <- p + ggtitle(companyName) +
      theme(plot.title = element_text(hjust = 0.5))
    
    return (p)
      
  })
  
  output$CompanyStock_TodayToLastMorning <- renderPlot({
    companyName <- input$companyInput
    
    if (companyName == "") {
      print("Empty")
      return (NULL)
    }
    
    companyTicker <- getTickerForCompany(companyName, SP500Head)
    values <- loadCompanyData(companyTicker)
    
    returns <- calculateReturn_TodayToLastMorning(values)
    
    p <- ggplot(data = returns, aes(x = date, y = return)) +
      geom_line(color = "#0077FF", size = 1)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold", 
                                              size=14, angle=45))
    p <- p + xlab("Date") + ylab("Return in %")
    
    p + ggtitle(paste(companyName, "Return Today To Last Morning")) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$CompanyStock_TodayOpenClosePrice <- renderPlot({
    companyName <- input$companyInput
    
    if (companyName == "") {
      print("Empty")
      return (NULL)
    }
    
    companyTicker <- getTickerForCompany(companyName, SP500Head)
    values <- loadCompanyData(companyTicker)
    
    returns <- calculateReturn_TodayOpenClosePrice(sp500Index)
    
    p <- ggplot(data = returns, aes(x = date, y = return)) +
      geom_line(color = "#0077FF", size = 1)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold", 
                                              size=14, angle=45))
    p <- p + xlab("Date") + ylab("Return in %")
    
    p + ggtitle(paste(companyName, "Today Open Close Return")) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$SP500Index <- renderPlot({
    
    p <- ggplot(data = sp500Index, aes(x = ref.date, y = price.adjusted)) +
      geom_line(color = "#0077FF", size = 2)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                         size=14, angle = 45),
              axis.text.y = element_text(face="bold", 
                                         size=14, angle=45))
    p + ggtitle("S&P 500 Index") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$SP500Return_TodayToLastMorning <- renderPlot({
    returns <- calculateReturn_TodayToLastMorning(sp500Index)
    
    p <- ggplot(data = returns, aes(x = date, y = return)) +
      geom_line(color = "#0077FF", size = 1)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold", 
                                              size=14, angle=45))
    p <- p + xlab("Date") + ylab("Return in %")
    
    p + ggtitle("S&P 500 Index Return Today To Last Morning") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$SP500Return_TodayOpenClosePrice <- renderPlot({
    returns <- calculateReturn_TodayOpenClosePrice(sp500Index)
    
    p <- ggplot(data = returns, aes(x = date, y = return)) +
      geom_line(color = "#0077FF", size = 1)
    
    p <- p + theme(axis.text.x = element_text(face="bold", 
                                              size=14, angle = 45),
                   axis.text.y = element_text(face="bold", 
                                              size=14, angle=45))
    p <- p + xlab("Date") + ylab("Return in %")
    
    p + ggtitle("S&P 500 Index Today Open Close Return") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
}