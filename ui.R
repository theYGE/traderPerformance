library(shiny)
library(dplyr)
library(BatchGetSymbols)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput('companyInput', "Select a company", c())
    ),
    mainPanel(
      plotOutput("BuyMedianProfitableStock"),
      plotOutput("BuyLeastProfitableStock"),
      plotOutput("BuyMostProfitableStock"),
      plotOutput("AverageReturn"),
      plotOutput('CumulativeReturn'),
      plotOutput('CompanyStockPrices'),
      plotOutput("CompanyStock_TodayToLastMorning"),
      plotOutput("CompanyStock_TodayOpenClosePrice"),
      plotOutput('SP500Index'),
      plotOutput("SP500Return_TodayToLastMorning"),
      plotOutput("SP500Return_TodayOpenClosePrice")
    )
  )
)