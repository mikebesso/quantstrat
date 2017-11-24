library(two.laws.big.bang)
library(two.laws.quant.data)
library(two.laws.quant.indicators)

library(two.laws.quant.charts)
library(lubridate)

library(quantmod)
library(TTR)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(foreach)
library(blotter)
library(quantstrat)
# library(egg)
# library(rlist)

# devtools::install_github("braverock/blotter")
# devtools::install_github("braverock/quantstrat")


DataSet <- LoadTestSymbol(amplitude = c(1, 2, 1, 2), slope = c(0.2, -0.5, -.1, -1.5))

DataSet %<>%
  AddMACD() %>%
  AddBBands() %>%
  df_filter(Date > ymd("2017-01-01"))

assign(
  attr(DataSet, "Symbol"), 
  xts(
    DataSet[, -1],
    order.by = DataSet[[1]]
  )
)

#StockChart$ggplot()

#####

initDate = "1990-01-01"
from = "2017-01-01"
to = "2017-11-01"
options(width = 70)

nLag = 252
pctATR = 0.02
period = 10



symbols <- attr(DataSet, "Symbol")
rm(list = ls(.blotter), envir = .blotter)

currency('USD')
Sys.setenv(TZ = "UTC")
stock(symbols, "USD", multiplier = 1)


osDollarATR <- function(
  orderside,
  tradeSize,
  pctATR,
  maxPctATR = pctATR,
  data,
  timestamp,
  symbol,
  prefer = "Open",
  portfolio,
  integerQty = TRUE,
  strMod = "",
  rebal = FALSE,
  ...
){
  return(100)
}

tradeSize <- 10000
initEq = tradeSize * length(symbols)

strategy.st <- "simple"
portfolio.st <- "simple"
account.st <- "simple"

rm.strat(portfolio.st)
rm.strat(strategy.st)

initPortf(
  portfolio.st,
  symbols = symbols,
  initPosQty = 0,
  initDate <- initDate,
  currency = 'USD'
)




initAcct(
  account.st,
  portfolios = portfolio.st,
  initDate <- initDate,
  currency = 'USD',
  initEq <- initEq
)

Account <- getAccount(account.st)

initOrders(portfolio.st, symbols = symbols, initDate = initDate)



strategy(strategy.st, store=TRUE)


add.signal(
  strategy.st,
  name = "sigCrossover",
  arguments = list(
    columns = c("MACD", "MACD.Signal"),
    relationship = "gt"
  ),
  label = "coverOrBuy"
)

add.signal(
  strategy.st,
  name = "sigCrossover",
  arguments = list(
    columns = c("MACD", "MACD.Signal"),
    relationship = "lt"
  ),
  label = "sellOrShort"
)

# Long Rules

add.rule(
  strategy.st,
  name = "ruleSignal",
  arguments = list(
    sigcol = "coverOrBuy",
    sigval = TRUE,
    ordertype = "market",
    orderside = "long",
    replace = FALSE,
    prefer = "Open",
    osFUN = osDollarATR,
    tradeSize = tradeSize,
    pctATR = pctATR,
    atrMod = "X"
  ),
  type = "enter",
  path.dep = TRUE
)

add.rule(
  strategy.st,
  name = "ruleSignal",
  arguments = list(
    sigcol = "sellOrShort",
    sigval = TRUE,
    orderqty = "all",
    ordertype = "market",
    orderside = "long",
    replace = FALSE,
    prefer = "Open"
  ),
  type = "exit",
  path.dep = TRUE
)



out <- applyStrategy(
  strategy = strategy.st,
  portfolios = portfolio.st
)

updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary[-1])
updateAcct(portfolio.st, dateRange)
updateEndEq(account.st)

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades", inclZeroDays = FALSE)

tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)





AddStrategy <- function(dataSet, portfolio.st, symbols){

  DataSet <- dataSet
  
  Portfolio <- getPortfolio(portfolio.st)
  symbols <- attr(DataSet, "Symbol")
  
  Prices <- Portfolio$symbols[[symbols]]$txn$Txn.Price
  
  Txn <- dplyr::data_frame(
    Date = as.Date(index(Portfolio$symbols[[symbols]]$txn$Txn.Qty)),
    Pos.Qty = coredata(Portfolio$symbols[[symbols]]$txn$Pos.Qty)[,1],
    Txn.Qty = coredata(Portfolio$symbols[[symbols]]$txn$Txn.Qty)[,1],
    Txn.Price = coredata(Portfolio$symbols[[symbols]]$txn$Txn.Price)[,1] 
  ) %>%
    df_mutate(
      Buy = if_else(Txn.Qty > 0, Txn.Qty, NA_real_),
      Buy.Price = if_else(Txn.Qty > 0, Txn.Price, NA_real_),
      Sell = if_else(Txn.Qty < 0, Txn.Qty, NA_real_),
      Sell.Price = if_else(Txn.Qty < 0, Txn.Price, NA_real_)
    )
  
  
  Txn <- Txn[-1,]
  

  
  Trades <- dplyr::data_frame(
    TradeStartDate = integer(0),
    TradeStartPrice = numeric(0), 
    TradeEndDate = integer(0),
    TradeEndPrice = numeric(0)
  )
  
  class(Trades$TradeStartDate) <- "Date"
  class(Trades$TradeEndDate) <- "Date"

  
  txn <- 1
  
  while (txn < nrow(Txn)){
  
    TradeStartDate <- Txn$Date[txn]
    TradeStartPrice <- Txn$Txn.Price[txn]

    txn <- txn + 1
    
      
    while (txn < nrow(Txn) && Txn$Pos.Qty[txn] != 0){
      
      
      message(txn)
      
      TradeEndDate <- Txn$Date[txn]
      TradeEndPrice <- Txn$Txn.Price[txn]
      
      Trades %<>%
        df_bind_rows(
          dplyr::data_frame(
            TradeStartDate = TradeStartDate,
            TradeStartPrice = TradeStartPrice, 
            TradeEndDate = TradeEndDate,
            TradeEndPrice = TradeEndPrice
          )
        )
      
      txn <- txn + 1
      
    }

  }
  

  attr(DataSet, "Trades") <- Trades 

  # We might have to handle the case of no data
  Position = na.locf(
    merge(
      Portfolio$symbols[[symbols]]$txn$Pos.Qty, 
      DataSet$Date
    )
  )  
  
  

  
  # We might have to handle the case of no data
  CumPL = cumsum(Portfolio$symbols[[symbols]]$posPL$Net.Trading.PL)
  if(length(CumPL) > 1){
    CumPL = na.omit(
      na.locf(
        merge(CumPL, DataSet$Date)
      )
    )
  } else {
    CumPL = NULL
  }
  
  if(!is.null(CumPL)) {
    CumMax <- cummax(CumPL)
    Drawdown <- -(CumMax - CumPL)
    Drawdown<-rbind(xts(-max(CumPL),order.by=first(index(Drawdown)-1)),Drawdown)
  } else {
    Drawdown <- NULL
  }  

  
  Position <- coredata(Position)[-1, 1]
  CumPL <- coredata(CumPL)[-1, 1]
  Drawdown <- coredata(Drawdown)[c(-1, -2), 1]
  
  DataSet %<>%
    df_mutate(
        Position = Position,
        CumPL = CumPL,
        Drawdown = Drawdown
      )
    
  
    

  return(DataSet)

}


DataSet <- AddStrategy(DataSet, portfolio.st)


Chart <- StockChartClass$new(DataSet)
Chart$ggplot()

