require("BatchGetSymbols");require("quantmod");require("pbapply")
require("data.table")
## tickers
tickers = c("AAPL","AMZN","BIDU","GOOGL","BKNG","TSLA","UBER","GLD","NFLX",
            "BRK.B","SPY","MSFT","FB","BABA")

## download data
data = BatchGetSymbols(tickers=tickers,first.date = "2000-01-01",last.date = Sys.Date(),
                       thresh.bad.data = 0.50, bench.ticker = "^GSPC",
                       type.return = "arit",freq.data = "daily",how.to.aggregate = "last",
                       do.complete.data = TRUE,do.fill.missing.prices = TRUE)
## output ticker summary
data$df.control

## extract price data
df = as.data.frame(data$df.tickers)

# adjust column names for TTR handling
names(df)[3:8] <- c("price.Open","price.High","price.Low",
                    "price.Close","Volume","price.Adjusted")

## calculating Indicator columns for each ticker
df = pblapply(as.list(unique(df$ticker)), function(x){
  # subset data by require ticker
  tmp = subset(df,df$ticker == x)
  # VWAP-20
  tmp$VWAP20  <- round(VWAP(Cl(tmp),volume = Vo(tmp),n = 20),2)
  # VWAP-50
  tmp$VWAP50  <- round(VWAP(Cl(tmp),volume = Vo(tmp),n = 50),2)
  # VWAP-100
  tmp$VWAP100 <- round(VWAP(Cl(tmp),volume = Vo(tmp),n = 100),2)
  # VWAP-200
  tmp$VWAP200 <- round(VWAP(Cl(tmp),volume = Vo(tmp),n = 200),2)
  # return data
  tmp
})
## rowbind data
df = rbindlist(df,use.names = TRUE)

## Extracting Adj Cl for each ticker
ADJ = pblapply(as.list(unique(df$ticker)), function(x){
  # subset data by require ticker
  tmp = subset(df,df$ticker == x)
  # extract indicator
  id = tmp[,c("ref.date","price.Adjusted")]
  # convert to xts
  id = xts(id[,2], order.by = as.Date(id$ref.date,format="%Y-%m-%d"))
  # format column name
  names(id) = x
  id
})
## merge Adj Cl & remove NAs
ADJ = na.omit(do.call(merge,ADJ))
