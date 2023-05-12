
source("SecondaryFunctions.R")

att<- readData("T")
verizon<- readData("VZ")
nvidia<- readData("NVDA")
amd <- readData("AMD")
btc <- readData("BTC-USD")
eth<- readData("ETH-USD")
raytheon <- readData("RTX")
lockheed <- readData("LMT")
pfizer <- readData("PFE")
moderna <- readData("MRNA")
tesla <- readData("TSLA")
nikola <- readData("NKLA")
sony<- readData("SONY")
northrop <- readData("NOC")
papajohn <- readData("PZZA")
dominoes <- readData("DPZ")
apple <- readData("AAPL")
kelloggs <- readData("K")
generalmills <- readData("GIS")
ford <- readData("F")
microsoft <- readData("MSFT")

compareStocks(lockheed, raytheon, k=0.5)

op_returns <- optimalReturn(lockheed, raytheon)
sum(op_returns)


k = .5
pos = getPositions(r, k)
plotRatio(r, k, col = "lightgray", ylab = "ratio")
showPosition(pos, r)

optimalReturn <- function(stockA, stockB, lengthTrain = "5 years"){
  overlap <- combine2Stocks(stockA,stockB, addRatio = T)
  r <- overlap$stockA/overlap$stockB
  
  i = 1:floor(nrow(overlap)/2)
  train = overlap[i,]
  test = overlap[- i,]
  
  #training data
  r.train = train$stockA/train$stockB
  r.test = test$stockA/test$stockB
  
  #we are going to train the model on the first 5 years of data from when we have data for both stocks
  #the rest of the data is for testing
  train.period = seq(min(overlap$Date), by = lengthTrain, length =2)
  #use this vector of length 2 of class Data to subset the stock price vectors to compute the ratio time series for the given sequence
  stockA.train = subset(stockA, Date >= train.period[1] &
                          Date < train.period[2])$Adj.Close
  stockB.train = subset(stockB,
                        Date >= train.period[1] &
                          Date < train.period[2])$Adj.Close
  r.train = stockA.train/stockB.train
  
  #now do the same thing but using the remaining data for the test data
  stockA.test = subset(stockA, Date > train.period[2] & Date < "2023-01-01")$Adj.Close
  stockB.test = subset(stockB, Date > train.period[2] & Date < "2023-01-01")$Adj.Close
  r.test = stockA.test/stockB.test
  
  #Now we want to find optimal value of k to maximize profit
  #choose largest k to be value such that the ratio never crosses these bounds and vice versa for min
  k.max = max((r.train - mean(r.train))/sd(r.train)) 
  k.min = min((abs(r.train - mean(r.train))/sd(r.train)))
  
  ks = seq(k.min, k.max, length = 1000)
  m = mean(r.train)
  
  profits =
    sapply(ks,function(k) {
      pos = getPositions(r.train, k)
      sum(positionProfit(pos, train$stockA, train$stockB))
    })
  
  plot(ks, profits, type = "l", xlab = "k", ylab = "Profit")
  
  #find k that maximizes profits for training data
  ks[profits == max(profits, na.rm=T)]
  
  #in case multiple k values yield same profit
  k.star = mean(ks[profits == max(profits, na.rm=T)], na.rm=T) 
  
  #calculate ROI on $1 investment on training data using optimal k
  roi<- max(profits, na.rm=T)
  
  #now, using this optimal k.star value on the test data
  pos = getPositions(r.test, k.star, mean(r.train), sd(r.train))
  testProfit = sum(positionProfit(pos, test$stockA,test$stockB), na.rm=T)
  testProfit
}
optimalReturn(apple, microsoft)
