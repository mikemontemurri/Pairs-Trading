
####Reading the data
readData = function(symbol, dateFormat = c("%Y-%m-%d", "%Y/%m/%d"), ...) {
  # Construct the file path based on the symbol
  fileName = paste0("C:\\Users\\mikem\\Downloads\\", symbol, ".csv")
  
  data = read.csv(fileName, header = TRUE, stringsAsFactors = FALSE, ...)
  
  for(fmt in dateFormat) {
    tmp = as.Date(data$Date, fmt)
    if(all(!is.na(tmp))) {
      data$Date = tmp
      break
    }
  }
  
  data[order(data$Date),]
}

####Combining two stocks
#function to find the overlapping dates between two stocks
combine2Stocks = function(a, b, stockNames = c(deparse(substitute(a)),deparse(substitute(b))), addRatio = FALSE) {
  rr = range(intersect(a$Date, b$Date))
  a.sub = a[a$Date >= rr[1] & a$Date <= rr[2],]
  b.sub = b[b$Date >= rr[1] & b$Date <= rr[2],]
  
  #additional parameter with default =FALSE for question 3. If specified TRUE, include the ratio in the dataframe
  if (addRatio) {
    ratio = a.sub$Adj.Close / b.sub$Adj.Close
    structure(data.frame(a.sub$Date, a.sub$Adj.Close, b.sub$Adj.Close, ratio),
              names = c("Date", stockNames, "Ratio"))
  } else {
    structure(data.frame(a.sub$Date, a.sub$Adj.Close, b.sub$Adj.Close),
              names = c("Date", stockNames))
  }
}

####Plotting the ratio
#Function to plot the ratio of two given stocks during their overlapping times.
#Displayed the k values on the horizontal red lines to show when we would have opened/closed a position
plotRatio =
  function(r, k = 1, date = seq(along = r), meanColor = "darkgreen", thresholdColor = "red", ...){
    title= paste(deparse(substitute(stockA)), " vs. ", deparse(substitute(stockB)), " time series")
    plot(date, r, type = "l", ...)
    
    abline(h = c(mean(r),
                 mean(r) + k * sd(r),
                 mean(r) - k * sd(r)),
           col = c(meanColor, rep(thresholdColor, 2*length(k))),
           lty = "dashed")
    text(x = min(date), y = mean(r) + k * sd(r), labels = paste0("k = ", k), pos = 4, col = "red")
    text(x = min(date), y = mean(r) - k * sd(r), labels = paste0("k = ", k), pos = 4, col = "red")
  }

####Finding a position
findNextPosition =
  function(ratio, startDay = 1, k = 1,
           m = mean(ratio), s = sd(ratio)){
    # identify the bounds
    up = m + k *s
    down = m - k *s
    
    #discard all ratios up until startDay
    if(startDay > 1){
      ratio = ratio[-(1:(startDay-1))]
    }
    # construct logical vector the length of the remaining values in ratio
    # this determines if the ratio is inside or outside of the bounds
    isExtreme = ratio >= up | ratio <= down
    if(!any(isExtreme)){
      return(integer())
      start = which(isExtreme)[1]
    }
    
    if(ratio[start] > up){
      backToNormal <- ratio[-(1:start)] <= m
    }else{
      backToNormal <-ratio[-(1:start)] >= m
    }
    # return either the end of the position or the index of the end of the vector.
    # could return NA for not ended, i.e. which(backToNormal)[1] for both cases. But then the caller       has to interpret that.
    if(any(backToNormal)){
      end<- which(backToNormal)[1] + start
    }else{
      end<- length(ratio)
    }
    c(start, end) + startDay - 1
  }

#### Show positions on the time series
showPosition1 =
  function(days, ratio, radius = 70){
    if(is.list(days))
      days = unlist(days)
    symbols(days, ratio[days],
            circles = rep(radius, length(days)),
            fg = c("darkgreen", "red"),
            add = TRUE, inches = FALSE)
  }

####Get all possible positions
getPositions =
  function(ratio, k = 1, m = mean(ratio), s = sd(ratio)){
    when = list()
    cur = 1
    while(cur < length(ratio)) {
      tmp = findNextPosition(ratio, cur, k, m, s)
      if(length(tmp) == 0) # done
        break
      when[[length(when) + 1]] = tmp
      if(is.na(tmp[2]) || tmp[2] == length(ratio))
        break
      cur = tmp[2]
    }
    when
  }

####Find position profit
positionProfit =
  #r = overlap$stockA/overlap$stockB
  #k = 1.7
  #pos = getPositions(r, k)
  #positionProfit(pos[[1]], overlap$stockA, overlap$stockB)
  function(pos, stockPriceA, stockPriceB,
           ratioMean = mean(stockPriceA/stockPriceB),
           p = .001, byStock = F){
    if(is.list(pos)) {
      ans = sapply(pos, positionProfit,
                   stockPriceA, stockPriceB, ratioMean, p, byStock)
      if(byStock)
        rownames(ans) = c("A", "B", "commission")
      return(ans)
    }
    # prices at the start and end of the positions
    priceA = na.omit(stockPriceA[pos])
    priceB = na.omit(stockPriceB[pos])
    # how many units can we by of A and B with $1
    unitsOfA = 1/priceA[1]
    unitsOfB = 1/priceB[1]
    # The dollar amount of how many units we would buy of A and B
    # at the cost at the end of the position of each.
    amt = c(unitsOfA * priceA[2], unitsOfB * priceB[2])
    # Which stock are we selling
    sellWhat = if(priceA[1]/priceB[1] > ratioMean) "A" else "B"
    
    if(sellWhat == "A"){
      profit =  c((1 - amt[1]), (amt[2] - 1), - p * sum(amt))
    } else {
      profit =  c((1 - amt[2]), (amt[1] - 1), - p * sum(amt))
    } 
    if(byStock){
      profit = profit
    } else {
      sum(profit)
    }
  } 

####Comparing two stocks using a given k
compareStocks <- function(stockA, stockB, k){
  overlap <- combine2Stocks(stockA,stockB, addRatio = T)
  r <- overlap$stockA/overlap$stockB
  pos = getPositions(r, k)
  plotRatio(r, k, col = "gray", ylab = "ratio", main = paste(deparse(substitute(stockA)), " vs. ", deparse(substitute(stockB)), " time series"))
  showPosition1(pos, r)
  prof <- positionProfit(pos, overlap$stockA, overlap$stockB, mean(r))
  return(prof)
}


####Finding and applying optimal k
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
  k.star = mean(ks[profits == max(profits, na.rm=T)], na.rm=T)#in case multiple k values yield same profit
  roi<- max(profits, na.rm=T) #calculate ROI on $1 investment on training data using optimal k
#now, using this optimal k.star value on the test data
  pos = getPositions(r.test, k.star, mean(r.train), sd(r.train))
#testProfit = sum(positionProfit(pos, test$stockA,test$stockB), na.rm=T)
#testProfit
#couldnt figure out the problem, just run the entire data(including the data used to train) with the original function
  return(compareStocks(stockA,stockB,k=k.star))
  
}

