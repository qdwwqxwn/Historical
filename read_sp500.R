library(zoo)

# function to read sp500 daily stock prices given a ticker, 
# and retrun a monthly time series as a zoo object 
 
read_sp500 <- function (ticker) { 
  filename = paste("SP500/quantquote_daily_sp500_83986/daily/table_", 
                   tolower(ticker),  ".csv", sep="")

  stock=read.zoo(filename, index=1, format="%Y%m%d", sep=',')
  colnames(stock)=c("time", "Open", "High", "Low", "Close", "Volume")

  # get monthly mean
  stock_monthly = aggregate(stock, as.yearmon, mean)

}

# function to compute SNR between seasonal and random
# Input: ticker of a SP500 stock 
# Oputput: ratio, with STL decomposition

compute_SNR <- function (ticker) { 
   stock_monthly = read_sp500(ticker) 

   # convert to time series 
   tom=as.ts(stock_monthly$Open) 

   # regular decomposition
   #  dtom=decompose(tom)
   #  plot(dtom)

# STL decomposition,  plotted in a separate window
# one difference is STL returns the components of the same length
# as the original time series
   if (length(tom) > 12*3 ) { # only do it if more than three years of data
     stom=stl(tom, t.window=15, s.window="periodic", robust=TRUE)

     #dev.new()
     #plot(stom)

     mts=stom$time.series
     sd(mts[, 'seasonal']) / sd(mts[, 'remainder']) 
   } else {  # return NA for too-short data
     NA
   }

}



