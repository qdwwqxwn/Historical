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


# function to plot raw monthly Open data

plot_monthly <- function (ticker) { 

  # get monthly mean
  stock_monthly = read_sp500(ticker) 
  plot(stock_monthly$Open)

}


plot_components <- function(ticker) { 
  # get monthly mean
  stock_monthly = read_sp500(ticker) 
   # convert to time series, fill NA values  
  tom=na.approx( as.ts(stock_monthly$Open) ) 
  stom=stl(tom, t.window=15, s.window="periodic", robust=TRUE)
  cycle=stom$time.series[, 'seasonal']
  noise=stom$time.series[, 'remainder']
  y1=min(cycle, noise) 
  y2=max(cycle, noise) 

  r=cor(cycle, noise) 

  par(mfrow=c(2, 1)) 
  plot(tom, main=paste(toupper(ticker), " open, raw data", sep=''), lwd=5, 
        xlab="Date", ylab="Prices") 
  plot(cycle, col='black', ylim=c(y1, y2), lwd=5,  
         main=paste(toupper(ticker), " (open components) R=", round(r, 2), sep=''), 
         xlab="Date", ylab="Prices") 
  lines(noise, col='red') 

}

# return STL decomposition ouput, mainly for testing purposes 
get_stl <- function (ticker) { 
  stock_monthly = read_sp500(ticker) 
   # convert to time series, fill NA values  
  tom=na.approx( as.ts(stock_monthly$Open) ) 
  stom=stl(tom, t.window=15, s.window="periodic", robust=TRUE)
}

compute_SNR <- function (ticker) { 
   stock_monthly = read_sp500(ticker) 

   # convert to time series, fill NA values  
   tom=na.approx( as.ts(stock_monthly$Open) ) 

# STL decomposition,  plotted in a separate window
# one difference is STL returns the components of the same length
# as the original time series
   datalen = length(tom) 
   if (datalen > 12*3 ) { # only do it if more than three years of data
     stom=stl(tom, t.window=15, s.window="periodic", robust=TRUE)

     #dev.new()
     #plot(stom)

     mts=stom$time.series
     list( SNR= sd(mts[, 'seasonal']) / sd(mts[, 'remainder']), datalen=datalen)  
   } else {  # return NA for too-short data
     list(SNR=NA, datalen=datalen) 
   }

}



