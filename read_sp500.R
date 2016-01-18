library(zoo)
library(xts) 

# function to read sp500 daily stock prices given a ticker, 
# and retrun a daily|weekly|monthly time series as a zoo object 
# Input: 
# ticker: string 
# frequency: 'daily'|'weekly'|'monthly' 
 
read_sp500 <- function (ticker, frequency) { 
  filename = paste("SP500/quantquote_daily_sp500_83986/daily/table_", 
                   tolower(ticker),  ".csv", sep="")

  stock=read.zoo(filename, index=1, format="%Y%m%d", sep=',')
  colnames(stock)=c("time", "Open", "High", "Low", "Close", "Volume")
 
  if (frequency == 'daily') { 
    return(stock) 
  } else if (frequency == 'weekly') { 
    return(apply.weekly(stock, mean)) 
  } else if (frequency == 'monthly') { 
    return (aggregate(stock, as.yearmon, mean)) 
    # or  return(apply.monthly(stock,  mean))
  } else  { 
    return(NA)
  } 
}


# function to plot raw Open data
# Input: 
# type: 'Open'|'High'|'Low'|'Close'|'Volume'
# frequency: 'daily'|'weekly'|'monthly' 

plot_stock <- function (ticker, type, frequency) { 

  stock = read_sp500(ticker, frequency) 
  plot(stock[, type])

}


# Only weeks with monthly data for now
plot_components <- function(ticker, type) { 
  stock = read_sp500(ticker, 'monthly') 
   # convert to time series, fill NA values  
  tom=na.approx( as.ts(stock[, type] ) ) 
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
get_stl <- function (ticker, type) { 
  stock_monthly = read_sp500(ticker,'monthly') 
   # convert to time series, fill NA values  
  tom=na.approx( as.ts(stock_monthly[, type]) ) 
  stom=stl(tom, t.window=15, s.window="periodic", robust=TRUE)
}

# only for monthly, for now 
compute_SNR <- function (ticker, type) { 
   stock_monthly = read_sp500(ticker, 'monthly') 

   # convert to time series, fill NA values  
   tom=na.approx( as.ts(stock_monthly[, type]) ) 

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



