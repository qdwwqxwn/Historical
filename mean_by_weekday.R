
# Compute mean by weekday, from daily data  
# Does not remove trend -- assuming averaged out
# Input: 
# ticker
# type: 'Open'|'Close'|'High'|'Low'|'Volume'

source("read_sp500.R") 

mean_by_weekday <- function (ticker, type) { 
   a=read_sp500(ticker, "daily")
   data=as.ts(a[, type])
   weekday=weekdays(as.Date(index(data)))
   wt=aggregate(data~weekday, FUN="mean")
} 




