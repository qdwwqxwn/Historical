library(zoo)
library(plyr)

# compute the relative fluctuation with a week
# defined as sd/mean  of the five weekdays

source("mean_by_weekday.R") 

files=list.files("SP500/quantquote_daily_sp500_83986/daily/", pattern="^table_")

nfiles = length(files) 

ticker = rep(NA, nfiles) 
fluc = rep(NA, nfiles) 

for (i in 1:length(files) ) { 
  #cat("doing ", files[i], "\n") 

  ticker[i] = sub(".csv$", "", sub("^table_", "", files[i]))
  out = mean_by_weekday(ticker[i], 'Open') 
  fluc[i] = sd(out$data)/mean(out$data) 
  # cat(ticker[i], SNR[i], "\n", sep="\t") 

} 
  
# output in descending order of fuc 
output = arrange(data.frame(ticker, fluc), desc(fluc) ) 
colnames(output)=c('ticker', 'weekly_fluctuation') 

# putput the top-25 
print(output[1:25, ]) 
 
write.csv(output, file='sp500_weekly_fluc.csv') 

