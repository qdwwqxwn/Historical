library(zoo)
library(plyr)


source("read_sp500.R") 

files=list.files("SP500/quantquote_daily_sp500_83986/daily/", pattern="^table_")

nfiles = length(files) 

ticker = rep(NA, nfiles) 
SNR = rep(NA, nfiles) 
datalen = rep(NA, nfiles) 

for (i in 1:length(files) ) { 
  #cat("doing ", files[i], "\n") 

  ticker[i] = sub(".csv$", "", sub("^table_", "", files[i]))
  out = compute_SNR(ticker[i], 'Open') 
  SNR[i] = out$SNR 
  datalen[i] = out$datalen 
  # cat(ticker[i], SNR[i], "\n", sep="\t") 

} 
  
# output in descending order of SNR
output = arrange(data.frame(ticker, SNR, datalen), desc(SNR) ) 
colnames(output)=c('ticker', 'SNR', 'nmonths')

# putput the top-25 
print(output[1:25, ]) 
 
write.csv(output, file='sp500_monthly_SNR.csv') 

