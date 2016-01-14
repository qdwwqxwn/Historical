library(zoo)


source("read_sp500.R") 

files=list.files("SP500/quantquote_daily_sp500_83986/daily/", pattern="^table_")


for (i in 1:length(files) ) { 
  cat("doing ", files[i], "\n") 

  ticker = sub(".csv$", "", sub("^table_", "", files[i]))
  SNR = compute_SNR(ticker) 
  cat(ticker, SNR, "\n", sep="\t") 

} 
  

