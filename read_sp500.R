
stock=read.zoo("SP500/quantquote_daily_sp500_83986/daily/table_dov.csv", index=1, format="%Y%m%d", sep=',')
colnames(stock)=c("time", "Open", "High", "Low", "Close", "Volume")

# get monthly mean
stock_monthly = aggregate(stock, as.yearmon, mean)
