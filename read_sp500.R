library(zoo)

stock=read.zoo("SP500/quantquote_daily_sp500_83986/daily/table_dov.csv", index=1, format="%Y%m%d", sep=',')
colnames(stock)=c("time", "Open", "High", "Low", "Close", "Volume")

# get monthly mean
stock_monthly = aggregate(stock, as.yearmon, mean)

# test time series analysis
tom=as.ts(stock_monthly$Open) 

# regular decomposition
dtom=decompose(tom)
plot(dtom)

# STL decomposition,  plotted in a separate window
# one difference is STL returns the components of the same length
# as the original time series
stom=stl(tom, t.window=15, s.window="periodic", robust=TRUE)
dev.new()
plot(stom)

#verify the components
# mts=stom$time.series
# mts[, 'trend']+mts[, 'seasonal']+mts[, 'remainder'] - tom



