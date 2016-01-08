
library(zoo) 

# read city temperature data. All data start from 1/1/1995
# return two time series in a list
# Usage examples: 
# data=get_city_temp("NYNEWYOR")
# plot(data$clim)
# plot(data$anom)
# data2=get_city_temp("MIFLINT")
# length(data2$clim)
# Sample data format: 
# 12            11            2015         54.3
# 12            12            2015         59.0
# 12            13            2015         57.1
# 12            14            2015         53.1

get_city_temp <- function(cityname) { 
  filename = paste("UDayton/", cityname, ".txt", sep='') 
  # get temperature field, and convert from deg-F to deg-C
  data = read.table(filename, sep="", header = F, na.strings='-99')
  
  # read into zoo 
  #data=read.zoo(filename, index=1:3, format="%m %d %Y", na.strings='-99')
  # temp = as.ts(data) 

  temp = na.approx(data$V4) 
  temp = (temp - 32) * 5 / 9 
  
  # get seasonal climatology, and duplicate to the same length as original data 
  nyr = floor(length(temp)/365) 
  tmp2d = matrix(temp[1:(nyr*365)], nrow=nyr, ncol=365, byrow=T) 
  clim = colMeans(tmp2d) 
  longclim =array(clim, dim=c(1, length(temp)))[1, ] 

  # anomalies 
  anom = temp - longclim
  anom.ts = ts(anom, start=c(1995, 1), frequency = 365)
  clim.ts = ts(longclim, start=c(1995, 1), frequency = 365)
  list(clim=clim.ts, anom=anom.ts) 

}



 





