
#Get population weighted temperature anomalies as "unconformatbility index."
# Treat cold and hot indecies separately. 

library(zoo) 


# get city population data 
# returns dataframe with Rank, City Name, and Population
get_cities <- function( )  { 
  data = read.csv("Cities/PEP_2014_PEPANNRSIP.US12A_with_ann.csv", skip=2, header=F)  
  #select Rank, City Name, and 2014 Population
  b=subset(data, select=c(V6, V8, V14)) 
  colnames(b)=c("Rank", "City", "Pop")
  cities=b
} 

# read city temperature data. All data start from 1/1/1995
# return a zoo object with "clim" and "anom" 
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
  # data = read.table(filename, sep="", header = F, na.strings='-99')
  
  # read into zoo 
  data=read.zoo(filename, index=1:3, format="%m %d %Y", na.strings='-99')

  data = na.approx(data) 
  temp = (data - 32) * 5 / 9 
  
  # get seasonal climatology, and duplicate to the same length as original data 
  nyr = floor(length(temp)/365) 
  tmp2d = matrix(temp[1:(nyr*365)], nrow=nyr, ncol=365, byrow=T) 
  sclim = colMeans(tmp2d) 
  longclim =array(sclim, dim=c(1, length(temp)))[1, ] 
  # convert to zoo
  clim = as.zoo(longclim)
  time(clim) = time(temp) 

  # anomalies 
  anom = temp - clim 
  merge(clim, anom) 
}


# Top 10 city names 
#   Rank                            City     Pop
#1     1         New York city, New York 8438379
#2     2    Los Angeles city, California 3897940
#3     3          Chicago city, Illinois 2722307
#4     4             Houston city, Texas 2203806
#5     5 Philadelphia city, Pennsylvania 1556052
#6     6           Phoenix city, Arizona 1512442
#7     7         San Antonio city, Texas 1411766
#8     8      San Diego city, California 1359844
#9     9              Dallas city, Texas 1260725
#10   10       San Jose city, California 1003821   <-- temperature data unavaiable 
#11   11                   Austin city, Texas  887124
#12   12           Jacksonville city, Florida  844014
#13   13       San Francisco city, California  841138
#14   14 Indianapolis city (balance), Indiana  843375
#15   15                  Columbus city, Ohio  823536
#16   16               Fort Worth city, Texas  794055 <-- temperature data unavaiable
#17   17       Charlotte city, North Carolina  793951

# Temperature city names
top15 = c("NYNEWYOR", "CALOSANG", "ILCHICAG", "TXHOUSTO", "PAPHILAD", 
          "AZPHOENI", "TXSANANT", "CASANDIE", "TXDALLAS", "TXAUSTIN", 
          "FLJACKSV", "CASANFRA", "ININDIAN", "OHCOLMBS", "NCCHARLO") 
ltop15 = c("New York city, New York", 
           "Los Angeles city, California",       
                 "Chicago city, Illinois",       
                    "Houston city, Texas",       
        "Philadelphia city, Pennsylvania",       
                  "Phoenix city, Arizona",       
                "San Antonio city, Texas",       
             "San Diego city, California",       
                     "Dallas city, Texas",       
                          "Austin city, Texas",       
                  "Jacksonville city, Florida",       
              "San Francisco city, California",       
        "Indianapolis city (balance), Indiana",       
                         "Columbus city, Ohio", 
              "Charlotte city, North Carolina") 

cities = get_cities() # get all the cities' pop 

ptindex = 0 
ntindex = 0 
for ( i in 1:length(top15) )  { 
  temp = get_city_temp( top15[i] ) 
  panom = temp$anom
  nanom = temp$anom
  panom[panom <0] = 0   # hot anomalies only
  nanom[nanom >0] = 0   # cold  anomalies only
  pop = subset(cities, City==ltop15[i])$Pop 
  ptindex = ptindex + panom * pop / 1000000    
  ntindex = ntindex + nanom * pop / 1000000    
} 

# do monthly mean
ptmonthly=aggregate(ptindex, as.yearmon, mean)
ntmonthly=aggregate(ntindex, as.yearmon, mean)



 





