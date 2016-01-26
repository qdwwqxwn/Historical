
# download and save SP500, from whatever is currently available  
# save to directory 'updated-yyyy-mm-dd'  

library(zoo)
library("quantmod")

#create directory 

ymd=format(Sys.time(), "%Y-%m-%d") 
dir=paste("updated-", ymd, sep='')
dir.create(dir, showWarnings = FALSE) 

# get ticker from the old files 
files=list.files("quantquote_daily_sp500_83986/daily/", pattern="^table_")

nfiles = length(files) 

for (i in 1:length(files) ) { 

  ticker = sub(".csv$", "", sub("^table_", "", files[i]))
  #cat("Doing ", toupper(ticker), "\n")  
  data=NULL
  try( data <- getSymbols(toupper(ticker), src='yahoo', auto.assign=FALSE) )  
  if ( is.null(data) ) { # try a different source
    try( data <- getSymbols(toupper(ticker), src='google', auto.assign=FALSE) )  
    if ( is.null(data) ) { # try a different source
      cat("Doing ", toupper(ticker), " failed \n")  
    } else { 
      write.zoo( data, paste(dir, "/", files[i], sep=''), sep=',') 
    }
  } else {   # only save valid data
    write.zoo( data, paste(dir, "/", files[i], sep=''), sep=',') 
  }

}  #for 
  
