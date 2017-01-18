#vim:ts=2:sts=2:sw=2:ht=2:expandtab:number:ruler:showmode:smartindent:ff=unix

#  downloadSecondBars.R - A program to download futures prices in time steps of 
#  seconds from Interactive Brokers via their TWS API.
#  Author: Stergios Marinopoulos
#  Date Created: 2/1/2011
#
#  source( "downloadSecondBars.R") ;

# This program will download a range of OHLC bars for 1 futures contract at a 
# time and save the OHLC bars to a file (if the file already exists it will 
# merge the new data into the file without overwriting existing data, ensuring 
# the timestamps are unique).  The time range can be as long as you want, it is 
# only limited by your persmissions.  The TWS API has a strong preference to 
# return data in the host machine's local time zone, so this program saves it in
# the same way.

## Clear the work space!
rm(list = ls(all = TRUE))

# library(timeSeries) ;
# library(forecast) ;
library(quantmod) ;
require(IBrokers) ;
library(lubridate)

############################################################
#                 GLOBAL VARIABLES
############################################################

tzone       = "America/Los_Angeles" ; # Other popular options include "America/New_York", or "America/Chicago"
Old.TZ = Sys.getenv("TZ")   ;         # Old.TZ is usually an empty string in most R environments
Sys.setenv(TZ=tzone) ;

# The time range to retreive historical data.  Note: this program counts time
# backwards, or counts down time.
startDT     = as.POSIXct("2017-01-18 10:00:05", tz = tzone) ;  # Download Time Start (we start here and go backwards in time.)
endDT       = as.POSIXct("2017-01-18 00:00:00", tz = tzone) ;  # Download Time End
barSize     = 1 ;             # We want to download 1 second bars. 
duration    = 2000 ;          # Number of bars to download at once.
fileName    = "CSV/YM-20170317-1sec.csv" ;
fileNameOld = "CSV/YM-20170317-1sec.old" ;
ticker      = "YM-20170317" ; # A convienence symbol name.  Only used for screen messages.
expiry      = "20170317" ;    # Expiration date
exchange    = "ECBOT"         # Exchange
symbol      = "YM" ;          # AKA "Local Symbol" by IB


clientId    = 8 ;           # TWS API Client connection number
host        = 'localhost' ; # TWS is running on localhost
port        = 7496 ;        # TWS is listening on the defualt port
pauseBetween = 11 ;         # IB rate limits historical data reqiests to no more than 6 in 60 seconds.


# Column names of the historical daily data. Any data returned via
# reqHistoricalData() will have the ticker preprened to the column name. For
# example ESH7.Open, ESH7.Volume, etc.  Use the column names below as
# replacements.
gColNames   = c("DateTime", "Open", "High", "Low", "Close", "Volume", "WAP", "hasGaps", "Count") ;

############################################################
#                   FUNCTIONS
############################################################

# A function to convert a POSIXct object into an IB API compatable time-stamp string
formatTWSDateTime = function(x) { 
  return(format(x, "%Y%m%d %H:%M:%S")) ;
}

############################################################
# Load the existing ticker data, and return it as a xts object.
# If the file does not exist or is empty return NULL.
#
loadStockFile <- function(ticker, fileName) {

  ## DEBUG   cat(paste("Loading file data: ", ticker , "\n", sep="") ) ;
  
  if ( ! file.exists( fileName ) ) {
    ## DEBUG cat(paste("\tNew Ticker Symbol=", ticker , " Full download from 1992-01-01\n", sep="") ) ;
    return( NULL ) ;
  }
  
  # read.csv returns a data.frame
  csvData = read.csv( fileName, header=TRUE, col.names = gColNames, stringsAsFactors=FALSE ) ;
  cat(str(csvData,"\n")) ;
  # WORKS    csvData = zd = read.zoo( fileName, index.column = 1, sep = ",", FUN=as.POSIXct, header=TRUE, stringsAsFactors=FALSE ) ;
  
  if ( nrow(csvData ) < 1 ) {
    cat(paste("\tFile (", fileName, ") exists, but is empty.  No data for ", ticker, "... Returning NULL.\n", sep="") ) ;
    return( NULL ) ;
  }
  
  # Convert data.frame to an xts object while specifying the data's timezone
  # "UTC", or "US/Eastern"
  xtsData = xts( csvData[, -1], tzone=tzone, order.by=as.POSIXct(x=csvData[,1], tz="America/Los_Angeles", format="%Y-%m-%d %H:%M:%S") )
  indexTZ(xtsData) = tzone ;
  
  ## If I wanted to return data in a New York Time:
  ##    attr(time(newData),"tzone")="America/New_York"
  
  return( xtsData ) ;
}

############################################################
# Merge the old and new data into a new xts object, and then save that new xts
# object to a CSV file.  Optionally will rename the existing file if parameter
# "oldFileName" is supplied.
#
mergeAndSave <- function(oldData, newData, fileName, oldFileName=NULL) {

  # merge the two xts objects while removing any duplicates in newData - the 
  # preference is to keep the previously existing data.  Another approach maybe
  # make.index.unique(..., drop=TRUE, fromLast=FALSE) in xts
  
  combinedData = newData ;
  if( is.null(oldData) == FALSE ) {
    if ( nrow(oldData) > 0 ) {
      combinedData = rbind(oldData, newData[ ! index(newData) %in% index(oldData) ] ) ;
    } else {
      # nothing can be done with an 0 row xts data.  just save the newData.
    }
  }
  
  if ( file.exists( fileName ) && is.null(oldFileName) == FALSE) {
    file.rename(fileName, oldFileName) ;
  }
  write.zoo(combinedData, file=fileName, index.name="DateTime", quote=FALSE, sep="," ) ;
  return(combinedData) ;
}

############################################################
#
fetchStockData <- function(contract, startDT, endDT, numberBars=2000, barSize=5) {

  currentDT   = startDT ;
  requestSecs = numberBars * barSize ;
  barSizeStr  = paste(barSize, "secs")    # "1 secs", "5 secs"
  durationStr = paste(requestSecs, "S")   # "2000 S", "10000 S"

  while ( currentDT > endDT ) {
    s = currentDT ;
    e = s - as.duration(requestSecs) ;
    requestEndTimeStr = formatTWSDateTime(s) ;
    cat("Start=", as.character(s), ", End=", as.character(e), "\n" ) ;
    
    newData = 
      tryCatch(
        # Returns data in an xts object in your local timezone.
        reqHistoricalData(
          conn        = tws,
          Contract    = contract,
          endDateTime = requestEndTimeStr,
          barSize     = barSizeStr, # "1 secs", "5 secs"
          duration    = durationStr, # "2000 S", "10000 S"
          useRTH      = 0,
          whatToShow  = "TRADES" 
      ) , error=function(e){e}
    ) ;
  
    indexTZ(newData) = tzone ;
    ## If I wanted to return data in a New York Time Zone:
    ##    attr(time(newData),"tzone")="America/New_York"
    
    ## DEBUG    cat("Start=", as.character(s), ", End=", as.character(e), "\n" ) ;
    ## DEBUG    print(head(newData, n=1)) ;
    ## DEBUG    print(tail(newData, n=1)) ;
  
    if ( exists("allData") ) {
      # Merge the two xts objects while removing any duplicates in newData - the
      # preference is to keep the previously existing data.
      # There's also make.index.unique(..., drop=TRUE, fromLast=FALSE) in xts
      allData = rbind(allData, newData[ ! index(newData) %in% index(allData) ] )    
    } else {
      allData = newData ;
    }
    
    cat("\t...  Sleeping ", pauseBetween, "\n\n\n") ;
    Sys.sleep(as.integer(pauseBetween)) ;
    
    currentDT = e ;
  }

  # Finally, clean up the XTS object generated by getSymbols() / IB's reqHistoricalData()
  names(allData) = gColNames[2:9] ; # Remove the ticker prefix from each column name that seems to be an xts pattern.
                                    # e.g. "ESH7.Volume" to "Volume", or "ESH7.Close" to "Close"

  allData[, "Volume"] = allData[, "Volume"] * 100 ;  # IB delivers the volume in 100's. Adjust back to normal here.
   
  return(allData) ;
}

############################################################
# Main Program Starts Here
############################################################

setwd("~/") ;
tws         = twsConnect(clientId=clientId, host=host, port=port) ;
contractES  = twsFUT(symbol=symbol, exch=exchange, expiry=expiry, include_expired=1 ) ;
newData     = fetchStockData(contract=contractES, startDT=startDT, endDT=endDT, numberBars=2000, barSize=barSize) ;
oldData     = loadStockFile(ticker, fileName) ;
allData     = mergeAndSave(oldData, newData, fileName, fileNameOld) ;

# Example Parameters used for downloading 1 second bars on the INDU index.
# barSize     = 1 ;
# fileName    = "CSV/INDU-1Sec.csv" ;
# fileNameOld = "CSV/INDU-1Sec.old" ;
# ticker      = "INDU"
# condId      = 1935181
# secType     = "IND"
# exchange    = "NYSE"
# symbol      = "INDU"
# duration    = 2000
# contract = twsContract(conId=1935181, symbol=symbol, sectype=secType, exch=exchange, primary="", 
#                        expiry="",strike="", currency="USD", right="", local="",
#                        multiplier = "", combo_legs_desc="", comboleg="", include_expired="") ;
# newData     = fetchStockData(contract=contract, startDT=startDT, endDT=endDT, numberBars=duration, barSize=barSize) ;

twsDisconnect(tws) ;

# Useful plotting commands to see what you just downloaded.
# chartSeries(to.minutes(allData))
# chartSeries(to.minutes(newData))
# chartSeries(to.minutes(allData["2017-01-12 08:00:00::2017-01-12 14:00:00"]))