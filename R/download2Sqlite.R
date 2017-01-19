#vim:ts=2:sts=2:sw=2:ht=2:expandtab:number:ruler:showmode:smartindent:ff=unix

#  download2Sqlite.R - A program to download security prices in time steps of 
#  seconds from Interactive Brokers via their TWS API.  This program will store 
#  the data into a Sqlite database file.
#  Author: Stergios Marinopoulos
#  Date Created: 2/1/2011
#
#  source( "download2Sqlite.R") ;

# This program will download a range of OHLC bars for 1 security at a time and
# save the OHLC bars to a sqlite database file.  The time range can be as long
# as you want, it is only limited by your persmissions.  The TWS API has a
# strong preference to return data in the host machine's local time zone, so
# this program saves it in the same way.

# To run this program, first edit a few variables under the "GLOBAL VARIABLES"
# sections below.  You will want to choose the security, the data start and end
# times, the bar size, and database file name at a minimum.

# Clear the work space!
rm(list = ls(all = TRUE))


library(DBI) ; 
library(RSQLite) ;
library(lubridate) ;
library(quantmod) ;  # xts would probably be enough, but I like to plot the output as a development tool.
require(IBrokers) ;

############################################################
#                 GLOBAL VARIABLES
############################################################


tzone       = "America/Los_Angeles" ; # Other popular options include "America/New_York", or "America/Chicago".
Old.TZ = Sys.getenv("TZ")   ;         # Old.TZ is usually an empty string in most R environments.
Sys.setenv(TZ=tzone) ;


# Parameters that you will likely change when you run this program.
# The time range to retreive historical data.  Note: this program counts time
# backwards, or counts down time.
startDT     = as.POSIXct("2017-01-19 11:15:01", tz = tzone) ;  # Download Time Start (we start here and go backwards in time.)
endDT       = as.POSIXct("2017-01-19 00:00:00", tz = tzone) ;  # Download Time End
barSize     = 1 ;             # We want to download 1 second bars. 
duration    = 2000 ;          # Number of bars to download at once.
dbFileName  = "CSV/YM-20170317-1SecBars.sqlite" ;
ticker      = "YM-20170317" ; # A convienence symbol name.  Only used for screen messages.
expiry      = "20170317" ;    # Expiration date
exchange    = "ECBOT"         # Exchange
symbol      = "YM" ;          # AKA "Local Symbol" by IB

# TWS Connection specific parameters
clientId    = 8 ;           # TWS API Client connection number
host        = 'localhost' ; # TWS is running on localhost
port        = 7496 ;        # TWS is listening on the defualt port
pauseBetween = 11 ;         # IB rate limits historical data reqiests to no more than 6 in 60 seconds.

# Column names of the historical data. Any data returned via reqHistoricalData()
# will have the ticker preprened to the column name. For example ESH7.Open,
# ESH7.Volume, etc.  Use the column names below as replacements.
gColNames   = c("start", "Open", "High", "Low", "Close", "Volume", "WAP", "hasGaps", "Count") ;


############################################################
#                   FUNCTIONS
############################################################

# A function to convert a POSIXct object into an IB API compatable time-stamp string
formatTWSDateTime = function(x) { 
  return(format(x, "%Y%m%d %H:%M:%S")) ;
}

############################################################
#
fetchStockData <- function(connection, contract, startDT, endDT, numberBars=2000, barSize=5) {
  
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
          conn        = connection,
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
  
  # Clean up the XTS object generated by getSymbols() / IB's reqHistoricalData()
  # Remove the ticker prefix from each column name that seems to be an xts
  # pattern. e.g. "ESH7.Volume" to "Volume", or "ESH7.Close" to "Close"
  names(allData) = gColNames[2:9] ;
  
  # IB delivers the volume in 100's. Adjust back to normal here.
  allData[, "Volume"] = allData[, "Volume"] * 100 ;

  # Chop off any extra data that was returned as a result of always asking for
  # the maximum number of 2000 bars.
  allData = allData[paste(as.character(endDT), "::", as.character(startDT), sep=""), ]

  return(allData) ;
}

############################################################
#                MAIN PROGRAM STARTS HERE                  #
############################################################

# Load the DBI sqlite driver.  (I'll use the DBI:: prefix to make clear which
# functions are from the DBI package)
drv    = DBI::dbDriver("SQLite") ;

# Connect the the Sqlite database specificed by 'dbFileName'.  By using the
# SQLITE_RWC flag the database file will be created if it does not exist.
db = tryCatch(DBI::dbConnect(drv, dbname=dbFileName, flags=SQLITE_RWC, cache_size=32000, synchronous="off" ),
              error=function(e) 
              {
                print(paste("Caught RSQLite Error: DBI::dbConnect() failed: ", dbFileName) ) ;
                print(e) ;
                stop("Stopping in Error Handler to DBI::dbConnect()") ;
                return(e) ;
              }
) ;

# Check to see if the 'Bars' table exists in the database.  If it does not then
# create it.
if ( DBI::dbExistsTable(db, "Bars") == FALSE ) {
  ddl = "CREATE TABLE Bars (
  start   INTEGER primary key,  
  Open    REAL,                 
  High    REAL,                 
  Low     REAL,                 
  Close   REAL,                 
  Volume  INTEGER,              
  WAP     REAL, 
  hasGaps INTEGER,
  Count   INTEGER,
  tsStart TIMESTAMP
  )" ;
  DBI::dbExecute(db, ddl) ;
}


# Connect to TWS API and fetch the historical data and store it in 'hData'.
tws         = twsConnect(clientId=clientId, host=host, port=port) ;
contractES  = twsFUT(symbol=symbol, exch=exchange, expiry=expiry, include_expired=1 ) ;
hData       = fetchStockData(connection=tws, contract=contractES, startDT=startDT, endDT=endDT, numberBars=2000, barSize=barSize) ;
twsDisconnect(tws) ;

# hData is a xts object.  Convert it to a data.frame and save it to a sqlite database.
df=as.data.frame(hData[paste(as.character(endDT), "::", as.character(startDT), sep=""), ]) ;
row.names(df) = as.numeric(index(hData)) ;  # Convert the row names in the data frame to the epoch time stamps of each xts row.
DBI::dbWriteTable(conn=db, name="Bars", value=df, append=TRUE, row.names = "start") ;

# DBI::dbWriteTable() does not save POSIXct objects properly to a sqlite
# TIMESTAMP type (even if I use the "field.types" argument to force sqlite type
# identification.)  So let sqlite update the rows properly.
firstRow2Update = as.numeric(row.names(head(df,n=1))) ;
lastRow2Update  = as.numeric(row.names(tail(df,n=1))) ;

fixTimeStampSQL = sprintf("UPDATE Bars SET tsStart=datetime(start, 'unixepoch', 'localtime') where start BETWEEN %d and %d;", firstRow2Update, lastRow2Update ) ;
DBI::dbExecute(db, fixTimeStampSQL) ;

# Disconnect from the database and we are done.
DBI::dbDisconnect(db) ;
