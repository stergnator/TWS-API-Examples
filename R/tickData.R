#vim:ts=2:sts=2:sw=2:ht=2:expandtab:number:ruler:showmode:smartindent:ff=unix


# tickData.R - A program to subscribe to misc stats including BidSize, 
# BidPrice, AskPrice, AskSize, Last, LastSize, and Volume from Interactive 
# Brokers via their TWS API.  A custom eWrapper object is created which 
# includes a method to print the state to the console as well. ( The code in 
# IBrokers eWrapper.MktData.CSV was heavily used for the custom eWrapper 
# object. ) This program subscribes to the top of the market misc stats for 1 
# security at a time and maintains state in 1 row xts object.   Finally, all
# data will be logged to a csv file as well.

#  Author: Stergios Marinopoulos
#  Date Created: 1/1/2013
#
#  source( "tickData.R") ;

# The TWS API has a strong preference to return data in the host machine's
# local time zone, so this program saves it in the same way.

# To run this program, first edit a few variables under the "GLOBAL VARIABLES"
# sections below.  You will want to choose the security, the data start and end
# times, the bar size, and database file name at a minimum.


## Clear the work space!
rm(list = ls(all = TRUE))



library(IBrokers)

############################################################
#                 GLOBAL VARIABLES                         #
############################################################

tzone = "America/Los_Angeles" ; # Other popular options include "America/New_York", or "America/Chicago".
oldTZ = Sys.getenv("TZ")   ;    # oldTZ is usually an empty string in most R environments.
Sys.setenv(TZ=tzone) ;
on.exit(Sys.setenv(TZ=oldTZ)) ;

# Parameters  you will likely change when you run this program.
ticker      = "ES-20170317" ; # A convienence symbol name.  Only used for screen messages.
expiry      = "20170317" ;    # Expiration date
exchange    = "GLOBEX" ;      # Exchange
symbol      = "ES" ;          # AKA "Local Symbol" by IB
fileNameCSV = "~/ES-Ticks.csv" ;

# TWS Connection specific parameters
clientId    = 8 ;             # TWS API Client connection number
# host        = 'localhost' ; # TWS is running on localhost
host        = '192.168.1.9' ; # TWS is running on server
port        = 7496 ;          # TWS is listening on the defualt port

############################################################
#             A CUSTOM eWrapper IMPLEMENTATION            #
############################################################

myEWrapper <- function() {
  eW <- eWrapper(NULL)  # use basic template
  eW$assign.Data("data", rep(list(structure(
                            .xts(matrix(rep(NA_real_,7),ncol=7),0, tzone="America/Los_Angeles"),
                            .Dimnames=list(NULL, c("BidSize","BidPrice", "AskPrice","AskSize", "Last","LastSize","Volume"))
                            )),
                         1)
  ) ;
  
  # The list of misc stats returned by the tickGeneric=165
  #                        1           2           3           4          5       6           7
  eW$assign.Data("headers", c("BidSize", "BidPrice", "AskPrice", "AskSize", "Last", "LastSize", "Volume") ) ;
  
  # Print the top of the market state to the console in a fixed width field format.
  eW$myPrint <- function(msg)
  {
    data = eW$get.Data("data") ;
    id   = as.numeric(msg[2]) ;
    d    = data[[id]] ;
    
    # Keep track of how often the headers are displayed.  Reprint them every 10 rows
    if( exists("headerCount", eW$.Data) ) {
      headerCount = eW$get.Data("headerCount") ;
    } else {
      headerCount = 0 ;
    }

    # Create the fixed width string of the data elements
    t = as.character(as.POSIXct(index(d[1,], origin="1970-01-01", tz="America/Los_Angeles" ))) ;
    v=as.vector(d[1,1:7])
    dataStr = sprintf("%-30s %10.0f %10.02f %10.02f %10.0f %10.02f %10.0f %10.0f",  t, v[1], v[2], v[3], v[4], v[5], v[6], v[7] );

    # Reprint headers every 10 rows
    if ( ( headerCount %% 10 ) == 0 ) {
      headers = eW$get.Data("headers") ;
      headerStr = sprintf("%-30s %10s %10s %10s %10s %10s %10s %10s", t, headers[1], headers[2], headers[3], headers[4], headers[5], headers[6], headers[7]  ) ;
      cat(headerStr, "\n") ;
    }

    # Print out the data
    cat(dataStr, "\n") ;
    headerCount = headerCount + 1 ;
    eW$assign.Data("headerCount", headerCount) ;


  }

  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) 
  {
    tickType = msg[3]
    msg <- as.numeric(msg)
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data") #[[1]]  # list position of symbol (by id == msg[2])
    attr(data[[id]], "index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    #data[[id]][1] <- timestamp
    if(tickType == .twsTickType$BID) {
      cat(paste(timestamp,
                msg[5], #bidSize
                msg[4], #bidPrice
                "",     #askPrice
                "",     #askSize
                "",     #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[[id]][nr.data,1:2] <- msg[5:4]
    } else
    if(tickType == .twsTickType$ASK) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                msg[4], #askPrice
                msg[5], #askSize
                "",     #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[[id]][nr.data,3:4] <- msg[4:5]
    } else
    if(tickType == .twsTickType$LAST) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                "",     #askPrice
                "",     #askSize
                msg[4], #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[[id]][nr.data,5] <- msg[4]
    }
    #data[[as.numeric(msg[2])]] <- data
    eW$assign.Data("data", data)
    eW$myPrint(msg) ;
    c(curMsg, msg)
  }
  eW$tickSize  <- function(curMsg, msg, timestamp, file, ...) 
  { 
    data <- eW$get.Data("data")
    tickType = msg[3]
    msg <- as.numeric(msg)
    id <- as.numeric(msg[2])
    file <- file[[id]]
    attr(data[[id]], "index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    #data[[id]][1] <- timestamp
    if(tickType == .twsTickType$BID_SIZE) {
      cat(paste(timestamp,
                msg[4], #bidSize
                "",     #bidPrice
                "",     #askPrice
                "",     #askSize
                "",     #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[[id]][nr.data,1] <- msg[4]
    } else
    if(tickType == .twsTickType$ASK_SIZE) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                "",     #askPrice
                msg[4], #askSize
                "",     #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[[id]][nr.data,4] <- msg[4]
    } else 
    if(tickType == .twsTickType$LAST_SIZE) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                "",     #askPrice
                "",     #askSize
                "",     #lastPrice
                msg[4], #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[[id]][nr.data,6] <- msg[4]
    } else
    if(tickType == .twsTickType$VOLUME) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                "",     #askPrice
                "",     #askSize
                "",     #lastPrice
                "",     #lastSize
                msg[4], #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[[id]][nr.data,7] <- msg[4]
    }
    eW$assign.Data("data", data)
    eW$myPrint(msg) ;
    c(curMsg, msg) # processMsg sees this raw vector
  }

  return(eW)
}


############################################################
#                MAIN PROGRAM STARTS HERE                  #
############################################################

# Connect to TWS API and request

tws           = twsConnect(clientId = clientId, host=host, port=port, verbose=1) #Connect to TWS via IB Gateway.
outputFileCSV = file(description=fileNameCSV, open = "w")                        # Also save data to this file.
contractES    = twsFUT(symbol=symbol, exch=exchange, expiry=expiry, include_expired=1 ) ; # Create a ES futures contract.
myWrapper     = myEWrapper() ;                                                   # Instantiate the custom eWrapper object.

reqMktData(conn=tws, Contract=contractES, tickGenerics="165", eventWrapper=myWrapper, file=outputFileCSV) ; # Subcribe to market data

# Code after here will never be reached because reqMktData() has an infinite loop.
close(outputFileCSV) ;
twsDisconnect(tws) ;

