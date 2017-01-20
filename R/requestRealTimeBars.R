#vim:ts=2:sts=2:sw=2:ht=2:expandtab:number:ruler:showmode:smartindent:ff=unix

#  requestRealTimeBars.R - A program that subscribes to 5 second updates on a 
#  security.  Once invoked, the most recent OHLC bar of five seconds will start
#  being delivered through the realtimeBars() callback on a eWrapper object. 
#  Additionally, this program draws a candlestick chart once every 30 seconds
#  on 1 minute OHLC bars created from the 5 second real time bars.

#  Author: Stergios Marinopoulos
#  Date Created: 12/1/2012
#
#  source( "requestRealTimeBars.R") ;

# To run this program, first edit a few variables under the "GLOBAL VARIABLES"
# sections below.  You will want to choose the security, the data start and end
# times, the bar size, and database file name at a minimum.

# Clear the work space!
rm(list = ls(all = TRUE))

library(lubridate) ;
library(quantmod) ;
require(IBrokers) ;

############################################################
#                 GLOBAL VARIABLES
############################################################

tzone = "America/Los_Angeles" ; # Other popular options include "America/New_York", or "America/Chicago".
oldTZ = Sys.getenv("TZ")   ;    # oldTZ is usually an empty string in most R environments.
Sys.setenv(TZ=tzone) ;
on.exit(Sys.setenv(TZ=oldTZ)) ;

# Parameters  you will likely change when you run this program.
ticker      = "ES-20170317" ; # A convienence symbol name.  Only used for screen messages.
expiry      = "20170317" ;    # Expiration date
exchange    = "GLOBEX"         # Exchange
symbol      = "ES" ;          # AKA "Local Symbol" by IB

# TWS Connection specific parameters
clientId    = 8 ;           # TWS API Client connection number
host        = 'localhost' ; # TWS is running on localhost
port        = 7496 ;        # TWS is listening on the defualt port

# Column names of the 5 second real time bars xts object.
gColNames   = c("start", "Open", "High", "Low", "Close", "Volume", "WAP", "Count") ;

# A data.frame template for the 5 second bars.  Everytime a new one arrives create a data.frame from it.
# Just initialize it with empty vectors:
  
gDF = data.frame(
  start=integer(),
  Open=double(),
  High=double(),
  Low=double(),
  Close=double(),
  Volume=integer(),
  WAP=double(),
  Count=integer(),
  stringsAsFactors=FALSE
) ;

emptyDF = gDF ;  # keep a copy of the empty data.frame

# For simplicity sake the program will store the 5 second real time bars into a
# global xts object. The IBrokers norm is to store data into an eWrapper owned
# environment called ".Data" and accessed via the get.Data() and assign.Data()
# eWrapper methods.

gXTS = NULL ;    # a global xts object that holds all 5 second real time bars.

# DEBUG    gMessage    = c() ; # a copy of the most recent "msg" received by realtimeBars()

############################################################
#                   FUNCTIONS
############################################################


# Prepare the event wrapper callback for real time bars.

ibRealTimeBars <- function(curMsg, msg, timestamp, file,...) {
  # DEBUG    gmsg <<- msg

  # Maintain/Update an xts object with the latest 5 second real time bar found
  # in the msg vector.  The msg vector layout is described below:
  #-----------------------------------------------------------------------------------
  #    id   time         Open      High      Low       Close     Vol  WAP       Count
  # 1   2   3            4         5         6         7         8    9         10
  # 3   1   1484868625   2262.75   2262.75   2262.75   2262.75   2    2262.75   1
  #-----------------------------------------------------------------------------------
  
  # Prepare a data.frame from the 5 second bar.
  newDF        = emptyDF ;
  newDF[1,1]   = as.integer(msg[3]) ;    # time
  newDF[1,2:5] = as.double(msg[4:7]) ;   # Open, High, Low, Close
  newDF[1,6]   = as.integer(msg[8]) ;    # Volume
  newDF[1,7]   = as.double(msg[9]) ;     # WAP
  newDF[1,8]   = as.integer(msg[10]) ;   # Count
  
  # DEBUG    gDF        <<- rbind(gDF, newDF) ;
  # DEBUG    cat(msg,"\n") ;

  # Create a POSIXct object of the start of the 5 second bar.
  theTime = as.POSIXct(as.integer(msg[3]), tz="America/Los_Angeles", origin="1970-01-01") ;
  # Convert data.frame to an xts object
  newXTS = xts(newDF[,2:8], order.by=theTime, tzone="America/Los_Angeles")
  names(newXTS) = gColNames[2:8] ;  # name the columns properly
  
  # Add the newest 5 second real time bar to the global xts object.
  if ( is.null(gXTS) ) {
    gXTS <<- newXTS ;  # it's our first time through, so a simple assign is need.
  } else {
    gXTS <<- rbind(gXTS, newXTS) ;  # concatenate to the existing global xts object.
  }

  if( nrow(to.minutes(gXTS)) > 1 ){
    # Guard against too few rows in the xts object when passed to chartSeries() other
    # wise a call to periodicity(x)$scale to compute the time.scale will produce
    # this error: Error in periodicity(x) : can not calculate periodicity of 1
    # observation
      if( second(theTime) == 0 || second(theTime) == 30 ) {
      chartSeries(to.minutes(gXTS)) ;
    }
  }
}

############################################################
#                MAIN PROGRAM STARTS HERE                  #
############################################################

# Connect to TWS API and request 
tws                    = twsConnect(clientId=clientId, host=host, port=port) ;
contractES             = twsFUT(symbol=symbol, exch=exchange, expiry=expiry, include_expired=1 ) ;

ibWrapper              = eWrapper(debug=NULL) ;
ibWrapper$realtimeBars = ibRealTimeBars ;

reqRealTimeBars(conn=tws, Contract=contractES, eventWrapper=ibWrapper, useRTH=FALSE, verbose=TRUE) ; 

twsDisconnect(tws) ;

