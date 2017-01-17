#vim:ts=2:sts=2:sw=2:ht=2:expandtab:number:ruler:showmode:smartindent:ff=unix
#
#  downloadDailyBarsIB.R - A program to download daily stock prices from Interactive Brokers via their TWS API
#  Author: Stergios Marinopoulos
#  Date Created: 2/1/2011

# source("downloadDailyBarsIB.R");

## Clear the work space!
rm(list = ls(all = TRUE))

library(quantmod) ;  # xts is probably enough, but I use quantmod all the time.
require(IBrokers) ;

############################################################
#                 GLOBAL VARIABLES
############################################################

clientId    = 8 ;           # TWS API Client connection number
host        = 'localhost' ; # TWS is running on localhost
port        = 7496 ;        # TWS is listening on the defualt port 
numDaysBack = 365 ;         # Number of days of daily data to fetch.  Can be shortened after the intial fetch.
  
# Column names of the historical daily data. Any data returned via
# reqHistoricalData() will have the ticker preprened to the column name. For
# example SPY.Open, SPy.Volume, etc.  Use the column names below as
# replacements.
colNames = c("Date", "Open", "High", "Low", "Close", "Volume", "WAP", "hasGaps", "Count") ;

# A mapping of common symbol names to that required by IB
stockNameMap = list(BRK.B="BRK B") ;

# Dow Jones Industrial Average stock list (as March 19, 2015)
indu = "
AAPL   AXP   BA            CAT   CSCO:NASDAQ   CVX   DD    DIS   GE    GS
HD     IBM   INTC:NASDAQ   JNJ   JPM           KO    MCD   MMM   MRK   MSFT:NASDAQ
NKE    PFE   PG            TRV   UNH           UTX   V     VZ    WMT   XOM
" ;

# SP100 Stock List (as of November 10, 2016)
sp100 = "
AAPL         ABBV         ABT   ACN   AGN   AIG          ALL    AMGN  AMZN  AXP
BA           BAC          BIIB  BK    BLK   BMY          BRK.B  C     CAT   CELG
CL           CMCSA        COF   COP   COST  CSCO:NASDAQ  CVS    CVX   DD    DHR
DIS          DOW          DUK   EMR   EXC   F            FB     FDX   FOX   FOXA
GD           GE           GILD  GM    GOOG  GOOGL        GS     HAL   HD    HON
IBM          INTC:NASDAQ  JNJ   JPM   KHC   KMI          KO     LLY   LMT   LOW
MA           MCD          MDLZ  MDT   MET   MMM          MO     MON   MRK   MS
MSFT:NASDAQ  NEE          NKE   ORCL  OXY   PCLN         PEP    PFE   PG    PM
PYPL         QCOM         RTN   SBUX  SLB   SO           SPG    T     TGT   TWX
TXN          UNH          UNP   UPS   USB   UTX          V      VZ    WBA   WFC
WMT          XOM
" ;

# Popular ETFs
etfs = "
AAXJ ACWI AGG  AMJ  AMLP BIV  BND  BSV  CIU  CSJ  DBC  DEM  DJP  DVY  EEM 
EFA  EMB  EPP  EWA  EWC  EWG  EWJ  EWT  EWY  EWZ  FXI  GDX  GLD  HYG  IAU 
ICF  IEF  IEI  IJH  IJJ  IJK  IJR  IJS  IVV  IVW  IWB  IWD  IWF  IWN  IWO 
IWP  IWR  IWS  IWV  IYR  JNK  LQD  MBB  MDY  MOO  MUB  OEF  PFF  RSP  RSX 
RWX  SDY  SHV  SHY  SLV  TBT  TIP  TLT  VB   VBK  VCSH VEA  VEU  VGK  VGT 
VIG  VNQ  VO   VOO  VTI  VTV  VUG  VV   VWO  VYM  XLB  XLE  XLF  XLI  XLK 
XLP  XLU  XLV  XLY 
" ;

# The list of stocks for which historical data will be retreived during the next execution of this program.
  stockList = indu ;
# stockList = sp100 ;
# stockList = etfs ;
# stockList = "AA    AAPL  ABT   AEP ZION" ;  # short list useful for debugging
# stockList = "CSCO:NASDAQ INTC:NASDAQ MSFT:NASDAQ" ; # short list useful for debugging
  
  
# Convert stockList from a string into a vector of strings
stockList = unique(unlist(strsplit(stockList, split="\\s+", perl=TRUE))) ;
stockList = stockList[nzchar(stockList)] ; 

############################################################
# DEBUG:  Use this next line if you have to restart the download in the middle
# of a run due to a bad stock symbol:
# stockList = stockList[stockList > "NVLS"] ;
############################################################

pauseBetween = 11 ; # IB rate limits historical data reqiests to no more than 6 in 60 seconds.

############################################################
# The file system directory where we store our daily stock data.  Each stock has
# its own file.
#
stockDBDir = "~/CSV";

############################################################
#                   FUNCTIONS
############################################################


############################################################
# Check for the existence of the output directory.
#
checkDBDir = function(toDir) {
  if ( ! file.exists(toDir) ) {
    stop("Storage directory '", toDir, "' for CSV data does not exist\n") ;  
  }
}

############################################################
# Load the existing ticker data, and return it as a xts object.
# If the file does not exist or is empty return NULL.
#
loadStockFile <- function(ticker, fileName) {
  ## DEBUG   cat(paste("Loading file data: ", ticker , "\n", sep="") ) ;

  if ( ! file.exists( fileName ) ) {
    ## DEBUG cat(paste("\tNew Ticker Symbol=", ticker , "\n", sep="") ) ;
    return( NULL ) ;
  }

  # stockData = read.csv( fileName, header=TRUE, col.names = colNames, stringsAsFactors=FALSE ) ;
  stockData = read.zoo( fileName, sep = ",", header=TRUE, stringsAsFactors=FALSE ) ;
  
  if ( nrow(stockData) ) {
    ## DEBUG cat(paste("\tThe file (", fileName, ") exists, but is empty.  No data for ", ticker, "\n", sep="") ) ;
    return( NULL ) ;
  }

  return( as.xts(stockData) ) ;
}

############################################################
# Merge the old data and the new data into a new xts object, and then save that
# new object to a CSV file.
#
mergeAndSave <- function(oldData, newData, fileName, oldFileName) {
  
  # Make sure the output directory exists.
  checkDBDir(stockDBDir) ;
  
  index(newData) = as.Date(index(newData))   # strip away PST and PDT from the index.
  
  combinedData = newData ;
  
  # Merge old and new data.  Merge the two xts objects while removing any 
  # duplicates found in newData - the preference is to keep the previously
  # existing data.
  if ( is.null(oldData) == FALSE && nrow(oldData > 0 ) ) {
      
    combinedData = rbind(oldData, newData[ ! index(newData) %in% index(oldData) ] ) ;
    # there's also make.index.unique(..., drop=TRUE, fromLast=FALSE) in xts

    file.rename(fileName, oldFileName) ;
  }
  write.zoo(combinedData, file=fileName, index.name="Date", quote=FALSE, sep="," ) ;
  return(combinedData) ;
}

############################################################
# Main Program Starts Here
############################################################

timeStartDownload = Sys.time() ;
stockCounter      = 1 ;
numStocks         = length(stockList) ;

tws = twsConnect(clientId=8, host='localhost', port=7496) ;

# Format the date to be passed into the reqHistoricalData() according to IB
# rules.  Set this outside of the loop below because it should be the same
# for all stocks in the list, and if this script runs at night through
# midnight, the last date retrieved will change.

asOfDate    = as.Date("2017-01-16") ;
endDateTime = format( asOfDate, "%Y%m%d %H:%M:%S" ) ;  # a time format required by reqHistoricalData
barSize     = "1 day"
duration    = paste(numDaysBack, " D", sep="") ;

while(stockCounter <= numStocks ) {
  cat("==========================================================================================\n") ;
  ticker      = stockList[stockCounter] ;
  # Check if the ticker specifies a primary exchange via a ":" (e.g.
  # CSCO:NASDAQ" or "INTC:NASDAQ"), and if it does split it apart and store in
  # seperate scalars.
  ticker = unique(unlist(strsplit(ticker, split=":", perl=TRUE))) ;
  ticker2IB = ticker[1] ;
  if (length(ticker)==2) {
    primary = ticker[2] ;
    ticker = ticker[1] ;
  } else {
    primary = ""
  }
  # See if ticker needs an IB specific ticker symbol
  if ( ticker %in% names(stockNameMap) ) {
    ticker2IB = stockNameMap[[ticker]] ;
  }
  timeStart   = Sys.time() ;

  # Set up file names, and get existing data.
  fileName    = paste(stockDBDir, "/", ticker, ".csv", sep="") ;
  oldFile     = paste(stockDBDir, "/", ticker, ".old", sep="") ; # We'll save the old data file before writing over it.
  exData      = loadStockFile(ticker, fileName) ; # existing data

  contract    = twsEquity( symbol=ticker2IB, exch="SMART", primary=primary) # equity specification
  newData = 
    tryCatch(
      # Returns data in an xts object in your local timezone.
      reqHistoricalData(
        conn= tws,
        Contract    = contract,
        endDateTime = endDateTime,
        barSize     = barSize,
        duration    = duration,
        useRTH      = 1,
        whatToShow  = "TRADES"
      ) , error=function(e){e}
    ) ;

  if ( is.null(newData) == FALSE) {
    names(newData) = colNames[2:9] ; # Remove the ticker prefix from each column name that 
                                      # xts places there. e.g. "ESH7.Volume" to "Volume", 
                                      # or "ESH7.Close" to "Close"

    newData[, "Volume"] = newData[, "Volume"] * 100 ;  # Multiply volume by 100 to get real volume. IB API returns volume in the 100's.
    
    mergeAndSave(exData, newData, fileName, oldFile) ;
  } else {
   cat("Failed to retireve data for ", ticker2IB, ".", "\n") ;
  }
  
  # Compute an estimate of the time left for fetching all stock data, and print a summary to the console.
  timeEnd      = Sys.time() ;
  numSecs      = as.numeric(difftime(timeEnd, timeStart, units="secs")) ;
  timeSoFar    = as.numeric(difftime(timeEnd, timeStartDownload, units="mins")) ;
  timeEstimate = timeSoFar / stockCounter * numStocks ;
  timeLeftEst  = timeEstimate - timeSoFar ;
  timeETA      = timeStartDownload + timeEstimate*60 ;
  waitSecs     = ceiling(pauseBetween - numSecs) ; # Round up!
  msg          = paste("Fetch Time for ", ticker, ": ", sprintf("%0.2f", numSecs), " secs", sep="") ;
  cat(msg, "\n") ;
  cat(paste("\t", sprintf("%0.2f", timeSoFar), "mins so far.  Est Total Time:", sprintf("%0.2f", timeEstimate), "mins.  Remaining:", sprintf("%0.2f", timeLeftEst), "mins.  ETA:", format( timeETA, "%Y%m%d %H:%M:%S" ),"\n")) ;

  # IB's TWS API rate limits calls to reqHistoricalData() to no more than 6 in
  # 60 seconds.  Pause the necessary amout of time to not exceed the rate limit.
  if ( waitSecs > 0 ) {
    cat("\t...  Sleeping ", waitSecs, "\n") ;
    Sys.sleep(as.integer(waitSecs)) ;
  } else {
    cat("\t... Skip sleeping, as ths stock took too long: ", numSecs, "\n" ) ;
  }

  flush.console() ;
  stockCounter = stockCounter + 1 ;
}

twsDisconnect(tws) 

timeEndDownload = Sys.time() ;
msg             = paste("Total Download Time : ", as.numeric(difftime(timeEndDownload, timeStartDownload, units="mins")), " mins") ;
cat(msg, "\n") ;