## ============================================================================
## Chris Castañeda-Barajas (chrisscastaneda@gmail.com)
## 
## merge.R
##
## Merges an address data file with a polling place data file along a common
## polling location attribute.  Cleans and normalizes data files in the process.
## ============================================================================

## ============================ SETUP ENVIRONMENT =============================
rm(list=ls())
source("settings.R")  # See settings.example.R as guide for setting up
source("commonStreetAddressSuffixes.R")  # Huge character vector of all common 
                                         # street address suffixes per USPS. 
                                         # Also sets the constant: 
                                         # kCommonStreetSuffixesRegexPattern
kZipcodePattern  <- "[0-9]{5}([- ][0-9]{4})?"  # "Constant" Regex for zipcodes
setwd(paste(c(kSettingsPath, "merge-address-to-polling-location/"), collapse=""))
library(dplyr) 
library(logging)

## ============================= HELPER FUNCTIONS =============================
ConvertDataFrameToUpperCase <- function (df) {
  # ---------------------------------------------------------------------------
  # Converts all characters in a dataframe to upper case.
  #
  # Args:
  #   df: Dataframe to be converted.  Contents are arbitrary.  Only features 
  #       that are character vectors will be converted.  Non character vector 
  #       features will be left alone.
  # Returns:
  #   Dataframe identical to df except all characters are capitalized.
  # ---------------------------------------------------------------------------
  features <- names(df)
  as.data.frame(sapply(features, function (feature) {
    if ( is.character(df[,feature])  ) {
      return( toupper(df[,feature]) )
    } else {
      return( df[,feature] )
    }
  }), stringsAsFactors = FALSE)
}
VIPNormalizePollingListFile <- function (file) {
  # ---------------------------------------------------------------------------
  # Standardizes polling list file according to VIP 3.0 specifications.  
  # Attempts to repair common data cleanliness issues, like misaligned data.  
  # Normalizes precinct ID.
  #
  # Args:
  #   file: Dataframe representing the address file. Assumes has the following
  #         column headers in this order: Street, City, State.ZIP, Country, Precinct. 
  # Returns:
  #   Dataframe representing the VIP normalized address file with the following 
  #   column headers: address_location_name, address_line, address_line2, 
  #   address_line3, address_city, address_state, address_zip, precinct_id
  # ---------------------------------------------------------------------------
  
  ## Initializations
  expectedColumns <- c("Street", "City", "State.ZIP", "Country", "Precinct")
  
  # Initialize empty output df with appropriate column headers
  output <- data.frame(address_location_name = character(),
                       address_line = character(),
                       address_line2 = character(), 
                       address_line3 = character(), 
                       address_city = character(), 
                       address_state = character(), 
                       address_zip = character(), 
                       precinct_id = character(),
                       stringsAsFactors = FALSE)
  
  
  ## Internal helper functions
  CleanDirtyData <- function ( d ) {
    ## TODO: CLEAN THIS UP, THIS WORKS
    # -------------------------------------------------------------------------
    # 
    # Args:
    #   d: Dataframe containing the dirty data to be cleaned.  Dataframe is of 
    #      the same schema as file, containing only the rows in file where the 
    #      Precinct value is missing.
    #
    # -------------------------------------------------------------------------
    rows <- rownames(d)
    d$string <- sapply(1:nrow(d), function(row) {
      # Collapeses entire row into a string and removes  NA values and any double spaces 
      toupper(gsub("  ", " ", gsub(" NA", "", paste(d[row, ], collapse=" "))))
    }) 
    d$list <- sapply(d$string, function(str) { 
      # Splits d$string by " " characters and converts to a character 
      strsplit(str, split=" ")
    })
    d$zipCodeIndex <- sapply(1:nrow(d), function(row) {
      # Regex to find the string that looks like a zipcode
      grep(kZipcodePattern, d$list[[row]], value=F)
    })
    d$streetEndIndex <- sapply(1:nrow(d), function(row) {
      grep(kCommonStreetSuffixesRegexPattern, d$list[[row]], value=F)
    })
    
    d$Street <- sapply(1:nrow(d), function(row) {
      paste(d$list[[row]][1:tail(d$streetEndIndex[[row]], n=1)], collapse=" ")
    })
    d$City <- sapply(1:nrow(d), function(row) { 
      #  d$list[[row]][d[row,]$zipCodeIndex - 2]
      cityStart <- tail(d$streetEndIndex[[row]], n=1) + 1  # right after the end of the street address
      cityEnd   <- d[row,]$zipCodeIndex - 2  # right before the State.Zip
      paste( d$list[[row]][cityStart:cityEnd], collapse=" ")
    })
    
    d$State.ZIP <- sapply(1:nrow(d), function(row) { 
      paste(d$list[[row]][d[row,]$zipCodeIndex - 1], 
            d$list[[row]][d[row,]$zipCodeIndex])
    })
    
    d$Precinct <- sapply(1:nrow(d), function(row) { 
      tail(d$list[[row]], n=1)
    })
    d$Country <- sapply(1:nrow(d), function(row) { 
      tail(d$list[[row]], n=2)[1]
    })
    
    
    return(d[,expectedColumns])
  }
  
  ## Error handeling
  hasExpectedColumns <- all(names(file) == expectedColumns)
  if ( !hasExpectedColumns ) {
    errorStr <- paste(
      "[ERROR] Polling list file does not have the expected column headers.", 
      "\n\tWas expecting: Street, City, State.ZIP, Country, Precinct. ",
      "\n\tFile has: ", paste(names(file), collapses=" "))
    stop(errorStr)
  }
  
  ## Convert everything to uppercase to simplify
  file <- ConvertDataFrameToUpperCase(file)
  
  ## Check for holes in data and try to repaire
  missingPrecinctIndex <- !complete.cases(file$Precinct)
  hasMissingData <- sum(missingPrecinctIndex) > 0
  if ( hasMissingData ) {
    file <- CleanDirtyData( file[missingPrecinctIndex,] )
    ### START HERE ###
  }
  
  
  
  
  return(file)
}

## =================================== MAIN ===================================
if (interactive()){

  ## ----------------------------- SETUP LOGGING ------------------------------

  basicConfig()
  logReset()
  addHandler(writeToFile, logger="data_logger", file="logs/merge.log")
  loginfo(paste(":\n",
                " Merging Address and Polling Location Files\n",
                "---------------------------------------------"),
          logger="data_logger")


  ## -------------------------------- LOAD DATA --------------------------------

  addresses <- read.csv(
    'data/VIP Data Associate - File 1_Addresses.csv', 
    stringsAsFactors=FALSE, na.strings="")

  polling_places <- read.csv(
    'data/VIP Data Associate - File 2_Precinct Polling List.csv',
     stringsAsFactors=FALSE, na.strings="")


  ## ----------------------------- NORMALIZE DATA ------------------------------
  polling_places <- VIPNormalizePollingListFile(polling_places)
}
