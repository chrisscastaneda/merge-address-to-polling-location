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

NormalizePrecinctId <- function (precincts) {
  # ---------------------------------------------------------------------------
  # 
  # 
  # Args:
  #   precincts: character vector of precinct IDs to be normalized.
  # Returns:
  #   Character vector of the same length as precincts in standardized format.
  # ---------------------------------------------------------------------------
}

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
  rows <- nrow(file)
  
  ## Closures ------------------------------------------------------------------
  RepairDataFile <- function ( brokenRows ) {
    # -------------------------------------------------------------------------
    # Closure function to repair and clean dirty records in file. Assumes   
    # address info exists in the data, and data messiness is due to messy
    # record's columns being misaligned with file's schema.  
    #
    # Algorithm:
    # 1. cache just the broken rows in a dataframe called d
    # 2. concatenate all values in each broken row into a string
    # 3. parse strings w/ regex to identify atomic boundaries in address data
    # 
    #
    # Args:
    #   brokenRows: logical vector representing row indices's of holes in file
    # Returns:
    #   A copy of file with the broken rows fixed.
    # -------------------------------------------------------------------------
    
    ## Cache just the broken/dirty rows into a dataframe
    d <- file[brokenRows, ]

    ## Collapses entire row into a string, removes NA values, and double spaces.
    d$string <- sapply(1:nrow(d), function(row) {
      toupper(gsub("  ", " ", gsub(" NA", "", paste(d[row, ], collapse=" "))))
    }) 

    ## Splits d$string by " " (space) characters and converts to character vector
    d$list <- sapply(d$string, function(str) { 
      strsplit(str, split=" ")
    })

    ## Regex to find the string that looks like a zipcode and cache index value
    d$zipCodeIndex <- sapply(1:nrow(d), function(row) {
      grep(kZipcodePattern, d$list[[row]], value=FALSE)
    })

    ## Regex to find the string that looks like a suffix of a street address
    ## (i.e. Street, Ave., Rd., Lane, etc...) the cache the index value
    d$streetEndIndex <- sapply(1:nrow(d), function(row) {
      grep(kCommonStreetSuffixesRegexPattern, d$list[[row]], value=FALSE)
    })

    ## Identify string segments that compromise street address
    d$Street <- sapply(1:nrow(d), function(row) {
      streetPosition <- 1:tail(d$streetEndIndex[[row]], n=1)
      paste(d$list[[row]][streetPosition], collapse=" ")
    })

    ## Identify city in string based relative location of street address and zip
    ## (Will identify city names that are longer than one word)
    d$City <- sapply(1:nrow(d), function(row) { 
      cityStart <- tail(d$streetEndIndex[[row]], n=1) + 1  # right after street address
      cityEnd   <- d[row,]$zipCodeIndex - 2  # right before the State.Zip
      paste( d$list[[row]][cityStart:cityEnd], collapse=" ")
    })

    ## Concatenate state and zip per schema of file
    d$State.ZIP <- sapply(1:nrow(d), function(row) { 
      paste(d$list[[row]][d[row,]$zipCodeIndex - 1],  # state is right before zip
            d$list[[row]][d[row,]$zipCodeIndex])  # zip
    })
    
    ## Assumes precinct is the last string segment 
    d$Precinct <- sapply(1:nrow(d), function(row) { 
      tail(d$list[[row]], n=1)
    })

    ## Assumes country is penultimate string segment
    d$Country <- sapply(1:nrow(d), function(row) { 
      tail(d$list[[row]], n=2)[1]
    })
    
    ## Replace broken rows in file with repaired rows in d
    output <- file
    sapply(rownames(d), function (row) {
      output[row,] <<- d[row,expectedColumns]
    })
    return(output)
  }
  ## END CLOSURES -------------------------------------------------------------
  
  ## Error handling
  hasExpectedColumns <- all(names(file) == expectedColumns)
  if ( !hasExpectedColumns ) {
    errorStr <- paste(
      "[ERROR] Polling list file does not have the expected column headers.", 
      "\n\tWas expecting: Street, City, State.ZIP, Country, Precinct. ",
      "\n\tFile has: ", paste(names(file), collapses=" "))
    loginfo(errorStr, logger="merge_logger")
    stop(errorStr)
  }
  
  ## Convert everything to uppercase to simplify
  file <- ConvertDataFrameToUpperCase(file)
  
  ## Check for holes in data and try to repair
  missingPrecinctIndex <- !complete.cases(file$Precinct)
  hasHolesInData <- sum(missingPrecinctIndex) > 0
  if ( hasHolesInData ) {
    file <- RepairDataFile( missingPrecinctIndex )
  }
  
  ## START HERE ## 
  ## TODO: normalize precinct
  
  ## Prepare output per VIP 3.0 address schema
  output <- data.frame(
    address_location_name = character(rows),
    address_line          = file$Street,
    address_line2         = character(rows), 
    address_line3         = character(rows), 
    address_city          = file$City, 
    address_state         = sapply(file$State.ZIP, function (statezip) { 
                              strsplit(statezip, split=" ")[[1]][1] }), 
    address_zip           = sapply(file$State.ZIP, function (statezip) { 
                              strsplit(statezip, split=" ")[[1]][2] }),
    precinct_id           = file$Precinct,
    # precinct_id         = NormalizePrecinctId(data.frame( 
    #                         precinct = file$Precinct, 
    #                         state = file$address_state )),
    stringsAsFactors = FALSE)
  
  return(output)
}

## =================================== MAIN ===================================
if (interactive()){

  ## ----------------------------- SETUP LOGGING ------------------------------

  basicConfig()
  logReset()
  addHandler(writeToFile, logger="merge_logger", file="logs/merge.log")
  loginfo(paste(":\n",
                " Merging Address and Polling Location Files\n",
                "---------------------------------------------"),
          logger="merge_logger")


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

