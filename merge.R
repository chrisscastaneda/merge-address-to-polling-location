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

NormalizePrecinctId <- function (precincts, states) {
  # ---------------------------------------------------------------------------
  # Normalizes Precinct IDs in the format WA-000, i.e. two character state
  # abbreviation immediately followed by a dash then the three digit number.
  # 
  # Args:
  #   precincts: character vector of precinct IDs to be normalized.
  #   states: character vector of states, corresponds to precincts.
  #   (precincts and states should both be the same length)
  # Returns:
  #   Character vector of the same length as precincts in standardized format.
  # ---------------------------------------------------------------------------

  ## Error handling
  notSameSize <- length(precincts) != length(states)
  if ( notSameSize ) {
    errorStr <- paste(
      "[ERROR] NormalizePrecinctId():", 
      "\n\tprecincts and states vectors not the same length ",
      "\n\tlength(precincts): ", length(precincts),
      "\n\tlength(states): ", length(states))
    loginfo(errorStr, logger="merge_logger")
    stop(errorStr)
  } 

  ## 
  df <- data.frame(s=states, p=precincts, stringsAsFactors = FALSE)

  df$pid <- sapply(1:nrow(df), function(row) { 
    suffix <- strsplit( gsub("--", "-", df[row,2]), "-")[[1]][2]
    if (nchar(suffix) == 2 ) {
      suffix = paste("0", suffix, sep="", collapse = "")
    } else if (nchar(suffix) == 1 ) {
      suffix = paste("00", suffix, sep="", collapse = "")
    }
    paste(df[row,1], suffix , sep="-")
  })
  return(df$pid)
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
VIPNormalizeAddressFile <- function (file) {
  # ---------------------------------------------------------------------------
  # Standardizes address file according to VIP 3.0 specifications.  
  # Attempts to repair common data cleanliness issues, like misaligned data.  
  # Normalizes precinct ID.
  #
  # Args:
  #   file: Dataframe representing the address file. Assumes has the following
  #         column headers in this order: Street, Apt, City, State, Zip, Precinct.ID
  # Returns:
  #   Dataframe representing the VIP normalized address file with the following 
  #   column headers: address_location_name, address_line, address_line2, 
  #   address_line3, address_city, address_state, address_zip, precinct_id
  # ---------------------------------------------------------------------------

  ## Initializations
  expectedColumns <- c("Street", "Apt", "City", "State", "Zip", "Precinct.ID")
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
    
    # ## Cache just the broken/dirty rows into a dataframe
    # d <- file[brokenRows, ]

    # ## Collapses entire row into a string, removes NA values, and double spaces.
    # d$string <- sapply(1:nrow(d), function(row) {
    #   toupper(gsub("  ", " ", gsub(" NA", "", paste(d[row, ], collapse=" "))))
    # }) 

    # ## Splits d$string by " " (space) characters and converts to character vector
    # d$list <- sapply(d$string, function(str) { 
    #   strsplit(str, split=" ")
    # })

    # ## Regex to find the string that looks like a zipcode and cache index value
    # d$zipCodeIndex <- sapply(1:nrow(d), function(row) {
    #   grep(kZipcodePattern, d$list[[row]], value=FALSE)
    # })

    # ## Regex to find the string that looks like a suffix of a street address
    # ## (i.e. Street, Ave., Rd., Lane, etc...) the cache the index value
    # d$streetEndIndex <- sapply(1:nrow(d), function(row) {
    #   grep(kCommonStreetSuffixesRegexPattern, d$list[[row]], value=FALSE)
    # })

    # ## Identify string segments that compromise street address
    # d$Street <- sapply(1:nrow(d), function(row) {
    #   streetPosition <- 1:tail(d$streetEndIndex[[row]], n=1)
    #   paste(d$list[[row]][streetPosition], collapse=" ")
    # })

    # ## Identify city in string based relative location of street address and zip
    # ## (Will identify city names that are longer than one word)
    # d$City <- sapply(1:nrow(d), function(row) { 
    #   cityStart <- tail(d$streetEndIndex[[row]], n=1) + 1  # right after street address
    #   cityEnd   <- d[row,]$zipCodeIndex - 2  # right before the State.Zip
    #   paste( d$list[[row]][cityStart:cityEnd], collapse=" ")
    # })

    # ## Concatenate state and zip per schema of file
    # d$State.ZIP <- sapply(1:nrow(d), function(row) { 
    #   paste(d$list[[row]][d[row,]$zipCodeIndex - 1],  # state is right before zip
    #         d$list[[row]][d[row,]$zipCodeIndex])  # zip
    # })
    
    # ## Assumes precinct is the last string segment 
    # d$Precinct <- sapply(1:nrow(d), function(row) { 
    #   tail(d$list[[row]], n=1)
    # })

    # ## Assumes country is penultimate string segment
    # d$Country <- sapply(1:nrow(d), function(row) { 
    #   tail(d$list[[row]], n=2)[1]
    # })
    
    # ## Replace broken rows in file with repaired rows in d
    output <- file
    # sapply(rownames(d), function (row) {
    #   output[row,] <<- d[row,expectedColumns]
    # })
    return(output)
  }
  ## END CLOSURES -------------------------------------------------------------
  


  ## Error handling
  hasExpectedColumns <- all(names(file) == expectedColumns)
  if ( !hasExpectedColumns ) {
    errorStr <- paste(
      "[ERROR] VIPNormalizeAddressFile():",
      "\n\tPolling list file does not have the expected column headers.", 
      "\n\tWas expecting: Street, Apt, City, State, Zip, Precinct.ID",
      "\n\tFile has: ", paste(names(file), collapses=" "))
    loginfo(errorStr, logger="merge_logger")
    stop(errorStr)
  }

  ## Convert everything to uppercase to simplify
  file <- ConvertDataFrameToUpperCase(file)  

  ## Check for holes in data and try to repair
  missingPrecinctIndex <- !complete.cases(file$Precinct.ID)
  hasHolesInData <- sum(missingPrecinctIndex) > 0
  if ( hasHolesInData ) {
    file <- RepairDataFile( missingPrecinctIndex )
  }
  # expectedColumns <- c("Street", "Apt", "City", "State", "Zip", "Precinct.ID")
  ## Prepare output per VIP 3.0 address schema
  output <- data.frame(
    address_location_name = character(rows),
    address_line          = file$Street,
    address_line2         = file$Apt, 
    address_line3         = character(rows), 
    address_city          = file$City, 
    address_state         = file$State, 
    address_zip           = file$Zip,
    # precinct_id           = file$Precinct.ID,
    precinct_id           = NormalizePrecinctId(file$Precinct.ID, file$State ),
    stringsAsFactors = FALSE)
  
  return(output)
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
      "[ERROR] VIPNormalizePollingListFile():",
      "\n\tPolling list file does not have the expected column headers.", 
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
  
  ## Prepare output per VIP 3.0 address schema
  state <- sapply(file$State.ZIP, function (statezip) { 
            strsplit(statezip, split=" ")[[1]][1] })
  zip <- sapply(file$State.ZIP, function (statezip) { 
          strsplit(statezip, split=" ")[[1]][2] })
  output <- data.frame(
    address_location_name = character(rows),
    address_line          = file$Street,
    address_line2         = character(rows), 
    address_line3         = character(rows), 
    address_city          = file$City, 
    address_state         = state, 
    address_zip           = zip,
    precinct_id           = NormalizePrecinctId(file$Precinct, state),
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
  addresses <- VIPNormalizeAddressFile(addresses)
  polling_places <- VIPNormalizePollingListFile(polling_places)

  ## ------------------------------- MERGE DATA --------------------------------
  merged_data <- inner_join(x=addresses, y=polling_places, by="precinct_id")
  headers <- c("address_location_name", "address_line", "address_line2", 
               "address_line3", "address_city", "address_state", "address_zip", 
               "precinct_id", "polling_address_location_name", 
               "polling_address_line", "polling_address_line2", 
               "polling_address_line3", "polling_address_city", 
               "polling_address_state", "polling_address_zip" )
  names(merged_data) <- headers  # update column headers

  write.csv(merged_data, "VIP_Data_Associate_Merged_Address_Polling.csv", 
            row.names = FALSE)

}

