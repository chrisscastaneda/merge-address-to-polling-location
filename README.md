# Exercise in Programmatically Matching Address to Polling Places

Given a spreadsheet of addresses ([`VIP Data Associate - File 1_Addresses.csv`](https://github.com/chrisscastaneda/merge-address-to-polling-location/blob/master/data/VIP%20Data%20Associate%20-%20File%201_Addresses.csv)) and a spreadsheet of associated polling paces ([`VIP Data Associate - File 2_Precinct Polling List.csv`](https://github.com/chrisscastaneda/merge-address-to-polling-location/blob/master/data/VIP%20Data%20Associate%20-%20File%202_Precinct%20Polling%20List.csv)), this is how one could merge two files along a common attribute, in this case along the Precinct ID values in each file.  While it's certainly possible to manually clean and merge two spreadsheets in Excel, in order to accomplish the task efficiently at scale it is important to accomplish the task programmatically.  

You can examine my solution to this problem here: [`merge.R`](https://github.com/chrisscastaneda/merge-address-to-polling-location/blob/master/merge.R).  I also built a simple Shiny webapp that will merge the two data files for you: [https://chrisscastaneda.shinyapps.io/merge-address-to-polling-location/](https://chrisscastaneda.shinyapps.io/merge-address-to-polling-location/).  

Here are the requested files:

  - [`VIP_Data_Associate_Merged_Address_Polling.csv`](https://github.com/chrisscastaneda/merge-address-to-polling-location/blob/master/VIP_Data_Associate_Merged_Address_Polling.csv)
  - [`precinct.txt`](https://github.com/chrisscastaneda/merge-address-to-polling-location/blob/master/precinct.txt)
  - [`polling_locations.txt`](https://github.com/chrisscastaneda/merge-address-to-polling-location/blob/master/polling_locations.txt)
  - [`precinct_polling_locations.txt`](https://github.com/chrisscastaneda/merge-address-to-polling-location/blob/master/precinct_polling_locations.txt)


## Step-by-step Overview:

The first step is to check for any common data entry errors, correct for messy data, and otherwise deal with any holes that may be in the data.  One of the most common types of "messy" data is data that is misaligned, that is the data in one row may not be lined up properly with the overall schema of the spreadsheet.  

A good rule of thumb for identifying the rows in the data that may be misaligned is looking for missing values in the last/right-most column in the data.  In our case, this is the Precinct ID column in each of our spreadsheets.  This has the added benefit of being a mandatory column for each of our spreadsheets and the column we're ultimately going to merge them by, so definitely want to make sure there are no holes here.

Once we've identified the rows that are potentially misaligned, we now must figure out how to realign them to the proper schema.  The approach I took was to concatenate each row into a string, then parse that string using regex pattern matching to identify key parts in the address.  Using that information we can properly atomize the addresses in each string into its component parts.  First I located the zipcode in each string using the following regex pattern: `[0-9]{5}([- ][0-9]{4})?`.  Once we know where the zipcode is in the address string, we can inch over and identify the state substring next to the zipcode.  Next I located the end of the street address substring by looking for street address suffixes (i.e. Ave., Road, Ln., St. Blvd, etc).  The regex for street address suffixes looks a little like this: `\b(?:AVE|BLVD|LN|RD|ROAD|etc|etc)\b`.  In actuality I refereed to USPS's website and found a list of over 900 common street address suffixes, you can see how I actually constructed that regex pattern in the file [`commonStreetAddressSuffixes.R`](https://github.com/chrisscastaneda/merge-address-to-polling-location/blob/master/commonStreetAddressSuffixes.R).  From there, you have enough information to fully parse the address.  

The next major step is to normalize the Precinct ID values in each data file.  To do this I took the existing precinct ID values and split each of those strings by the dash in each string to separate the ID prefixes from the ID suffixes.  I replaced the prefixes with the standard two letter state abbreviation and ensured each suffix was three digits long (I made sure that leading zeros were added to any suffix values that were less than thee digits).

Once the Precinct IDs in each spreadsheet are standardized, it's then relatively straight forward to merge or join the two tables together along the common attribute (specifically, in this case, we want to do a *left inner join* on the data tables).












