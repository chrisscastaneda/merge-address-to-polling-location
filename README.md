# Exercise in Programmatically Matching Address to Polling Places

Given a spreadsheet of addresses (`VIP Data Associate - File 1_Addresses.csv`) and a spreadsheet of associated polling paces (`VIP Data Associate - File 2_Precinct Polling List.csv`), this is how one could merge two files along a common attribute, in this case along the Precinct ID values in each file.  While it's certainly possible to manually clean and merge two spreadsheets in Excel, in order to accomplish the task efficiently at scale it is important to accomplish the task programmatically.  

You can examine my solution to this problem here: `merge.R`.  I also built a simple Shiny webapp that will merge the two data files for you: [https://chrisscastaneda.shinyapps.io/merge-address-to-polling-location/](https://chrisscastaneda.shinyapps.io/merge-address-to-polling-location/).  

## Step-by-step Overview:

The first step is to check for any common data entry errors, correct for messy data, and otherwise deal with any holes that may be in the data.  One of the most common types of "messy" data is data that is misaligned, that is the data in one row may not be lined up properly with the overall schema of the spreadsheet.