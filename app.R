library(shiny)
source("merge.R")

server <- function(input, output) {
  output$contents <- renderTable({

    addressFile <- input$addressFile
    pollingFile <- input$pollingFile

    if ( is.null(addressFile) || is.null(pollingFile) ) 
      return(NULL)

    ## -------------------------------- LOAD DATA -------------------------------
    addresses <- read.csv(addressFile$datapath, stringsAsFactors=FALSE, na.strings="")
    polling_places <- read.csv(pollingFile$datapath, stringsAsFactors=FALSE, na.strings="")

    ## ----------------------------- NORMALIZE DATA -----------------------------
    addresses <- VIPNormalizeAddressFile(addresses)
    polling_places <- VIPNormalizePollingListFile(polling_places)

    ## ------------------------------- MERGE DATA -------------------------------
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

    read.csv("VIP_Data_Associate_Merged_Address_Polling.csv", stringsAsFactors=FALSE, na.strings="")
    
    # read.csv(addressFile$datapath, header=input$header, sep=input$sep, 
    #      quote=input$quote)
  })
}

ui <- fluidPage(
  titlePanel("Merge Address and Polling Location Files"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Upload your files here"),
      tags$p(HTML("Make sure your address file is a <b>.csv</b> and contains the following column headers in the following order: <br><i>Street, Apt, City, State, Zip, Precinct ID</i>")),
      fileInput('addressFile', 'Upload Address File:',
                accept=c('text/csv', 
                 'text/comma-separated-values,text/plain', 
                 '.csv')),

      tags$p(HTML("Make sure your polling location file is a <b>.csv</b> and contains the following column headers in the following order: <br><i>Street, City, State/ZIP, Country, Precinct</i>")),
      fileInput('pollingFile', 'Upload Polling Location File:',
                accept=c('text/csv', 
                 'text/comma-separated-values,text/plain', 
                 '.csv')),

      tags$hr()
    ),
    mainPanel(
      tableOutput('contents')
    )
  )
)

shinyApp(ui = ui, server = server)