library(shiny)

fluidPage(
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

      # checkboxInput('header', 'Header', TRUE),
      # radioButtons('sep', 'Separator',
      #              c(Comma=',',
      #                Semicolon=';',
      #                Tab='\t'),
      #              ','),
      # radioButtons('quote', 'Quote',
      #              c(None='',
      #                'Double Quote'='"',
      #                'Single Quote'="'"),
      #              '"')
    ),
    mainPanel(
      tableOutput('contents')
    )
  )
)