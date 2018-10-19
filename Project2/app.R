library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinydashboard)
library(readr)
library(grid)
library(scales)
library(leaflet)
library(leaflet.extras)
library(readxl)

sale.upload <- read.csv ("sales_2.csv")
zipcodes <- readOGR("County_Zip_Code.geojson")


ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub("NaN|''", 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}


# Unique values for Resource Field
ckanUniques <- function(field, id) {
  url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

category <- sort(ckanUniques("4af05575-052d-40ff-9311-d578319e810a", "SaleType")$SaleType)
taxes <- sort(ckanUniques("4af05575-052d-40ff-9311-d578319e810a", "CostsTaxes")$CostsTaxes)
ready <- sort(ckanUniques("4af05575-052d-40ff-9311-d578319e810a", "ReadyForSale")$ReadyForSale)
  
  
#pdf(NULL)


header <- dashboardHeader(title = "Pittsburgh Properties",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Mayor Bill Peduto",
                                         message = HTML("We need to increase the tax base!"),
                                         icon = icon("exclamation-circle"))
                                       ))

sidebar <- dashboardSidebar(
  # bars on the side
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("View Map", icon = icon("map"), tabName = "map"),
    menuItem("Download Data", icon = icon("download"), tabName = "table"),

    # Category Select
    selectInput("categorySelect",
                "Types of Sheriff Sale's:",
                choices = category,
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Mortgage Foreclosure", "Municipal Lien", "Other Real Estate")),

   # Date Select
    dateRangeInput("dateSelect",
                   "Sheriff Sale Auction Date:",
                   start = Sys.Date()-38, end = Sys.Date()-7,
                   min = "2001-01-01", max = Sys.Date()-7,
                   format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "en", separator = " to ", width = NULL),

   # Select Amount Owed
   sliderInput("taxesSelect",
                "Outstanding Taxes Owed:",
                min = min(taxes, na.rm = T),
                max = max(taxes, na.rm = T),
                value = c(min(taxes, na.rm = T), max(taxes, na.rm = T)),
                step = 5000),

    # Select Ready for Auction
   selectizeInput("readySelect",
                  "Is the Property Ready for Auction?",
                  choices = c("Yes", "No"),
                  multiple = FALSE,
                  selected = "Yes")
  ))

body <- dashboardBody(tabItems(
  # names of boxes
  tabItem("plot",
          fluidRow(
            infoBoxOutput("attorney"),
             infoBoxOutput("avgtaxes"),
             infoBoxOutput("zipcode")),
          
           # names of the plot tabs
           fluidRow(
             tabBox(title = "Plot", width = 12,
                    tabPanel("Types of Sheriff Sales", plotlyOutput("plot_types")),
                    tabPanel("Sum of taxes owed by Zip Code", plotlyOutput("plot_taxes"))))),
  # View Map
  tabItem("map",
          fluidRow(
            leafletOutput("map"),
            p())),
  
  # Download Data
  tabItem("table",
          inputPanel(
            downloadButton("downloadData","Download Sheriff Sale Data") # add button to download table as csv
          ),
          fluidPage(
            box(title = "Pittsburgh Sheriff Sales Properties", DT::dataTableOutput("table"), width = 12)))
 ))

 ui <- dashboardPage(header, sidebar, body)

 # Define server logic
 server <- function(input, output, session) {
   
   # Creating filtered sheriff sale data
   sheriffsales <- reactive({
       # Build API Query with proper encodes
       # If no categorySelect input selected
       if (length(input$categorySelect) == 0 ) {
         url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%224af05575-052d-40ff-9311-d578319e810a%22%20WHERE%20%22SaleDate%22%20%3E=%20%27", 
                       input$dateSelect[1], "%27%20AND%20%22SaleDate%22%20%3C=%20%27", input$dateSelect[2], 
                       "%27%20AND%20%22ReadyForSale%22%20=%20%27", input$readySelect, "%27", "%27%20AND%20%22CostsTaxes%22%20=%20%27",input$taxesSelect, "%27")
         
         url <- gsub(pattern = " ", replacement = "%20", x = url)
         
         # If one categorySelect input selected  
       } else if (length(input$categorySelect) == 1) {
         url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%224af05575-052d-40ff-9311-d578319e810a%22%20WHERE%20%22SaleDate%22%20%3E=%20%27", 
                       input$dateSelect[1], "%27%20AND%20%22SaleDate%22%20%3C=%20%27", input$dateSelect[2], 
                       "%27%20AND%20%22ReadyForSale%22%20=%20%27", input$readySelect, "%27", "%27%20AND%20%22CostsTaxes%22%20=%20%27",input$taxesSelect, "%27", 
                       "%27%20AND%20%22SaleType%22%20=%20%27", input$categorySelect, "%27")
         
         url <- gsub(pattern = " ", replacement = "%20", x = url)
         
         # Multiple categorySelect inputs selected
       } else {
         prim <- paste0(input$categorySelect, collapse = "%27%20OR%20%22SaleType%22%20=%20%27")
         url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%224af05575-052d-40ff-9311-d578319e810a%22%20WHERE%20%22SaleDate%22%20%3E=%20%27", 
                       input$dateSelect[1], "%27%20AND%20%22SaleDate%22%20%3C=%20%27", input$dateSelect[2], 
                       "%27%20AND%20%22ReadyForSale%22%20=%20%27", input$readySelect, "%27", "%27%20AND%20%22CostsTaxes%22%20=%20%27",input$taxesSelect, "%27",
                       "%27%20AND%20%22SaleType%22%20=%20%27", prim, "%27")
         url <- gsub(pattern = " ", replacement = "%20", x = url)}
       
       data <- ckanSQL(url)
       
       # Load and clean data
       if (is.null(data[1,1])){
         alert("There is no data available for your selected inputs. Please reset filters and select different inputs.")
         # Now, if you wanna be fancy! You could have put in an updateInputs or autmatically click your button with shinyjs()
       } else {
         sale.load <- sale.upload %>%
           mutate(
             City = case_when(
               City %in% c("PITTSBURGH", "PITSBURGH", "PITTBURGH", "PITTSBIURGH", "PITTSBRGH", "PITTSBSURGH") ~ "Pittsburgh"),
             SaleDate = as.POSIXct(SaleDate),
             ZIPCode = str_replace_all(ZIPCode, '"', ""),
             AttorneyName = str_replace_all(AttorneyName, '"', ""),
             ReadyForSale = case_when(
               ReadyForSale %in% c("yes", "yes.no", TRUE) ~ "Yes",
               ReadyForSale %in% c("no", "no.no", FALSE) ~ "No")
           )
         
         return(sale.load) 
       }
       
     })
   
  
   # propInput <- reactive({
   #   property <- sale.load  %>%
   # 
   #  # Slider Filter
   #  filter(CostsTaxes >= input$taxesSelect[1] & CostsTaxes <= input$taxesSelect[2])
   # 
   #   # Category Filter
   #  if (length(input$categorySelect) > 0 ) {
   #     property <- subset(property, SaleType %in% input$categorySelect)
   #   }
   #   # Property ready for sale filter?
   #  if (length(input$readySelect) > 0 ) {
   #     property <- subset(property, ReadyForSale %in% input$readySelect)
   #  }
   #  return(property)
   # })
   # #Reactive data
   # mInput <- reactive({
   #   property <- propInput()
   # })

   #map
   output$map <- renderLeaflet({
     # Plot map 
     leaflet() %>%
       
       # Add Basemaps
       addProviderTiles(providers$OpenMapSurfer.Grayscale, options = providerTileOptions(noWrap = TRUE)) %>%
       addTiles(options = providerTileOptions(noWrap = TRUE), group = "Default") %>%
       addProviderTiles("Esri.WorldTerrain", options = providerTileOptions(noWrap = TRUE), group = "Terrain") %>%
       
       # Set View
       setView(lat = 40.44, lng = -79.95, zoom = 11.8) %>%
       
       # Add Pittsburgh Zip Codes
       addPolygons(data = zipcodes, color = "#000000", label = ~ZIP, fillOpacity = 0.00) %>%
       
       # Add Layers control
       addLayersControl(
         baseGroups = c("Default", "Terrain"),
         options = layersControlOptions(collapsed = FALSE)
       ) %>%

       # Add Sheriff Sale Points
       addAwesomeMarkers(data = propInput(),
                         lat = ~latitude,
                         lng = ~longitude,
                         label = ~SaleType,
                         clusterOptions = markerClusterOptions())
   })
     
   # Plot 1-  Counts of Properties by Sale Types
   output$plot_types <- renderPlotly({
     property <- propInput()
     ggplot(data = property,
            aes(x = CostsTaxes, color = SaleType))  +
       geom_freqpoly(binwidth = 500) +
       guides(fill = FALSE) +
       scale_y_continuous(name = "Count of Properties") +
       scale_x_continuous(name = "Taxes Owed") +
       theme(axis.text.x = element_text(angle = 15,
                                        vjust = 1,
                                        hjust = 1))
   })
   
   # Plot 2- Plot showing taxes owed by zip code
   output$plot_taxes <- renderPlotly({
     property <- propInput ()
     ggplot (data = property,
            aes (x = ZIPCode,
                y = round (CostsTaxes, 0), na.rm = T )) +
       geom_col (position = position_dodge(width = 0.9)) +
       guides (fill = FALSE) +
       theme(axis.text.x = element_text(angle = 30,
                                        hjust = 1),
             axis.text = element_text(size = rel(0.5))) +
       scale_y_continuous (name = "Sum of Taxes Owed") +
       scale_x_discrete (name = "Zip Code")
   })

   # Data table of Assessment
   output$table <- DT::renderDataTable({
     subset(propInput(), select = c(DocketNumber, SaleType, AttorneyName, Plaintiff, Defendant, SaleDate, Address, CostsTaxes))
   })

   # Common Attorney infobox
   output$attorney <- renderInfoBox({
     proper <- propInput()
     name <- names(sort(table(sale.load$AttorneyName), decreasing = TRUE))
     valueBox(subtitle = "Is the most common Attorney", value = name, icon = icon("briefcase"),  color = "green")
   })
   
   # Average Taxes Owed infobox
   output$avgtaxes <- renderValueBox({
     proper <- propInput()
     nums <- prettyNum(round(mean(sale.load$CostsTaxes, na.rm = T), 0))
     valueBox(subtitle = "Average Taxes Owed ", value = nums, icon = icon("usd"), color = "red")
   })
   
   # Most in a zipcode infobox
   output$zipcode <- renderValueBox({
     proper <- propInput()
     name <- names(sort(table(sale.load$ZIPCode), decreasing = TRUE))
     valueBox(subtitle = "This Zipcode has the most Sheriff Sales", value = name, icon("home"), color = "blue")
   })
   
   # Make data downloadable and set default download name
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("sheriff-sale-data-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(propInput(), file)
     })
 }
 # Run the application
shinyApp(ui = ui, server = server)


