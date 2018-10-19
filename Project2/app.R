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
library(shinyjs)
library(httr)
library(jsonlite)
library(plyr)
#library(shinyWidgets)
#library(wordcloud2)
library(htmltools)

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
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

category <- sort(ckanUniques("SaleType", "4af05575-052d-40ff-9311-d578319e810a")$SaleType)
taxes <- sort(ckanUniques("CostsTaxes", "4af05575-052d-40ff-9311-d578319e810a")$CostsTaxes)
ready <- sort(ckanUniques("ReadyForSale", "4af05575-052d-40ff-9311-d578319e810a")$ReadyForSale)
  
  
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
                selected = c("Mortgage Foreclosure", "Municipal Lien", "Other Real Estate", "Sci Fa sur Tax Lien")),

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
                min = min(taxes),
                max = max(taxes),
                value = c(min(taxes), max(taxes)),
                step = 5000),

    # Select Ready for Auction
   selectizeInput("readySelect",
                  "Is the Property Ready for Auction?",
                  choices = c("Yes", "No"),
                  multiple = FALSE,
                  selected = "Yes"),
   
   # Reset button
   actionButton("reset", "Reset Filters", icon = icon("refresh")),
   actionButton("button", "Submit")
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
 server <- function(input, output, session=session) {
   
   # Creating filtered sheriff sale data
   propInput <- eventReactive(input$button, {
       # API Stuff

     url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%224af05575-052d-40ff-9311-d578319e810a%22%20WHERE%20%22CostsTaxes%22%20%3E%3D%27", 
                   input$taxesSelect[1],"%27%20AND%20%22CostsTaxes%22%20%3C%3d%27", input$taxesSelect[2], "%27%20AND%20%22SaleType%22%20IN%20%28%27",
                   gsub(" ", "%20", input$categorySelect[1]), "%27%2C%20%27",
                   gsub(" ", "%20", input$categorySelect[2]), "%27%2C%20%27",
                   gsub(" ", "%20", input$categorySelect[3]), "%27%2C%20%27",
                   gsub(" ", "%20", input$categorySelect[4]), "%27%29")
# %29%20AND%20%22ReadyForSale%22%20IN%20%28%27",
#                    gsub(" ", "%20", input$readySelect[1]), "%27%29")
#                    
                   # IN%20%28%27",
                   # gsub(" ", "%20", input$readySelect), "%27%29")
                   
     
     #dates don't work
                   # "%27%20AND%22SaleDate%22%20%3E%3D%27", input$dateSelect[1], 
                   # "%27%20AND%20%22SaleDate%22%20%3C%3d%27", input$dateSelect[2], "%27")
                   
                   

     
                   
                   
                   
                  # "%27%20AND%22SaleDate%22%20%3E%3d%27", input$dateSelect[1], "%27")
                   # input$dateSelect[1], "%27%20AND%20%22SaleDate%22%3C%3D%27",
                   # input$dateSelect[2], "%27%29" )
                   
                   
                   
                   # "%27%20AND%20%22CostsTaxes%22%20%3E=%20%27", 
                   # input$taxesSelect[1], "%27%20AND%20%22CostsTaxes%22%20%3C=%20%27",
                   # input$taxesSelect[2], "%27%20AND%20%22SaleType%22%20IN%20%28%27", 
                   # gsub(" ", "%20",input$categorySelect[1]),"%27%2C%20%27", 
                   # gsub(" ", "%20",input$categorySelect[2]),"%27%2C%20%27", 
                   # gsub(" ", "%20",input$categorySelect[3]), "%27%2C%20%27", 
                   # gsub(" ", "%20",input$categorySelect[4]), "%27%29%20AND%20%22ReadyForSale%22%20=%20%28%27",
                   # gsub(" ", "%20", input$readySelect[1]), "%27%29"
                   # )
    
     

  # "         %20%22SafetyAV%22%3E%3D%27", input$safetySelect[1],
  #  "%27%20AND%20%22SafetyAV%22%3C%3D%27",input$safetySelect[2],
  #  "%27%20AND%20%22FeelingsProvingGround%22%20IN%20%28%27",
  #  gsub(" ", "%20", input$feelSelect[1]), "%27%2C%20%27",
  #  gsub(" ", "%20", input$feelSelect[2]), "%27%2C%20%27",
  #  gsub(" ", "%20", input$feelSelect[3]), "%27%2C%20%27",
  #  gsub(" ", "%20", input$feelSelect[4]), "%27%2C%20%27",
  #  gsub(" ", "%20", input$feelSelect[5]), "%27%29%20AND%20%22FamiliarityTechnoology%22%20IN%20%28%27",
  #  gsub(" ", "%20", input$techSelect[1]), "%27%2C%20%27",
  #  gsub(" ", "%20", input$techSelect[2]), "%27%2C%20%27",
  #  gsub(" ", "%20", input$techSelect[3]), "%27%2C%20%27",
  #  gsub(" ", "%20", input$techSelect[4]), "%27%2C%20%27",
  #  gsub(" ", "%20", input$techSelect[5]),"%27%29"
  #  )"
  print(url)
     
     
     
         sale.load <- ckanSQL(url) %>%
           mutate(
             AttorneyName = str_replace_all(AttorneyName, '"', ""),
             SaleDate = as.POSIXct(SaleDate),
             City = case_when(
               City %in% c("PITTSBURGH", "PITSBURGH", "PITTBURGH", "PITTSBIURGH", "PITTSBRGH", "PITTSBSURGH") ~ "Pittsburgh"),
             ZIPCode = str_replace_all(ZIPCode, '"', ""),
             ReadyForSale = case_when(
               ReadyForSale %in% c("yes", "yes.no", TRUE) ~ "Yes",
               ReadyForSale %in% c("no", "no.no", FALSE) ~ "No")
           )
         
         return(sale.load) 

       # zipcodes <- readOGR("County_Zip_Code.geojson")
     })

   #map
   # output$map <- renderLeaflet({
   #   # Plot map 
   #   leaflet() %>%
   #     
   #     # Add Basemaps
   #     addProviderTiles(providers$OpenMapSurfer.Grayscale, options = providerTileOptions(noWrap = TRUE)) %>%
   #     addTiles(options = providerTileOptions(noWrap = TRUE), group = "Default") %>%
   #     addProviderTiles("Esri.WorldTerrain", options = providerTileOptions(noWrap = TRUE), group = "Terrain") %>%
   #     
   #     # Set View
   #     setView(lat = 40.44, lng = -79.95, zoom = 11.8) %>%
   #     
   #     # Add Pittsburgh Zip Codes
   #     addPolygons(data = zipcodes, color = "#000000", label = ~ZIP, fillOpacity = 0.00) %>%
   #     
   #     # Add Layers control
   #     addLayersControl(
   #       baseGroups = c("Default", "Terrain"),
   #       options = layersControlOptions(collapsed = FALSE)
   #     ) %>%
   # 
   #     # Add Sheriff Sale Points
   #     addAwesomeMarkers(data = propInput(),
   #                       lat = ~latitude,
   #                       lng = ~longitude,
   #                       label = ~SaleType,
   #                       clusterOptions = markerClusterOptions())
   # })
     
   # Plot 1-  Counts of Properties by Sale Types
   output$plot_types <- renderPlotly({
     property <- propInput()
     ggplotly( ggplot(data = property,
            aes(x = CostsTaxes, color = SaleType))  +
       geom_freqpoly(binwidth = 500) +
       guides(fill = FALSE) +
       scale_y_continuous(name = "Count of Properties") +
       scale_x_continuous(name = "Taxes Owed") +
       theme(axis.text.x = element_text(angle = 15,
                                        vjust = 1,
                                        hjust = 1)))
   })
   
   
   # Plot 2- Plot showing taxes owed by zip code
   output$plot_taxes <- renderPlotly({
     
     property <- propInput ()
     df <- property 
     
     ggplotly( ggplot (data = property,
            aes (x = ZIPCode,
                y = round (CostsTaxes, 0), na.rm = T )) +
       geom_col (position = position_dodge(width = 0.9)) +
       guides (fill = FALSE) +
       theme(axis.text.x = element_text(angle = 30,
                                        hjust = 1),
             axis.text = element_text(size = rel(0.5))) +
       scale_y_continuous (name = "Sum of Taxes Owed") +
       scale_x_discrete (name = "Zip Code"))
   })

   # Data table of Assessment
   output$table <- DT::renderDataTable({
     df <- propInput()
     subset(df, select = c(DocketNumber, SaleType, AttorneyName, Plaintiff, Defendant, SaleDate, Address, CostsTaxes))
   })
  
   # Common Attorney infobox
   output$attorney <- renderInfoBox({
     proper <- propInput()
     name <- names(sort(table(propInput$AttorneyName), decreasing = TRUE))
     valueBox(subtitle = "Is the most common Attorney", value = name, icon = icon("briefcase"),  color = "green")
   })
   
   # Average Taxes Owed infobox
   output$avgtaxes <- renderValueBox({
     proper <- propInput()
     nums <- prettyNum(round(mean(propInput$CostsTaxes, na.rm = T), 0))
     valueBox(subtitle = "Average Taxes Owed ", value = nums, icon = icon("usd"), color = "red")
   })
   
   # Most in a zipcode infobox
   output$zipcode <- renderValueBox({
     proper <- propInput()
     name <- names(sort(table(propInput$ZIPCode), decreasing = TRUE))
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
   
   # Reset Filter Data
   observeEvent(input$reset, {
     updateSelectInput(session, "categorySelect", selected = "Mortgage Foreclosure")
     #updateDateRangeInput(session, "dateSelect", selected = c("Mostly familiar"))
     updateSliderInput(session, "taxesSelect", value = c(min(taxes), max(taxes)))
     updateSelectizeInput(session, "readySelect", selected = c("Yes"))
     showNotification("You have successfully reset the filters! Make sure to hit the Submit again!", type = "message")
   })
 }
 
 # Run the application
shinyApp(ui = ui, server = server)


