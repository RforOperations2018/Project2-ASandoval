

# Upload packages
require(rgdal)
require(leaflet)
require(leaflet.extras)
require(dplyr)
require(readxl)
require(stringr)



# 2018 Philadelphia Marathon Route
# This data was created from http://geojson.io/#map=2/20.0/0.0.
# Point and line data was traced from the 2018 Philadelphia marathon website, https://philadelphiamarathon.phila.gov/#/
# Neighborhoods were downloaded from https://github.com/azavea/geo-data

# Upload polylines, polypoints, and polygons
route <- readOGR("marathon.geojson")
plot(route)
markers <- readOGR("finalmarkers.geojson")
plot(markers)
hoods <- readOGR("Neighborhoods_Philadelphia.geojson")
plot(hoods)

# Add an if else statement to change colors of the markers
getColor <- function(markers) {
  sapply(markers$marker.symbol, function(marker.symbol) {
    if(marker.symbol %in% "S") {
      "green"
    } else if(marker.symbol %in% "F") {
      "red"
    } else {
      "orange"
    } })
}

# Add icons
icons <- awesomeIcons(
  text = ~as.character(marker.symbol),
  markerColor = getColor(markers)
)

# Choose Icon Legend
html_legend <- "Philadelphia Marathon 2018 Route <br/> <br/> <img src='https://png.icons8.com/material-outlined/50/000000/marker.png'>Mile Markers "

# Plot map
leaflet() %>%
  # Add Philly 2018 Marathon Route
  addPolylines(data = route, color = "#000000", fillOpacity = 1, weight = 10) %>%
  # Add Philly Neighborhoods
  addPolygons(data = hoods, color = "#1ab2ff", label = ~mapname, fillOpacity = 0.00, weight = 3, highlightOptions = highlightOptions(color = "red", bringToFront = TRUE)) %>%
  # Add Philly 2018 Mile Markers
  addAwesomeMarkers(data = markers, icon = icons, label = ~as.character(marker.symbol)) %>%
  # Add Basemaps
  addProviderTiles(providers$OpenMapSurfer.Grayscale, options = providerTileOptions(noWrap = TRUE)) %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group = "Default") %>%
  addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Imagery") %>%
  addProviderTiles("Esri.WorldTerrain", options = providerTileOptions(noWrap = TRUE), group = "Terrain") %>%
  # Add Layers control
  addLayersControl(
    baseGroups = c("Default", "Imagery", "Terrain"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Set View
  setView( lat = 39.980379,  lng= -75.165490, zoom = 12.1) %>%
  # Add Legend
  addControl(html_legend, position = "bottomleft")



# library(shiny)
# library(reshape2)
# library(dplyr)
# library(plotly)
# library(shinythemes)
# library(stringr)
# library(shinydashboard)
# library(readr)
# require(scales)
# 
# 
# # Upload Philadelphia property assessment data from Opendataphilly
# # Many fields were removed to decrease data upload
# # Data can be found here: https://www.phila.gov/property/data/
# # There were originally 580,919 rows of data. I used a random number generator to get 2000 rows. 
# # It now runs faster. 
# # link to shinyapp.io https://assandoval.shinyapps.io/Project1-ASandoval/ 
# property.load <- read_csv ("projectdata_7.csv")
# 
# 
# pdf(NULL)
# 
# 
# header <- dashboardHeader(title = "Philadelphia Properties",
#                           dropdownMenu(type = "messages",
#                                        messageItem(
#                                          from = "Donald J. Trump",
#                                          message = HTML("Help me expand Trump Organization!"),
#                                          icon = icon("exclamation-circle"))
#                           )
# )
# choices <- c("Select All", "1", "2", "3", "4")
# 
# 
# sidebar <- dashboardSidebar(
#   # bars on the side
#   sidebarMenu(
#     id = "tabs",
#     menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
#     menuItem("Table", icon = icon("table"), tabName = "table"),
#     # No third page?!
#     # Category Select
#     selectInput("categorySelect",
#                 "Types of Properties:",
#                 choices = sort(unique(property.load$category_code_description)),
#                 multiple = TRUE,
#                 selectize = TRUE,
#                 selected = c("Single Family", "Vacant Land", "Commercial", "Industrial")),
#     # Year Select
#     sliderInput("yearSelect",
#                 "Sale Year:",
#                 min = min(property.load$sale_year, na.rm = T),
#                 max = max(property.load$sale_year, na.rm = T),
#                 value = c(min(property.load$sale_year, na.rm = T), max(property.load$sale_year, na.rm = T)),
#                 step = 25),
#     # Check box Input for how many bathrooms
#     checkboxGroupInput(inputId = "bathroomSelect",
#                        label = "How Many Bathrooms does the Property Have?:",
#                        choiceNames = list("0", "1", "2", "3", "4", "5", "6"),
#                        choiceValues = list("0", "1", "2", "3", "4", "5", "6")),
#     # Select input for story
#     selectInput("storySelect",
#                 "Story:",
#                 choices = sort(unique(property.load$number_stories)),
#                 multiple = TRUE,
#                 selectize = TRUE,
#                 selected = c("0","1", "2", "3", "4", "5", "6","22", "28", "31", "33"))
#   )
# )
# 
# 
# body <- dashboardBody(tabItems(
#   # names of boxes
#   tabItem("plot",
#           fluidRow(
#             infoBoxOutput("avgmarket"),
#             infoBoxOutput("avgprice"),
#             infoBoxOutput("totaltaxland")
#           ),
#           # names of the plot tabs
#           fluidRow(
#             tabBox(title = "Plot", width = 12,
#                    tabPanel("Property Change of Value", plotlyOutput("plot_value")),
#                    tabPanel("Properties by Ward", plotlyOutput("plot_properties")),
#                    tabPanel("Purchases by Year", plotlyOutput("plot_years")))
#           )
#   ),
#   # Table name
#   tabItem("table",
#           fluidPage(
#             box(title = "Philadelphia Property Assessment Data", DT::dataTableOutput("table"), width = 12))
#   )
# )
# )
# 
# 
# ui <- dashboardPage(header, sidebar, body)
# 
# # Define server logic
# server <- function(input, output) {
#   propInput <- reactive({
#     property <- property.load  %>%
#       
#       # Slider Filter
#       filter(sale_year >= input$yearSelect[1] & sale_year <= input$yearSelect[2])
#     
#     # Category Filter
#     if (length(input$categorySelect) > 0 ) {
#       property <- subset(property, category_code_description %in% input$categorySelect)
#     }
#     # Is there a bathroom inside?
#     if (length(input$bathroomSelect) > 0 ) {
#       property <- subset(property, number_of_bathrooms %in% input$bathroomSelect)
#     }
#     # Select Story
#     if (length(input$storySelect) > 0 ) {
#       property <- subset(property, number_stories %in% input$storySelect)
#     }
#     return(property)
#   })
#   # Reactive melted data
#   mInput <- reactive({
#     property <- propInput()
#     property_m <- property %>%
#       melt(id = "category_code_description")
#   })
#   # A plot showing the sale price of properties
#   output$plot_value <- renderPlotly({
#     property <- propInput()
#     ggplot(data = property, 
#            aes(x = sale_year, 
#                y = change_value, 
#                fill = category_code_description))  + 
#       geom_point(stroke = 0) +
#       guides(fill = FALSE) +
#       scale_y_continuous(name = "Property Change of Value", 
#                          labels = comma, 
#                          breaks = c(-10000000,-800000, -600000, -400000, -200000, 0, 200000, 400000, 600000, 800000, 1000000)) +
#       scale_x_continuous(name = "Sale Year", 
#                          breaks = c(1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
#       theme(legend.title = element_blank())
#   })
#   # A plot showing properties by ward
#   output$plot_properties <- renderPlotly({
#     property <- propInput()
#     ggplot(data = property, 
#            aes(x = geographic_ward,
#                fill = category_code_description)) +
#       geom_bar(position = "stack") +
#       guides(fill = FALSE) +
#       scale_y_continuous(name = "Count of Properties") +
#       scale_x_continuous(name = "Wards", 
#                          breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 66))
#   })
#   # A plot showing the the fequency of properties purchased over the years
#   output$plot_years <- renderPlotly({
#     property <- propInput()
#     ggplot(data = property, 
#            aes(x = sale_year, 
#                color = category_code_description ))  + 
#       geom_freqpoly() +
#       guides(fill = FALSE) +
#       scale_x_continuous(name = "Sale Year",
#                          breaks = c(1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
#       scale_y_continuous(name = "Count of Property Purchases") +
#       theme(legend.title = element_blank())
#   })
#   # Data table of Assessment
#   output$table <- DT::renderDataTable({
#     subset(propInput(), select = c(category_code_description, location, market_value, owner_1, parcel_number, sale_date, sale_price))
#   })
#   # Average current market value box
#   output$avgmarket <- renderInfoBox({
#     proper <- propInput()
#     num <- round(mean(property.load$market_value, na.rm = T), 0)
#     # When you're dealing with values in the thousands wrapping your value in prettyNum() will give you a nice thousands seperator
#     valueBox(subtitle = "Average Market Value", value = num, icon = icon("usd"), color = "red")
#   })
#   # Average sale price box
#   output$avgprice <- renderValueBox({
#     proper <- propInput()
#     num <- round(mean(property.load$sale_price, na.rm = T), 0)
#     valueBox(subtitle = "Average Sale Price", value = num, icon = icon("credit-card"), color = "blue")
#   })
#   # Total Taxable Land box
#   output$totaltaxland <- renderValueBox({
#     proper <- propInput()
#     num <- sum(property.load$taxable_land, na.rm = T)
#     valueBox(subtitle = "Total Taxable Land", value = num, icon = icon("thumbs-up"), color = "purple")
#   })
# }
# # Run the application 
# shinyApp(ui = ui, server = server)


