## app.R ##
library(shiny)
library(shinydashboard)
library(RJSONIO)
library(leaflet)
library(googleVis)
source("utils.R")

token <- "3286805.a6db5a4.77093db3a963441f94d09832af5b87bb"
# UI section ----

## Header content ----
header <- dashboardHeader(title = "INSTAPOSTER")


## Sidebar content ----
sidebar <- dashboardSidebar( 
        
        # Css style for having the button centered.
        tags$style(".centerAlign{text-align: center;}
                   .hidden{display: none;}"),
        
        # Instructions
        tags$p("Put the latitude and logitude for obtaining the publications around this point."),
        
        # Input for obtaining the latitude
        textInput(inputId = "latitude", label = "Latitude", width = "100%"),
        
        # Input for obtaining the longitude
        textInput(inputId = "longitude", label = "Longitude", width = "100%"),
        
        # Slider for obtaining the distance
        sliderInput(inputId = "distance", label = "Distance", min = 100, max = 5000,
                    value = 1000, step = 100, width = "100%" ),
        
        # Date range input for selecting the date from and until
        dateRangeInput(inputId = "dateRange", label = "Dates from/to", start = Sys.Date()-2,
                       end = Sys.Date()-1, format = "dd-mm-yyyy", separator = "to",
                       width = "100%"),
        
        tags$br(),
        tags$div(actionButton(inputId = "search", label = "Search", icon = icon("search"), width = "60%"),
                 class = "centerAlign"),
        
        tags$br(),
        tags$div(downloadButton(outputId = "downloadData", label = "Download"),
                 class = "centerAlign")
)


## Body content ----
body <- dashboardBody(
        fluidPage(
                # Page content
                tabBox( id = "tab",width = "100%", height = "100%",
                        tabPanel( "Map", 
        #                 fluidRow(box(title = "Publications Map",
        #                              width = "100%",
        #                              leafletOutput("map"))
        #                 ),
                        
                                fluidRow(
                                        valueBoxOutput("NumPub"),
                                        valueBoxOutput("NumUsers"),
                                        valueBoxOutput("NumHashtags")
                                ),                                        
        
                                fluidRow(leafletOutput("map"))
                        ),
                        
                tabPanel("Insights",
                         fluidRow(
                                 
                         )
                         
                         )
                ),
                tags$div(tags$p("made by: Sergio Aguado"), class = "hidden")
        )

)


ui <- dashboardPage(header, sidebar, body) 


# SERVER section ----
server <- function(input, output, session) {
        
        # Calling the app configuration
        if (!exists("token") || is.null(token)) {
                configApp()     
        }
        
        # Initialize the map
        map <- leaflet() %>%
                addProviderTiles("Tittle",
                                 options = providerTileOptions(noWrap = TRUE))
        
        
        # Get all the data from the inputs
        dataInput <- eventReactive(input$search, {

#                 print(input$latitude)
#                 print(input$longitude)
#                 print(input$distance)
#                 print(str(input$dateRange))
#                 print(as.numeric(as.POSIXct(input$dateRange[1])))
#                 print(as.numeric(as.POSIXct(input$dateRange[2])))
                
                print("ha entrado")
                
                
                if (testInputs(as.numeric(input$latitude),
                               as.numeric(input$longitude),                
                               as.numeric(as.POSIXct(input$dateRange[1])),
                               as.numeric(as.POSIXct(input$dateRange[2])),
                               input$distance)==T) {
                        
                        print("Bien")
                        result <- searchMedia(lat = as.numeric(input$latitude),
                                              lng = as.numeric(input$longitude),
                                              min_time = as.numeric(as.POSIXct(input$dateRange[1])),
                                              max_time = as.numeric(as.POSIXct(input$dateRange[2])),
                                              dist = input$distance)
                        
                } else {
                        
                        result <- NULL
                }
                
                
                result
                
        })    
        
        
#         appURL <- reactive({
#                 if (!is.null(session)) {
#                         ## build redirect URI
#                         paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, 
#                                ifelse(session$clientData$url_hostname == "127.0.0.1", 
#                                       ":", session$clientData$url_pathname), session$clientData$url_port)
#                 }
#         })
        
        # Map with all publications
        output$map <- renderLeaflet({
                
                if (!is.null(dataInput())) {
                        
                        map <- plotPointsMap(media = dataInput()$media,
                                             lat = as.numeric(input$latitude),
                                             lng = as.numeric(input$longitude))
                        print("map loaded")
                }
                
                map
        })
        
        # Num publications output
        output$NumPub <- renderValueBox({
                pub <- 0
                if (!is.null(dataInput())) {
                        pub <- dim(dataInput()$media)[1]
                        
                }
                
                valueBox(
                        pub, "Publications", icon = icon("picture", lib = "glyphicon"),
                        color = "blue"
                )
        })
        
        # Num Users output
        output$NumUsers <- renderValueBox({
                users <- 0
                if (!is.null(dataInput())) {
                        users <- getUsers(dataInput()$media)
                        
                }
                
                valueBox(
                        length(users), "Users", icon = icon("user", lib = "glyphicon"),
                        color = "teal"
                )
        })
        
        # Num hashtags output
        output$NumHashtags <- renderValueBox({
               hashtags <- 0
                if (!is.null(dataInput())) {
                        hashtags <- getHashTags(dataInput()$media)
                        
                }
                
                valueBox(
                        length(hashtags), "Hashtags", icon = icon("align-justify", lib = "glyphicon"),
                        color = "aqua"
                )
        })
        
        # Download button
        output$downloadData <- downloadHandler(
                filename <- "temp.csv",
                
                content <- function(file){
                        media <- dataInput()$media
                        esFormat<-T
                        if (esFormat==T) {
                                write.table(media, file, sep = ";", 
                                            dec = ",", row.names = F, na = "")     
                        } else {
                                write.csv(media, file, row.names = F)  
                        }
                },
                
                contentType <-  "text/csv"
        )
        
        

}


# APP ----
shinyApp(ui = ui, server = server)
