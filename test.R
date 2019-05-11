getwd()
library(sf)
library(data.table)
library(tidyverse)
Sys.setlocale(category="LC_ALL",locale="zh_TW.UTF-8")
#data
town <- st_read(dsn="鄉鎮市區.shp",options="ENCODING=BIG-5",stringsAsFactors=FALSE,crs=3826)
town <- st_transform(town,crs = 4326)
vote <- setDT(rio::import("perfectdata.csv"))

data <- dplyr::left_join(x=town,y=vote,by=c("COUNTY","TOWN"))

County<-st_cast(st_union(x=summarise(group_by(.data=tvdata,COUNTY,COUNTY_ID)),
                         by_feature = TRUE))


### leaflet

library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)

quakes %>% str()
##
library(data.table)
library(stringr)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(foreign)
library(sf)
library(grid)
library(plotly)
library(C50)
Sys.setlocale(category="LC_ALL",locale="zh_TW.UTF-8")
setwd("/Users/will/Desktop/行政區")
town<-st_read(dsn="鄉鎮市區.shp",options="ENCODING=BIG-5",stringsAsFactors=FALSE,crs=3826)
setwd("/Users/will/Desktop/0316")

Lowincome <- setDT(rio::import("lowincome.xlsx"))


Lowincome1 <- Lowincome[,c(2,4,6)]
dimnames(Lowincome1)[[2]] <- c("COUNTY","TOWN","中低收入戶占總戶數比例")
tvdata <- dplyr::left_join(x=town,y=Lowincome1,by=c("COUNTY","TOWN"))
tvdata %>% head()

tvdata1 <- tvdata
tvdata2 <- setDT(tvdata1)
tvdata2 %>% head()
tvdata2 <- as.data.frame(tvdata2)

leaflet(tvdata2) %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5
  ) 
TaipeiCenter<-st_centroid(town)
TaipeiCenter$geometry <- as.factor(TaipeiCenter$geometry)
TaipeiCenter <- setDT(TaipeiCenter)

TaipeiCenter %>% str()
TaipeiCenter$geometry %>% unlist() ->listg
TaipeiCenter$ll <- listg
substring(TaipeiCenter$ll,0,9)

as.factor(TaipeiCenter$geometry)
Lowincome <- setDT(rio::import("ii.csv"))
Lowincome$lng <- paste0(substring(Lowincome$long,1,3),".",substring(Lowincome$long,5,6))
Lowincome$lng <- as.numeric(Lowincome$lng)

Lowincome$lat <- paste0(substring(Lowincome$lag,1,2),".",substring(Lowincome$lag,4,5))
Lowincome$lat <- as.numeric(Lowincome$lat)

mini <- Lowincome[,c(9,12,13,6)]
mini %>% str()
##test
mini$中低收入戶占總戶數比例 <- as.numeric(mini$中低收入戶占總戶數比例)
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(mini$中低收入戶占總戶數比例), max(mini$中低收入戶占總戶數比例),
                            value = range(mini$中低收入戶占總戶數比例), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    mini[mini$中低收入戶占總戶數比例 >= input$range[1] & mini$中低收入戶占總戶數比例 <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, mini$中低收入戶占總戶數比例)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(mini) %>% addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~中低收入戶占總戶數比例^2, weight = 1, color = "#777777",
                 fillColor = ~pal(中低收入戶占總戶數比例), fillOpacity = 0.7, popup = ~paste(中低收入戶占總戶數比例)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = mini)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~中低收入戶占總戶數比例
      )
    }
  })
}

shinyApp(ui, server)
