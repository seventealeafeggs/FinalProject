getwd()
library(sf)
library(data.table)
library(tidyverse)

#Sys.setlocale(category="LC_ALL",locale="zh_TW.UTF-8")
#data
town <- st_read(dsn="鄉鎮市區.shp",options="ENCODING=BIG-5",stringsAsFactors=FALSE,crs=3826)
town <- st_transform(town,crs = 4326)
vote <- fread("perfectdata.csv",encoding = "UTF-8")
data <- dplyr::left_join(x=town,y=vote,by=c("COUNTY","TOWN"))
point<- fread("商業登記_經緯度//雙北_核准設立_化妝品零售業.csv")


data$city <- paste0(data$COUNTY,data$TOWN)


# create marker
PinenutIcon <- makeIcon(
  iconUrl = "cone.png",
  iconWidth = 14, iconHeight = 14,
  iconAnchorX = 0, iconAnchorY = 0
)
#----
library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                checkboxInput("circle", "化妝品零售業分布(circle)", FALSE),
                checkboxInput("markers", "化妝品零售業分布(markers)", FALSE)
                
  )
)

server <- function(input, output, session) {
 
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data) %>%  
      addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 121.537964 , lat = 25.039485, zoom = 12)
  })


# circle  
  observe({ ## circle
    proxy <- leafletProxy("map", data = point)
    proxy %>% clearShapes()
    if (input$circle){
     proxy%>%
     addCircles(~經度, ~緯度, radius=30,
                stroke=FALSE, fillOpacity=0.4)
  }
})


# markers
  observe({ ## markers
    proxy <- leafletProxy("map", data = point)
    proxy %>% clearMarkers()
    if (input$markers){
    proxy%>%
    addMarkers(~經度, ~緯度, icon = PinenutIcon)
   }
 })
  
    
}



shinyApp(ui, server)
