getwd()
library(sf)
library(data.table)
library(tidyverse)
Sys.setlocale(category="LC_ALL",locale="zh_TW.UTF-8")
#data
town <- st_read(dsn="鄉鎮市區.shp",options="ENCODING=BIG-5",stringsAsFactors=FALSE,crs=3826)
town <- st_transform(town,crs = 4326)
vote <- setDT(rio::import("perfectdata.csv"))
s <- fread("perfectdata.csv")
data <- dplyr::left_join(x=town,y=vote,by=c("COUNTY","TOWN"))

County<-st_cast(st_union(x=summarise(group_by(.data=tvdata,COUNTY,COUNTY_ID)),
                         by_feature = TRUE))
data$city <- paste0(data$COUNTY,data$TOWN)

#----
library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("variable", "Variables select",
                            colnames(data[,c(16,12:15)])
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  #labels <- reactive({
  #  labels <- sprintf(
  #   "<strong>%s</strong><br/>所得中位數:%g",
  #   data$city, data[,input$variable]
  #  ) %>% lapply(htmltools::HTML)
  
  #})
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  bins <- reactive({
    if(input$variable=="所得中位數") return(c(0, 40, 50, 60, 70, 90, 100, 120, Inf))
    if(input$variable=="年齡中位數") return(c(0,40,42,44,46,48,50,52,54,Inf))
    if(input$variable=="曾經結婚比例") return(c(0,0.6,0.64,0.68,0.72,0.76,Inf))
    if(input$variable=="大學畢業比例") return(c(0,0.15,0.18,0.21,0.24,0.3,0.4,0.5,0.6,Inf))
    if(input$variable=="KMT得票率") return(c(0,0.2,0.3,0.35,0.40,0.45,0.50,0.55,0.6,0.7,0.8,0.9,Inf))
  })
  colorpal <- reactive({
    colorBin(input$colors, domain = data[,input$variable], bins = bins())
    
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data) %>% addTiles() %>%
      setView(120.5, 23.5, 8)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  var <- reactive({
    if(input$variable=="所得中位數") return(data$所得中位數)
    if(input$variable=="年齡中位數") return(data$年齡中位數)
    if(input$variable=="曾經結婚比例") return(data$曾經結婚比例)
    if(input$variable=="大學畢業比例") return(data$大學畢業比例)
    if(input$variable=="KMT得票率") return(data$KMT得票率)
  })
  
  observe({
    pal <- colorpal()
    leafletProxy("map", data = data)%>% addPolygons(
      fillColor = ~pal(var()),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = data)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~data[,input$variable]
      )
    }
  })
}

shinyApp(ui, server)
