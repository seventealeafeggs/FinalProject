library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)
library(data.table)
library(stringr)
library(readr)
library(foreign)
library(readxl)
library(haven)
library(tidyverse)
load("dataTP.rdata")
dataTP <- dataTP
## V2
## shinyapp
#----
## success

ui <- bootstrapPage(
  
  tags$head(
    # Include our custom CSS
    includeCSS("type.css")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 340, height = "auto",fillOpacity = 0.7,
                
                h2("Select Bar"),
                
                selectInput("marry", "是否加入「已婚有偶之比例」進行分析",
                            c("Yes","No")
                ),
                selectInput("college", "是否加入「擁有大學以上學歷人口比例」進行分析",
                            c("Yes","No")
                ),
                selectInput("cohort", "請選擇一個世代進行分析",
                            colnames(setDT(dataTP[,c(15,18:22)])[,1:6])
                ),
                selectInput("target", "想分析哪個產業呢？",
                            colnames(setDT(dataTP[,c(24:26)])[,1:3])
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colmar <- reactive({
    if(input$marry=="No"&input$college=="No"&input$cohort=="老化指數"&input$target=="化妝品業家數") return(dataTP$化妝m1)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="老化指數"&input$target=="化妝品業家數") return(dataTP$化妝m2)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="老化指數"&input$target=="化妝品業家數") return(dataTP$化妝m3)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="老化指數"&input$target=="化妝品業家數") return(dataTP$化妝m4)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_0到19歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m5)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_0到19歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m6)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_0到19歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m7)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_0到19歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m8)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_20到39歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m9)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_20到39歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m10)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_20到39歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m11)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_20到39歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m12)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_40到59歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m13)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_40到59歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m14)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_40到59歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m15)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_40到59歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m16)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_60到79歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m17)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_60到79歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m18)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_60到79歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m19)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_60到79歲人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m20)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_80歲以上人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m21)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_80歲以上人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m22)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_80歲以上人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m23)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_80歲以上人口比例"&input$target=="化妝品業家數") return(dataTP$化妝m24)
    
    #daily
    if(input$marry=="No"&input$college=="No"&input$cohort=="老化指數"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m1)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="老化指數"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m2)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="老化指數"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m3)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="老化指數"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m4)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_0到19歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m5)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_0到19歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m6)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_0到19歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m7)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_0到19歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m8)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_20到39歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m9)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_20到39歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m10)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_20到39歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m11)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_20到39歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m12)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_40到59歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m13)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_40到59歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m14)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_40到59歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m15)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_40到59歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m16)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_60到79歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m17)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_60到79歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m18)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_60到79歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m19)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_60到79歲人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m20)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_80歲以上人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m21)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_80歲以上人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m22)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_80歲以上人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m23)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_80歲以上人口比例"&input$target=="日常用品業家數") return(dataTP$日常用品業家數m24)
    
    #res
    if(input$marry=="No"&input$college=="No"&input$cohort=="老化指數"&input$target=="餐館業家數") return(dataTP$餐館業家數m1)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="老化指數"&input$target=="餐館業家數") return(dataTP$餐館業家數m2)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="老化指數"&input$target=="餐館業家數") return(dataTP$餐館業家數m3)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="老化指數"&input$target=="餐館業家數") return(dataTP$餐館業家數m4)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_0到19歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m5)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_0到19歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m6)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_0到19歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m7)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_0到19歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m8)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_20到39歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m9)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_20到39歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m10)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_20到39歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m11)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_20到39歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m12)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_40到59歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m13)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_40到59歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m14)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_40到59歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m15)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_40到59歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m16)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_60到79歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m17)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_60到79歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m18)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_60到79歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m19)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_60到79歲人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m20)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_80歲以上人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m21)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_80歲以上人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m22)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_80歲以上人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m23)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_80歲以上人口比例"&input$target=="餐館業家數") return(dataTP$餐館業家數m24)
    
  })
  
  colorpal <- reactive({
    bins <- c(Inf,10,7,5,3,1,-1,-3,-5,-7,-10,-Inf)
    colorBin("PiYG",domain = colmar(), bins = bins)
    
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(dataTP) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(
        lng = 121.537964 , lat = 25.039485, zoom = 12
      )
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%s:%g<br/>%s:%g<br/>%s:%g<br/>%s:%g<br/>%s:%g<br/>%s:%g",
      dataTP$city,"人口密度", dataTP$人口密度, "性別比",dataTP$性別比,"所得中位數",dataTP$所得中位數,"已婚有偶人口之比例",dataTP$已婚有偶人口之比例,"擁有大學以上學歷人口比例",dataTP$擁有大學以上學歷人口比例,"產業潛力",colmar()
    ) %>% lapply(htmltools::HTML)
    
  })
  observe({
    pal <- colorpal()
    leafletProxy("map", data = dataTP)%>% clearShapes() %>% addPolygons(
      fillColor = ~pal(colmar()),
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
      label = labels(),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = dataTP)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomleft",
                          pal = pal, values = ~colmar(),title = "分類結果"
      )
    }
  })
}

shinyApp(ui, server)
