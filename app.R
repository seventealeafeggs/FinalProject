library(sf)
library(data.table)
library(tidyverse)
Sys.setlocale(category="LC_ALL",locale="zh_TW.UTF-8")
vilg <- st_read(dsn="村里.shp",options="ENCODING=BIG-5",stringsAsFactors=FALSE,crs=3826)
vilg <- st_transform(vilg,crs = 4326)
pop <- setDT(rio::import("STAT_TP.xlsx"))
pop <- load("STAT_TP.rdata")


dtp <- fread("雙北人口結構加機器學習資料V2.csv")

fwrite(dtp,"雙北人口結構加機器學習資料V2.csv")
## shiny
dtp %>% str()
dimnames(dtp)[[2]] <- c("COUNTY_ID","COUNTY","TOWN_ID","TOWN","V_ID","VILLAGE","人口總數","性別比","平均每戶人數","人口密度","老化指數","擁有大學以上學歷人口比例","已婚有偶人口之比例","世代_0到19歲人口比例","世代_20到39歲人口比例","世代_40到59歲人口比例","世代_60到79歲人口比例","世代_80歲以上人口比例","所得中位數","ml1","ml2","ml3","ml4","ml5","ml6","ml7","ml8","ml9","ml10","ml11","ml12","ml13","ml14","ml15","ml16","ml17","ml18","ml19","ml20","ml21","ml22","ml23","ml24")
dtp$city <- paste0(dtp$COUNTY,dtp$TOWN,dtp$VILLAGE)
vilgTP <- vilg[vilg$COUNTY=="臺北市"|vilg$COUNTY=="新北市",]
dataTP <- dplyr::left_join(x=vilgTP,y=dtp,by=c("COUNTY","TOWN","V_ID"))

dtp2 <- dtp[]
## V2
## shinyapp
#----
## success
eat <- setDT(rio::import("餐館業（雙北）經緯度.xlsx"))
eat <- na.omit(eat)
dataTPP
library(shiny)
library(leaflet)
library(RColorBrewer)

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
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE),
                checkboxInput("circle", "餐館業分布", FALSE)
  )
)

server <- function(input, output, session) {
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colmar <- reactive({
    if(input$marry=="No"&input$college=="No"&input$cohort=="老化指數") return(dataTP$ml1)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="老化指數") return(dataTP$ml2)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="老化指數") return(dataTP$ml3)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="老化指數") return(dataTP$ml4)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_0到19歲人口比例") return(dataTP$ml5)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_0到19歲人口比例") return(dataTP$ml6)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_0到19歲人口比例") return(dataTP$ml7)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_0到19歲人口比例") return(dataTP$ml8)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_20到39歲人口比例") return(dataTP$ml9)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_20到39歲人口比例") return(dataTP$ml10)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_20到39歲人口比例") return(dataTP$ml11)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_20到39歲人口比例") return(dataTP$ml12)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_40到59歲人口比例") return(dataTP$ml13)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_40到59歲人口比例") return(dataTP$ml14)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_40到59歲人口比例") return(dataTP$ml15)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_40到59歲人口比例") return(dataTP$ml16)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_60到79歲人口比例") return(dataTP$ml17)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_60到79歲人口比例") return(dataTP$ml18)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_60到79歲人口比例") return(dataTP$ml19)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_60到79歲人口比例") return(dataTP$ml20)
    
    if(input$marry=="No"&input$college=="No"&input$cohort=="世代_80歲以上人口比例") return(dataTP$ml21)
    if(input$marry=="No"&input$college=="Yes"&input$cohort=="世代_80歲以上人口比例") return(dataTP$ml22)
    if(input$marry=="Yes"&input$college=="Yes"&input$cohort=="世代_80歲以上人口比例") return(dataTP$ml23)
    if(input$marry=="Yes"&input$college=="No"&input$cohort=="世代_80歲以上人口比例") return(dataTP$ml24)
    
  })
  
  colorpal <- reactive({
    colorFactor(input$colors, colmar())
    
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
      "<strong>%s</strong><br/>%s:%g<br/>%s:%g<br/>%s:%g<br/>%s:%g<br/>%s:%g",
      dataTP$city,"人口總數", dataTP$人口總數, "性別比",dataTP$性別比,"所得中位數",dataTP$所得中位數,"已婚有偶人口之比例",dataTP$已婚有偶人口之比例,"擁有大學以上學歷人口比例",dataTP$擁有大學以上學歷人口比例
    ) %>% lapply(htmltools::HTML)
    
  })
  observe({
    pal <- colorpal()
    leafletProxy("map", data = dataTP)%>% addPolygons(
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
  observe({ 
    proxy <- leafletProxy("map", data = eat)
    
    proxy %>% clearControls()
    if (input$circle){
      proxy %>% addCircles(~Response_X, ~Response_Y, radius=1.5, layerId=~id,
                           stroke=FALSE, fillOpacity=0.4)
    }
  })
}

shinyApp(ui, server)


