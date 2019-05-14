library(leaflet)
library(sf)
library(dplyr)
library(data.table)


## 引進村里地圖
village <- st_read(dsn="村里.shp",options="ENCODING=BIG-5",stringsAsFactors=FALSE,crs=3826)
village <- st_transform(village,crs = 4326)
village_tp <- village%>%filter(COUNTY%in%c("臺北市","新北市"))

## 機器學習結果
ml <- fread("雙北人口結構加機器學習資料.csv", encoding="UTF-8")
ml[,COUNTY_ID:= as.character(COUNTY_ID)][,TOWN_ID:=as.character(TOWN_ID)]

## 合併地圖與模型
data <- dplyr::left_join(x=village_tp,y=ml,by=c("COUNTY","TOWN","COUNTY_ID","TOWN_ID","V_ID"))


#ins <- c(0, 100, 150, 200,300, 500, Inf)
#pal <- colorBin("YlOrRd", domain = data$A65_A0A14_RAT, bins = bins)

wardpal <- colorFactor("YlOrRd", data$ml1)




## 面量圖
m<-leaflet(data) %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    setView(
      lng = 121.537964 , lat = 25.039485, zoom = 12
    ) %>%
    addPolygons(
      fillColor = ~wardpal(ml1),
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
        bringToFront = TRUE))


m %>% addLegend(pal = wardpal, values = ~ml1, opacity = 0.7, title = NULL,
                position = "bottomright")



