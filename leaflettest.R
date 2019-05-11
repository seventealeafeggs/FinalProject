getwd()
library(sf)
library(data.table)
library(tidyverse)
data[,"所得中位數"]->a
data%>% group_by(COUNTY) %>% summarise(mean(所得中位數))
data[]
da <- setDT(data)
da[,.(aa=mean(所得中位數)),by=.(COUNTY)]
Sys.setlocale(category="LC_ALL",locale="zh_TW.UTF-8")
#data
town <- st_read(dsn="鄉鎮市區.shp",options="ENCODING=BIG-5",stringsAsFactors=FALSE,crs=3826)
town <- st_transform(town,crs = 4326)
vote <- setDT(rio::import("perfectdata.csv"))
s <- fread("perfectdata.csv")
data <- dplyr::left_join(x=town,y=vote,by=c("COUNTY","TOWN"))

County<-st_cast(st_union(x=summarise(group_by(.data=tvdata,COUNTY,COUNTY_ID)),
                         by_feature = TRUE))
data %>% str()
data$city <- paste0(data$COUNTY,data$TOWN)

### leaflet
m <- leaflet(data) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('pk.eyJ1Ijoid2lsbDY3NjciLCJhIjoiY2p2MHZiZDMxMDV0cjN5cWp1ZWh6azh2cCJ9.Fst54QIIFhDrEMctpjkI8Q')))

bins <- c(0, 40, 50, 60, 70, 90, 100,120, Inf)
pal <- colorBin("YlOrRd", domain = data$所得中位數, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>所得中位數:%g",
  data$city, data$所得中位數
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~pal(所得中位數),
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
m
m %>% addLegend(pal = pal, values = ~所得中位數, opacity = 0.7, title = NULL,
                position = "bottomright")