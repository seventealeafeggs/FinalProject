# WhyNut?
## 雲端連結：https://docs.google.com/document/d/1p1Qoa4kMCLNlWUsDZI9LsZA_mwJgrASBk1XvHskL-r8/edit?usp=sharing

## 目前資料集架構：
1. 一是人口
2. 二是產業類別
3. 三是商業條件
4. 加入時間軸資料（歷史資料，time series）
* 資料集架構設計目的 -- 1, 方便資料處理； 2, 利於設計可供user點選的選項

## 目前可能分工：
* UI UX設計流程
  * scrum 
  * google design sprint

* MAP呈現
  * google map api （Mymaps）X
  * plotdb（套疊上可能有問題）X
  * leaflet 
  
* 資料集架構
  * 人口
    * 將不同人口結構橫向合併，串成包含所有人口特性的資料
    * 每個年度一個Data，不做年度間的合併（方便運算）
    * 運算後吐出json讓leatlet讀取
  * 產業類別
    * 將不同年度產業類別合併，即縱向加上橫向合併（須加上座標）
  * 商業條件
    * 將不同年度商業條件合併，即縱向加上橫向合併（須加上坐標）

（在代入參數時盡量不要用迴圈的寫法，若真的要寫迴圈是否能用apply家族取代）

* 機器學習
  * 只放入人口結構（可能只有五個變項）
  * 先運算好再讓User點選，不要即時運算
  * 設置lower bound建造C5取3的規則，最少需要選3項
  * Method:
    * Hierarchical Clustering: hclust()
    * Partitional Clustering:  kmeans()

## "如何說服user某些變項"是重要的
* 設置Defult，在一開始就先點選某些選項
## *身分類別是重要的（e.g.教育程度、類別）

## 參考網站
* 英文
  * Using Leaflet with Shiny: https://rstudio.github.io/leaflet/shiny.html
  * 4 tricks of Leaflet and Shiny: https://www.r-bloggers.com/4-tricks-for-working-with-r-leaflet-and-shiny/
  * example of Leaflet and Shiny : https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
  * leaflet官方教學: https://leafletjs.com/examples.html
  * leaflet面量圖: https://leafletjs.com/examples/choropleth/
  * Shiny Theme: https://rstudio.github.io/shinythemes/
* 中文
  * R 語言資料互動視覺化-Leaflet、Shiny: https://reurl.cc/Ymlm4
  * GTW:  https://blog.gtwang.org/r/r-leaflet-interactive-map-package-tutorial/
  * R筆記–(9)分群分析(Clustering): http://rpubs.com/skydome20/R-Note9-Clustering
## leaflet研究進度：
* 目前已知leaflet運作原則：
  * 將資料整理成以下形式即可放入leaflet()裡面，進行資料顯示，圖片如下：
![Alt text](https://i.imgur.com/CVNOMAe.png)
  * 範例網站：https://rstudio.github.io/leaflet/shiny.html
  
  
