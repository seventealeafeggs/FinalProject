library(sf)
library(data.table)
library(tidyverse)
Sys.setlocale(category="LC_ALL",locale="zh_TW.UTF-8")
VILLAGE_MOI_1080509.shp
vilg <- st_read(dsn="村里.shp",options="ENCODING=BIG-5",stringsAsFactors=FALSE,crs=3826)
vilg <- st_transform(vilg,crs = 4326)
pop <- setDT(rio::import("STAT_2018.xlsx"))
popnew <- pop[,c(2,3,4,5,6,7,9,13,14,15,19,21,23,29:33)]
popnew %>% str()
pairs(popnew[,c(7:17)])

barplot(table(popnew[,7]))#r
barplot(table(popnew[,8]))

barplot(table(popnew[,9]))

barplot(table(popnew[,10]))

barplot(table(popnew[,11]))
barplot(table(popnew[,12]))
barplot(table(popnew[,13]))
barplot(table(popnew[,14]))
barplot(table(popnew[,15]))
barplot(table(popnew[,16]))
barplot(table(popnew[,17]))#l

cor<- cor(popnew[,c(7:17)],method = "pearson")
round(cor,2)
popnew %>% summary()
#MARRIAGED_RATE COLLEGE_RATE零相關
#年輕人和老年人高度相關
#平均每戶人數小於三，而雙北又更為低
#每戶人口數對判斷老年人比例是有意義的
popnew[P_H_CNT<2,.(COUNTY,TOWN,VILLAGE)]
dtp<-popnew[COUNTY == "臺北市"|COUNTY == "新北市",]
dtp[P_H_CNT<1.5,.(COUNTY,TOWN,VILLAGE)]
barplot(table(dtp[,9]))
mean(popnew[,9])
dtp %>% summary()
pop %>% summary()
plot(pop$P_H_CNT,pop$A65_A0A14_RAT)
abline(h=100)
cor<- cor(pop[,c(13:33)],method = "pearson")
round(cor,2)
###
dtp
###ml1 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT 
ml1 <- list(c(7:10,11))
#ml1
pmatrix <- scale(dtp[,c(7:10,11)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml1 <- groups
###ml2 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT COLLEGE_RATE
ml2 <- c(7:10,11,12)
#ml2
pmatrix <- scale(dtp[,c(7:10,11,12)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml2 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT MARRIAGED_RATE COLLEGE_RATE
ml3 <- c(7:10,11,12,13)
#ml3
pmatrix <- scale(dtp[,c(7:10,11,12,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml3 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT MARRIAGED_RATE
ml4 <- c(7:10,11,13)
ml3 <- c(7:10,11,12,13)
#ml4
pmatrix <- scale(dtp[,c(7:10,11,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml4 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A0_A19_COUNT_RATE 
ml5 <- c(7:10,14)
#ml5
pmatrix <- scale(dtp[,c(7:10,14)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml5 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A0_A19_COUNT_RATE COLLEGE_RATE
ml6 <- c(7:10,14,12)
#ml6
pmatrix <- scale(dtp[,c(7:10,14,12)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml6 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A0_A19_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
ml7 <- c(7:10,14,12,13)
#ml7
pmatrix <- scale(dtp[,c(7:10,14,12,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml7 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A0_A19_COUNT_RATE MARRIAGED_RATE
ml8 <- c(7:10,14,13)
#ml8
pmatrix <- scale(dtp[,c(7:10,14,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml8 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE 
ml9 <- c(7:10,15)
pmatrix <- scale(dtp[,c(7:10,15)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml9 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE COLLEGE_RATE
ml10 <- c(7:10,15,12)

pmatrix <- scale(dtp[,c(7:10,15,12)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml10 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATET MARRIAGED_RATE COLLEGE_RATE
ml11 <- c(7:10,15,12,13)

pmatrix <- scale(dtp[,c(7:10,15,12,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml11 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE MARRIAGED_RATE
ml12 <- c(7:10,15,13)

pmatrix <- scale(dtp[,c(7:10,15,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml12 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A40_A59_COUNT_RATE 
ml13 <- c(7:10,16)
pmatrix <- scale(dtp[,c(7:10,16)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml13 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A40_A59_COUNT_RATE COLLEGE_RATE
ml14 <- c(7:10,16,12)
pmatrix <- scale(dtp[,c(7:10,16,12)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml14 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A40_A59_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
ml15 <- c(7:10,16,12,13)
pmatrix <- scale(dtp[,c(7:10,16,12,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml15 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A40_A59_COUNT_RATE MARRIAGED_RATE
ml16 <- c(7:10,16,13)
pmatrix <- scale(dtp[,c(7:10,16,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml16 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE 
ml17 <- c(7:10,17)

pmatrix <- scale(dtp[,c(7:10,17)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml17 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE COLLEGE_RATE
ml18 <- c(7:10,17,12)
pmatrix <- scale(dtp[,c(7:10,17,12)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml18 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
ml19 <- c(7:10,17,12,13)
pmatrix <- scale(dtp[,c(7:10,17,12,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml19 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE MARRIAGED_RATE
ml20 <- c(7:10,17,13)
pmatrix <- scale(dtp[,c(7:10,17,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml20 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE 
ml21 <- c(7:10,18)

pmatrix <- scale(dtp[,c(7:10,18)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml21 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A80up_COUNT_RATE COLLEGE_RATE
ml22 <- c(7:10,18,12)

pmatrix <- scale(dtp[,c(7:10,18,12)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml22 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
ml23 <- c(7:10,18,12,13)


pmatrix <- scale(dtp[,c(7:10,18,12,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml23 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE MARRIAGED_RATE
ml24 <- c(7:10,18,13)


pmatrix <- scale(dtp[,c(7:10,18,13)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml24 <- groups

dtp
fwrite(dtp,"雙北人口結構加機器學習資料.csv")
