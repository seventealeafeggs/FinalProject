library(sf)
library(data.table)
library(tidyverse)
Sys.setlocale(category="LC_ALL",locale="zh_TW.UTF-8")
vilg <- st_read(dsn="村里.shp",options="ENCODING=BIG-5",stringsAsFactors=FALSE,crs=3826)
vilg <- st_transform(vilg,crs = 4326)
pop <- setDT(rio::import("STAT_TP.xlsx"))
pop <- load("STAT_TP.rdata")
pop <- STAT_TP
tmplist <- c(1:1488)
pop <-data.table(cbind(tmplist,pop))
str(pop)
popnew <- pop[,c(2,3,4,5,6,7,9,13,14,15,19,21,23,29:34)]
dtp <- popnew 

dtp %>% str()
dtp$P_CNT <- as.numeric(dtp$P_CNT)
dtp$M_F_RAT <- as.numeric(dtp$M_F_RAT)
dtp$P_H_CNT <- as.numeric(dtp$P_H_CNT)
dtp$P_DEN <- as.numeric(dtp$P_DEN)
dtp$A65_A0A14_RAT <- as.numeric(dtp$A65_A0A14_RAT)
dtp$FLD04 <- as.numeric(dtp$FLD04)
dtp %>% str()
## new version
###ml1 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT 
#ml1
pmatrix <- scale(dtp[,c(8:10,11,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml1 <- groups
###ml2 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT COLLEGE_RATE

#ml2
pmatrix <- scale(dtp[,c(8:10,11,12,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml2 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT MARRIAGED_RATE COLLEGE_RATE

#ml3
pmatrix <- scale(dtp[,c(8:10,11,12,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml3 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT    P_DEN A65_A0A14_RAT MARRIAGED_RATE

#ml4
pmatrix <- scale(dtp[,c(8:10,11,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml4 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A0_A19_COUNT_RATE 

#ml5
pmatrix <- scale(dtp[,c(8:10,14,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml5 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A0_A19_COUNT_RATE COLLEGE_RATE

#ml6
pmatrix <- scale(dtp[,c(8:10,14,12,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml6 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A0_A19_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE

#ml7
pmatrix <- scale(dtp[,c(8:10,14,12,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml7 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A0_A19_COUNT_RATE MARRIAGED_RATE

#ml8
pmatrix <- scale(dtp[,c(8:10,14,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml8 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE 

pmatrix <- scale(dtp[,c(8:10,15,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml9 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE COLLEGE_RATE

pmatrix <- scale(dtp[,c(8:10,15,12,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml10 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATET MARRIAGED_RATE COLLEGE_RATE

pmatrix <- scale(dtp[,c(8:10,15,12,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml11 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A20_A39_COUNT_RATE MARRIAGED_RATE

pmatrix <- scale(dtp[,c(8:10,15,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml12 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A40_A59_COUNT_RATE 

pmatrix <- scale(dtp[,c(8:10,16,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml13 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A40_A59_COUNT_RATE COLLEGE_RATE

pmatrix <- scale(dtp[,c(8:10,16,12,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml14 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A40_A59_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE

pmatrix <- scale(dtp[,c(8:10,16,12,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml15 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A40_A59_COUNT_RATE MARRIAGED_RATE

pmatrix <- scale(dtp[,c(8:10,16,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml16 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE 

pmatrix <- scale(dtp[,c(8:10,17,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml17 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE COLLEGE_RATE

pmatrix <- scale(dtp[,c(8:10,17,12,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml18 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE

pmatrix <- scale(dtp[,c(8:10,17,12,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml19 <- groups

###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A60_A79_COUNT_RATE MARRIAGED_RATE

pmatrix <- scale(dtp[,c(8:10,17,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml20 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE 

pmatrix <- scale(dtp[,c(8:10,18,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml21 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT  P_DEN  A80up_COUNT_RATE COLLEGE_RATE

pmatrix <- scale(dtp[,c(8:10,18,12,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml22 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE MARRIAGED_RATE COLLEGE_RATE
pmatrix <- scale(dtp[,c(8:10,18,12,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml23 <- groups
###ml1 P_CNT M_F_RAT P_H_CNT   P_DEN A80up_COUNT_RATE MARRIAGED_RATE

pmatrix <- scale(dtp[,c(8:10,18,13,19)])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

dtp$ml24 <- groups

dtp
fwrite(dtp,"雙北人口結構加機器學習資料V2.csv")

