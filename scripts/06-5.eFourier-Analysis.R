##Normalizing outlines and eFourier analysis

# read data for morphometric analysis

library(Momocs)
library(ade4)
library(rlang)

setwd("~/Desktop/Bur Oak Morphology Project/ANALYSIS-v1/workspace")
outlines <- import_jpg(dir('../data/BWscans copy/', full = T))
outlines.out <- Out(outlines)

{r, label="Centering_and_scaling_of_leaves", cache=TRUE, echo=FALSE}
outlines.out$coo <- lapply(outlines.out$coo, coo_interpolate, n=200) 
outlines.out <- coo_center(outlines.out)
outlines.out$coo <- lapply(outlines.out$coo, FUN=function(x)(coo_scale(x, max(x[,2]/1000)))) 
outlines.out <- coo_scale(outlines.out, 1000) 


{r, label="Landmarking_of_leaves",echo=FALSE, cache=TRUE, message=FALSE, warning=FALSE,results='hide'}
ldks2 <- vector("list", length=length(outlines.out$coo))
names(ldks2) <- names(outlines.out$coo)
centroids <- coo_centpos(outlines.out) 
for (i in 1:length(outlines.out)) {
  ldk <- numeric(4)
  outlines.out$coo[[i]] -> x
  # lower middle
  p <- c(centroids[i,1], min(x[,2]))
  l <- apply(x, 1, function(y) sqrt(sum((p - y)^2)))
  ldk[1] <- which.min(l)
  # upper middle
  p <- c(centroids[i,1], max(x[,2]))
  l <- apply(x, 1, function(y) sqrt(sum((p - y)^2)))
  ldk[2] <- which.min(l)
  # middle left
  p <- c(min(x[,1]), centroids[i,2])
  l <- apply(x, 1, function(y) sqrt(sum((p - y)^2)))
  ldk[3] <- which.min(l)
  # middle right
  p <- c(max(x[,1]), centroids[i,2])
  l <- apply(x, 1, function(y) sqrt(sum((p - y)^2)))
  ldk[4] <- which.min(l)
  ldk -> ldks2[[i]]
}
outlines.out$ldk <- ldks2
outlines.out <- coo_slide(outlines.out, ldk=2) 
outlines.out <- fgProcrustes(outlines.out) 

outlines.F <- efourier(outlines.out, smooth.it = 0, norm = FALSE, start = FALSE)
outlines.D <- PCA(outlines.F, size = 1.5, nr = 5, pos = "xy")

pdf('eFourier Analysis.pdf', 5,5)

plot(outlines.D, labelspoints = TRUE, axisnames = TRUE)

dev.off()

#KRIEBEL REVIEW FEEDBACK
jpg_list <- list.files("outlines",pattern = ".jpg", full.names = T)
leaf_coords <- import_jpg(dir('..workspace/BWscans copy/', full = T)) 

#remove bad outlines
##leaf_coords <- leaf_coords[,-c(10, 29, 35, 60, 62, 82, 172, 208, 210, 220, 234, 238, 256, 276, 286, 287, 289,296)]
leaf_coords_1_50[[10]]<- NULL
leaf_coords_1_50[[29]]<- NULL
leaf_coords_1_50[[35]]<- NULL
leaf_coords_1_50[[33]]<- NULL
leaf_coords_51_100[[10]]<- NULL
leaf_coords_51_100[[12]]<- NULL
leaf_coords_51_100[[32]]<- NULL
leaf_coords_51_100[[33]]<- NULL
leaf_coords_51_100[[30]]<- NULL
leaf_coords_101_150[[22]]<- NULL
leaf_coords_151_200[[8]]<- NULL
leaf_coords_151_200[[9]]<- NULL
leaf_coords_151_200[[10]]<- NULL
leaf_coords_151_200[[20]]<- NULL
leaf_coords_151_200[[34]]<- NULL
leaf_coords_201_250[[6]]<- NULL
leaf_coords_201_250[[26]]<- NULL
leaf_coords_201_250[[36]]<- NULL
leaf_coords_201_250[[37]]<- NULL
leaf_coords_201_250[[39]]<- NULL
leaf_coords_201_250[[46]]<- NULL
leaf_coords_251_302[[6]]<- NULL
leaf_coords_251_302[[15]]<- NULL
leaf_coords_251_302[[24]]<- NULL
leaf_coords_251_302[[25]]<- NULL
leaf_coords_251_302[[47]]<- NULL




#landmarking leaves
leaf_coords_1_50 <- leaf_coords[1:50]
leaf_coords_1_50 <- Out(leaf_coords_1_50) 
leaf_coords_1_50 <- def_ldk(leaf_coords_1_50, 4)
saveRDS(leaf_coords_1_50, "leaf_coords_1_50_w_ldks.rds")

leaf_coords_51_100 <- leaf_coords[51:100]
leaf_coords_51_100 <- Out(leaf_coords_51_100) 
leaf_coords_51_100 <- def_ldk(leaf_coords_51_100, 4)
saveRDS(leaf_coords_51_100, "leaf_coords_51_100_w_ldks.rds")

leaf_coords_101_150 <- leaf_coords[101:150]
leaf_coords_101_150 <- Out(leaf_coords_101_150) 
leaf_coords_101_150 <- def_ldk(leaf_coords_101_150, 4)
saveRDS(leaf_coords_101_150, "leaf_coords_101_150_w_ldks.rds")

leaf_coords_151_200 <- leaf_coords[151:200]
leaf_coords_151_200 <- Out(leaf_coords_151_200) 
leaf_coords_151_200 <- def_ldk(leaf_coords_151_200, 4)
saveRDS(leaf_coords_151_200, "leaf_coords_151_200_w_ldks.rds")

leaf_coords_201_250 <- leaf_coords[201:250]
leaf_coords_201_250 <- Out(leaf_coords_201_250) 
leaf_coords_201_250 <- def_ldk(leaf_coords_201_250, 4)
saveRDS(leaf_coords_201_250, "leaf_coords_201_250_w_ldks.rds")

leaf_coords_251_302 <- leaf_coords[251:302]
leaf_coords_251_302 <- Out(leaf_coords_251_302) 
leaf_coords_251_302 <- def_ldk(leaf_coords_251_302, 4)
saveRDS(leaf_coords_251_302, "leaf_coords_251_302_w_ldks.rds")


leaf_coords_1_50 <- readRDS("leaf_coords_1_50_w_ldks.rds")
leaf_coords_51_100 <- readRDS("leaf_coords_51_100_w_ldks.rds")
leaf_coords_101_150 <- readRDS("leaf_coords_101_150_w_ldks.rds")
leaf_coords_151_200 <- readRDS("leaf_coords_151_200_w_ldks.rds")
leaf_coords_201_250 <- readRDS("leaf_coords_201_250_w_ldks.rds")
leaf_coords_251_302 <- readRDS("leaf_coords_251_302_w_ldks.rds")


coordinates<-c(leaf_coords_1_50$coo, leaf_coords_51_100$coo, leaf_coords_101_150$coo, leaf_coords_151_200$coo, leaf_coords_201_250$coo, leaf_coords_251_302$coo)
landmarks<-c(leaf_coords_1_50$ldk, leaf_coords_51_100$ldk, leaf_coords_101_150$ldk, leaf_coords_151_200$ldk, leaf_coords_201_250$ldk, leaf_coords_251_302$ldk)

leaf_coords <- Out(coordinates, ldk=landmarks)
saveRDS(leaf_coords,"leaves_4ldks_July_2020.rds")

leaf_coords <- coo_slide(leaf_coords, ldk=2)
leaf_coords <- fgProcrustes(leaf_coords)
leaf_coords_f <- efourier(leaf_coords, smooth.it = 0, norm = FALSE, start = FALSE)
leaf_coords_D <- PCA(leaf_coords_f)

pdf('eFourier Analysis1.pdf', 5,5)

plot(leaf_coords_D, labelspoints = TRUE, axisnames = TRUE)

dev.off()

##BAD OUTLINES REMOVED 
pdf('eFourier Analysis2.pdf', 5,5)

plot(leaf_coords_D, labelspoints = TRUE, axisnames = TRUE)

dev.off()



#PCA AXES vs. LAT
PCA.scores <- as.data.frame(outlines.D$x, use.names = TRUE)
oak.dat.PCA <- merge(oak.dat, PCA.scores)

oak.tree.means.PCA <- as.data.frame(t(sapply(split(oak.dat.PCA[c('lat', 'PC1', 'PC2', oak.vars, c('mds1', 'mds2', 'mds3'), paste('bio', 1:19, sep = ''))], oak.dat.PCA$tree), function(x) apply(x, 2, mean))))

oak.means.PCA <- as.data.frame(t(sapply(split(oak.tree.means.PCA, oak.dat.PCA$site[match(row.names(oak.tree.means.PCA), oak.dat.PCA$tree)]), function(x) apply(x, 2, mean))))

oak.se.PCA <- as.data.frame(t(sapply(split(oak.tree.means.PCA, oak.dat.PCA$site[match(row.names(oak.tree.means.PCA), oak.dat.PCA$tree)]), function(x) apply(x, 2, sd) / (sqrt(3)))))

names(oak.se.PCA) <- paste(names(oak.se.PCA), 'se', sep = '.')

oak.means.se.PCA <- cbind(oak.means.PCA, oak.se.PCA)

summary(lm(PC1 ~ lat, data = oak.means.se.PCA))

pdf('PC1.v.Lat', 8,8)
p <- ggplot(oak.means.se.PCA, aes(x=lat, y=PC1))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = PC1-PC1.se, ymax = PC1+PC1.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'PC1')
p <- p + annotate("text", x = 45.5, y = -3.1e-19,
                  label = "p = 0.7429, r2 =  0.01125",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(PC2 ~ lat, data = oak.means.se.PCA))

pdf('PC2.v.Lat', 8,8)
p <- ggplot(oak.means.se.PCA, aes(x=lat, y=PC2))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = PC2-PC2.se, ymax = PC2+PC2.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'PC1')
p <- p + annotate("text", x = 45.5, y = 9.39e-18,
                  label = "p = 0.2999, r2 =  0.1068",
                  hjust = 0)
plot(p)
dev.off()






