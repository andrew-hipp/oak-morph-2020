#MDS
library(vegan)
oak.mds <- lapply(1:7, function(x) {
  metaMDS(scale(oak.dat[oak.vars]), k = x, distance = "euclidean", wascores = FALSE, autotransform = FALSE, noshare = FALSE)
}
)

oak.dat$mds1 <- oak.mds[[3]]$points[, 1]
oak.dat$mds2 <- oak.mds[[3]]$points[, 2]
oak.dat$mds3 <- oak.mds[[3]]$points[, 3]

oak.lm.site <- lm(mds1 ~ site, data = oak.dat)
oak.lm.tree <- lm(mds1 ~ tree, data = oak.dat)
oak.lm.site.tree <- lm(mds1 ~ site + tree, data = oak.dat)

oak.lm.2 <- lm(mds1 ~ lat, data = oak.dat)

