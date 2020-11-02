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

#get rid of bad point
oak.dat.temp <- oak.dat[-c(253), ]
temp <- prcomp(oak.dat.temp[ , oak.vars], scale = T)

#Assign values for trees
oak.dat.temp$newTrees <- NA
for(sites in unique(oak.dat.temp$site)) {
  trees <- unique(oak.dat.temp$tree[which(oak.dat.temp$site == sites)])
  for(i in 1:length(trees)) {
    oak.dat.temp$newTrees[which(oak.dat.temp$tree == trees[i])] <- LETTERS[i]
  }
}

oak.dat.temp$newTrees <- as.factor(oak.dat.temp$newTrees)

if(!exists('temp2')) temp2 <- metaMDS(temp$x, 'euclidean')
#plot(temp2$points)

#Plot ellipses
colors <- c("red", "orange", "yellow", "green", "blue", "purple", "brown", "deeppink", "olivedrab", "magenta", "black", "skyblue")

names(colors) <- c("Assiniboine Forest", "Bur Oak Woods", "Buttin Rock Access", "Cherokee Park Trail", "Mohawk Park", "Morton Arboretum", "Prairie Moon Nursery", "Red Rock Canyon State Park", "Spruce Woods Provincial Park", "Tallgrass Prairie Preserve", "UMN Campus", "Whiteshell Provincial Park")

pdf('../out/Fig4.Ordination.pdf', 9, 5.5)

plot(temp2$points, pch = as.numeric(oak.dat.temp$newTrees) + 14, col = colors[as.factor(oak.dat.temp$site)])

ordiellipse(temp2$points, oak.dat.temp$site, col = colors, kind = "ehull", label = FALSE)

legend(-7.5, 10, legend = unique(oak.dat.temp$site), pch = 19, col = colors[as.factor(unique(oak.dat.temp$site))], bty = 'n')

dev.off()
