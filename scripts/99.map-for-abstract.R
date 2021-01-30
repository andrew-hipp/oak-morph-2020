## added post-acceptance, 2021-01-30
## just to make a pretty map for the visual abstract

require(Momocs)
require(scales)
require(maps)
require(raster)
require(sp)

outline.do <- 13

source('https://raw.githubusercontent.com/andrew-hipp/mor-systematics/master/AAA.LAB/coo_plot2.R')

if(!exists('worldclim')) worldclim <- getData("worldclim",var="bio",res=10)

os <- oak.sites[!oak.sites$site %in%
                        c("Pearl King Savana", "Near Shawnee National Forest"),
                        ]
os.trees <- sapply(unique(os$site), function(x) paste(os$tree[os$site == x], collapse = "|"))
os <- os[!duplicated(os$site), ]
row.names(os) <- os$site
os$trees <- os.trees[os$site]
os.outlines <-
  lapply(os$trees, function(x) grep(x, names(outlines.out[[1]])))
names(os.outlines) <- os$site
##sampling range with bio1 (mean annual temperature)
pdf('../out/AbstractImage.lf13.map.pdf', 7.25,6, useDingbats = F)
plot(worldclim, y = 'bio1',
    xlim = c(-110, -60), ylim = c(25, 55),
    axes = T, box = T, legend = F,
    main = "",
    xlab = "",
    ylab = "",
    cex.axis = 0.7)
map('state', add = TRUE, col = 'black', lwd = 1, bty = 'n',
xlim = c(-110, -60), ylim = c(25, 55),)
for(i in os$site) {
  cp2(outlines.out[[1]][[os.outlines[[i]][outline.do]]],
      x = os[i, 'long'],
      y = os[i, 'lat'],
      scaleTo =3, col = 'black', border = 'white',
      plot.new = F,
      lwd = 0.5)
}
#points(oak.coord, pch = 21, col = 'black', bg = 'red', cex = 1)
dev.off()
