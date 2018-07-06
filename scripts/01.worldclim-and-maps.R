library(raster)
library(sp)
library(maps)

doState = FALSE

if(!exists('worldclim')) worldclim <- getData("worldclim",var="bio",res=10)
oak.coord <- SpatialPoints(oak.dat[ , c('long', 'lat')])

## whole sampling range.... you need to adjust xlim and ylim to limit yourself to the range you want to plot
pdf('Fig 1. Map of Sampling Sites.pdf', 5, 6)
map('world', c('usa', 'canada'), xlim = c(-141, -50), ylim = c(20, 55))
map('county', add = TRUE, col = 'gray', lwd = 0.25)
map('state', add = TRUE, col = 'black', lwd = 1)
map.axes(cex.axis=0.8)
points(oak.coord, pch = 21, col = 'black', bg = 'red', cex = 1)

dev.off()

##sampling range with bio1 (mean annual temperature)
pdf('Fig 1. Map of Sampling Sites.bio1.pdf', 6,5)
plot(worldclim, y = 'bio1', xlim = c(-150, -50), ylim = c(10, 55), main = "")
map('county', add = TRUE, col = "white", lwd = 0.5)
map('state', add = TRUE, col = 'black', lwd = 1)
points(oak.coord, pch = 21, col = 'black', bg = 'red', cex = 1)

dev.off()

## a single state example
if(doState) {
  map('county', 'oklahoma', lwd = 0.25, col = 'gray')
  map('state', 'oklahoma', add = T)
  points(oak.coord, pch = 21, col = 'black', bg = 'red', cex = 1)
}
## and you might want to do a single site

## now let's get climatic data:
oak.clim <- extract(worldclim, oak.coord)

## and attach it to our dataframe:
oak.dat <- cbind(oak.dat, oak.clim)
