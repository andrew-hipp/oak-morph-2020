library(raster)
library(sp)
library(maps)

doState = FALSE

if(!exists('worldclim')) worldclim <- getData("worldclim",var="bio",res=10)
oak.coord <- SpatialPoints(oak.dat[ , c('long', 'lat')])

##sampling range with bio1 (mean annual temperature)
pdf('../out/Fig-1.MAP.bio1.pdf', 7.25,6)
plot(worldclim, y = 'bio1',
    xlim = c(-110, -60), ylim = c(25, 55),
    axes = T, box = T, legend = T,
    main = "",
    xlab = "Longitude (degrees)",
    ylab = "Latitude (degrees)",
    cex.axis = 0.7)
#map('county', add = TRUE, col = "white", lwd = 0.5)
map('state', add = TRUE, col = 'black', lwd = 1, bty = 'n',
xlim = c(-110, -60), ylim = c(25, 55),)
points(oak.coord, pch = 21, col = 'black', bg = 'red', cex = 1)
# axis(1, cex = 0.5)
# axis(2, cex = 0.5)
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
