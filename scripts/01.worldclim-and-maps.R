library(raster)
library(sp)
library(maps)

if(!exists('worldclim')) worldclim <- getData("worldclim",var="bio",res=10)
oak.coord <- SpatialPoints(oak.dat[ , c('long', 'lat')])

##sampling range with bio1 (mean annual temperature)
pdf('../out/FIG1.map-bio1.pdf', 7.25,6, useDingbats = F)
plot(worldclim, y = 'bio1',
    xlim = c(-110, -60), ylim = c(25, 55),
    axes = T, box = T, legend = T,
    main = "",
    xlab = "Longitude (degrees)",
    ylab = "Latitude (degrees)",
    cex.axis = 0.7)
map('state', add = TRUE, col = 'black', lwd = 1, bty = 'n',
xlim = c(-110, -60), ylim = c(25, 55),)
points(oak.coord, pch = 21, col = 'black', bg = 'red', cex = 1)
dev.off()

## now let's get climatic data:
oak.clim <- extract(worldclim, oak.coord)

## and attach it to our dataframe:
oak.dat <- cbind(oak.dat, oak.clim)
