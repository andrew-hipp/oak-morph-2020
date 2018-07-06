#biplot
pdf('Biplot for all individuals.pdf', 15,8)

plot(Area.Mass ~ bladeL, data = oak.dat, col = as.numeric(as.factor(oak.dat$site)), pch = 19, cex = 1, xlab="bladeL (mm)", ylab="Area.Mass (mm2)")


#cex = (oak.dat$lat-34.4) / 10

legend(210, 22000, legend = unique(oak.dat$site), pch = 19,col = as.numeric(as.factor(unique(oak.dat$site))), bty = 'n')
       
 dev.off()


## biplots for individual sites
pdf('Fig 4. Biplots for four sites.pdf', 7.25,6.5)
par(mfrow=c(2,2))

plot(Area.Mass ~ bladeL, data = oak.dat[oak.dat$site == 'Mohawk Park', ], pch = 19, col = as.numeric(as.factor(oak.dat$tree[oak.dat$site == 'Mohawk Park'])), cex = 1.5, main="Mohawk Park", xlab="bladeL (mm)", ylab="Area.Mass (mm2)")

plot(Area.Mass ~ bladeL, data = oak.dat[oak.dat$site == 'Whiteshell Provincial Park', ], pch = 19, col = as.numeric(as.factor(oak.dat$tree[oak.dat$site == 'Whiteshell Provincial Park'])), cex = 1.5, main="Whiteshell Provincial Park", xlab="bladeL (mm)", ylab=("Area.Mass (mm2)"))

plot(Area.Mass ~ bladeL, data = oak.dat[oak.dat$site == 'Red Rock Canyon State Park', ], pch = 19, col = as.numeric(as.factor(oak.dat$tree[oak.dat$site == 'Red Rock Canyon State Park'])), cex = 1.5, main="Red Rock Canyon State Park", xlab="bladeL (mm)", ylab="Area.Mass (mm2)")

plot(Area.Mass ~ bladeL, data = oak.dat[oak.dat$site == 'UMN Campus', ], pch = 19, col = as.numeric(as.factor(oak.dat$tree[oak.dat$site == 'UMN Campus'])), cex = 1.5, main="UMN Campus", xlab="bladeL (mm)", ylab="Area.Mass (mm2)")

dev.off()