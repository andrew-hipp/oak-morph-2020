require(vegan)

oak.dat <- read.delim('../data/Leaf Trait Measurements - data.tsv', as.is = TRUE)

oak.dat <- oak.dat[! (oak.dat$use=="F"),]

oak.vars <- c("bladeL", "bladeW", "sinusMinL", "sinusNextL", "petioleL", "petioleW","bladeLtoWidestPoint", "Area", "Mass", "Area.Mass", "bladeBaseAngle", "TotalL.PL.BL", "PL.TL", "SinusRatio", "BL.BW", "PL.PW", "BL.BW.over.PL.PW", "BL.BLWP")


oak.dat$sinus.v.width <- oak.dat$sinusMinL / oak.dat$bladeW

oak.vars <- c(oak.vars, 'sinus.v.width')
for(i in oak.vars) oak.dat[[i]] <- as.numeric(oak.dat[[i]])

