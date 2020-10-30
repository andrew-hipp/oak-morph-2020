library(ggplot2)
summary(lm(Area ~ lat , data = oak.means))

pdf('Fig 3a. area.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=Area))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = Area-Area.se, ymax = Area+Area.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Leaf area (mm2)')
p <- p + annotate("text", x = 45.5, y = 12500,
                  label = "p = 0.014, r2 =  0.47",
                  hjust = 0)
plot(p)
dev.off()


summary(lm(bladeL ~ lat , data = oak.means))

pdf('Fig 3b. bladeL.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=bladeL))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = bladeL-bladeL.se, ymax = bladeL+bladeL.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Leaf blade length (mm)')
p <- p + annotate("text", x = 45.5, y = 200,
                  label = "p = 0.012, r2 =  0.49",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(bladeLtoWidestPoint ~ lat , data = oak.means))

pdf('Fig 3c. bladeLtoWidestPoint.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=bladeLtoWidestPoint))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = bladeLtoWidestPoint-bladeLtoWidestPoint.se, ymax = bladeLtoWidestPoint+bladeLtoWidestPoint.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Length of blade from base to widest point (mm)')
p <- p + annotate("text", x = 45.5, y = 145,
                  label = "p = 0.011, r2 =  0.49",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(TotalL.PL.BL ~ lat , data = oak.means))

pdf('Fig 3d. TotalL.PL.BL.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=TotalL.PL.BL))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = TotalL.PL.BL-TotalL.PL.BL.se, ymax = TotalL.PL.BL+TotalL.PL.BL.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Total length (blade length + petiole length) (mm)')
p <- p + annotate("text", x = 45.5, y = 215,
                  label = "p = 0.019, r2 =  0.44",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(Area.Mass ~ lat , data = oak.means))

pdf('Fig 3e. SLA.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=Area.Mass))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = Area.Mass-Area.Mass.se, ymax = Area.Mass+Area.Mass.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Specific leaf area')
p <- p + annotate("text", x = 43, y = 14000,
                  label = "p = 0.015, r2 =  0.47",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(petioleW ~ lat , data = oak.means))

pdf('Fig 3f. petioleW.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=petioleW))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = petioleW-petioleW.se, ymax = petioleW+petioleW.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Petiole width (mm)')
p <- p + annotate("text", x = 43, y = 1.8,
                  label = "p = < 0.001, r2 =  0.69",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(bladeW ~ lat , data = oak.means))

pdf('Fig 3g. bladeW.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=bladeW))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = bladeW-bladeW.se, ymax = bladeW+bladeW.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Leaf blade width (mm)')
p <- p + annotate("text", x = 45.3, y = 120,
                  label = "p = 0.014, r2 =  0.47",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(bladeBaseAngle ~ lat , data = oak.means))

pdf('Fig 3h. bladeBaseAngle.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=bladeBaseAngle))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = bladeBaseAngle-bladeBaseAngle.se, ymax = bladeBaseAngle+bladeBaseAngle.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Blade base angle (degrees)')
p <- p + annotate("text", x = 45, y = 100,
                  label = "p = 0.15, r2 =  0.19",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(sinusNextL ~ lat , data = oak.means))

pdf('Fig 3i. sinusNextL.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=sinusNextL))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = sinusNextL-sinusNextL.se, ymax = sinusNextL+sinusNextL.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Width of blade between sinuses just above the deepest pair (mm)')
p <- p + annotate("text", x = 45.3, y = 80,
                  label = "p = 0.49, r2 =  0.05",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(PL.TL ~ lat , data = oak.means))

pdf('Fig 3j. PL.TL.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=PL.TL))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = PL.TL-PL.TL.se, ymax = PL.TL+PL.TL.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Petiole Length / Total Length')
p <- p + annotate("text", x = 45.3, y = 0.14,
                  label = "p = 0.94, r2 =   0.0005",
                  hjust = 0)
plot(p)
dev.off()


summary(lm(SinusRatio ~ lat , data = oak.means))

pdf('Fig 3k. SinusRatio.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=SinusRatio))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = SinusRatio-SinusRatio.se, ymax = SinusRatio+SinusRatio.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Sinus ratio')
p <- p + annotate("text", x = 39, y = 0.9,
                  label = "p = 0.42, r2 =   0.065",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(BL.BW ~ lat , data = oak.means))

pdf('Fig 3l. BL.BW.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=BL.BW))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = BL.BW-BL.BW.se, ymax = BL.BW+BL.BW.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Blade length / Blade width')
p <- p + annotate("text", x = 42, y = 1.79,
                  label = "p = 0.31, r2 =   0.10",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(BL.BLWP ~ lat , data = oak.means))

pdf('Fig 3m. BL.BLWP.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=BL.BLWP))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = BL.BLWP-BL.BLWP.se, ymax = BL.BLWP+BL.BLWP.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Blade length / Blade length to widest point')
p <- p + annotate("text", x = 44, y = 1.7,
                  label = "p = 0.83, r2 = 0.0046",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(petioleL ~ lat , data = oak.means))

pdf('Fig 3n. petioleL.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=petioleL))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = petioleL-petioleL.se, ymax = petioleL+petioleL.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Petiole length (mm)')
p <- p + annotate("text", x = 45, y = 30,
                  label = "p =  0.26, r2 = 0.12",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(sinusMinL ~ lat , data = oak.means))

pdf('Fig 3o. petioleW.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=sinusMinL))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = sinusMinL-sinusMinL.se, ymax = sinusMinL+sinusMinL.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Width of blade between deepest pair of sinuses (mm)')
p <- p + annotate("text", x = 44, y = 40,
                  label = "p =  0.21, r2 =  0.15",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(sinus.v.width ~ lat , data = oak.means))

pdf('Fig 3p. sinus.v.width.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=sinus.v.width))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = sinus.v.width-sinus.v.width.se, ymax = sinus.v.width+sinus.v.width.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'SinusMinL/BladeL (mm)')
p <- p + annotate("text", x = 44, y = 0.5,
                  label = "p =  0.024, r2 =  0.41",
                  hjust = 0)
plot(p)
dev.off()

summary(lm(sinus.v.width ~ lat , data = oak.means[-c(9), ]))

pdf('Fig 3q. sinus.v.width (no spruce woods).lat.pdf', 8,8)
p <- ggplot(oak.means.se[-c(9), ], aes(x=lat, y=sinus.v.width))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = sinus.v.width-sinus.v.width.se, ymax = sinus.v.width+sinus.v.width.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Leaf blade width at narrowest point / total width')
p <- p + annotate("text", x = 37, y = .37,
                  label = "p = 0.054, r2 = 0.35",
                  hjust = 0)
print(p)
dev.off()

#CLIMATE REGRESSIONS
summary(lm(bio1 ~ lat , data = oak.means))

pdf('Fig 4a. MeanAnnualTemp.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=bio1))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = bio1-bio1.se, ymax = bio1+bio1.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Mean annual temperature')
p <- p + annotate("text", x = 45, y = 150,
                  label = "p =  5.505e-12, r2 = 0.9926",
                  hjust = 0)
print(p)
dev.off()

summary(lm(bio12 ~ lat , data = oak.means))

pdf('Fig 4b. MeanAnnualPrecip.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=bio12))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = bio12-bio12.se, ymax = bio12+bio12.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Mean annual precipitation')
p <- p + annotate("text", x = 45, y = 1000,
                  label = "p =  0.00096, r2 = 0.68",
                  hjust = 0)
print(p)
dev.off()

summary(lm(bio4 ~ lat, data = oak.means))

pdf('Fig 4c. TempSeasonalaity.lat.pdf', 8,8)
p <- ggplot(oak.means.se, aes(x=lat, y=bio4))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = bio4-bio4.se, ymax = bio4+bio4.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Temperature seasonality')
p <- p + annotate("text", x = 45, y = 10000,
                  label = "p =  7.894e-09, r2 = 0.9684",
                  hjust = 0)
print(p)
dev.off()


##regressions for relationship between variances and lat 

summary(lm(bladeL ~ lat , data = oak.var.se))

pdf('Fig 5a. bladeL.var.lat.pdf', 8,8)
p <- ggplot(oak.var.se, aes(x=lat, y=bladeL))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = bladeL-bladeL.se, ymax = bladeL+bladeL.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'blade length variance')
p <- p + annotate("text", x = 45, y = 800,
                  label = "p =  0.001687, r2 = 0.6438",
                  hjust = 0)
print(p)
dev.off()

summary(lm(Area ~ lat , data = oak.var.se))

pdf('Fig 5b. Area.var.lat.pdf', 8,8)
p <- ggplot(oak.var.se, aes(x=lat, y=Area))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = Area-Area.se, ymax = Area+Area.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'Area variance')
p <- p + annotate("text", x = 45, y = 1.5e+07,
                  label = "p =  0.05939, r2 = 0.3114",
                  hjust = 0)
print(p)
dev.off()

summary(lm(Area.Mass ~ lat , data = oak.var.se))

pdf('Fig 5c. SLA.var.lat.pdf', 8,8)
p <- ggplot(oak.var.se, aes(x=lat, y=Area.Mass))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p <- p + geom_errorbar(aes(ymin = Area.Mass-Area.Mass.se, ymax = Area.Mass+Area.Mass.se),
                       width = 0.2)
p <- p + labs(x = 'Latitude (degrees)',
              y = 'SLA variance')
p <- p + annotate("text", x = 45, y = 8.0e+06,
                  label = "p =  0.09058, r2 = 0.2597",
                  hjust = 0)
print(p)
dev.off()










