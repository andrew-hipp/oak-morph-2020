library(ggplot2)

# summary(lm(Area ~ bladeL , data = oak.means))

# p <- ggplot(oak.means.se, aes(x=bladeL, y=Area))
# p <- p + geom_point()
# p <- p + geom_smooth(method="lm")
# p <- p + geom_errorbar(aes(ymin = Area-Area.se, ymax = Area+Area.se),
#                        width = 0.2)
# p <- p + labs(x = 'bladeL (mm)',
#               y = 'Leaf area (mm2)')
# p <- p + annotate("text", x = 120, y = 12500,
#                   label = "p < 0.001, r2 =  0.9328",
#                   hjust = 0)

# print(p)

oak.means.scale <- scale(oak.means, center = TRUE, scale = TRUE)
oak.means.scale <- as.data.frame(oak.means.scale)

oak.means.scale.out <- list(
    bladeW=summary(lm(bladeW ~ lat + bladeL, data = oak.means.scale)),
  sinusMinL=summary(lm(sinusMinL ~ lat + bladeL, data = oak.means.scale)),
  sinusNextL=summary(lm(sinusNextL ~ lat + bladeL, data = oak.means.scale)),
  sinusNextL.noOutlier=summary(lm(sinusNextL ~ lat + bladeL, data = oak.means.scale[-c(9), ])),
  petioleL=summary(lm(petioleL ~ lat + bladeL, data = oak.means.scale)),
  petioleW=summary(lm(petioleW ~ lat + bladeL, data = oak.means.scale)),
  bladeLtoWidestPoint=summary(lm(bladeLtoWidestPoint ~ lat + bladeL, data = oak.means.scale)),
  bladeBaseAngle=summary(lm(bladeBaseAngle ~ lat + bladeL, data = oak.means.scale)),
  TotalL.PL.BL=summary(lm(TotalL.PL.BL ~ lat + bladeL, data = oak.means.scale)),
  Area=summary(lm(Area ~ lat + bladeL, data = oak.means.scale)),
  Area.Mass=summary(lm(Area.Mass ~ lat + bladeL, data = oak.means.scale)),
  PL.TL=summary(lm(PL.TL ~ lat + bladeL, data = oak.means.scale)),
  SinusRatio=summary(lm(SinusRatio ~ lat + bladeL, data = oak.means.scale)),
  BL.BW=summary(lm(BL.BW ~ lat + bladeL, data = oak.means.scale)),
  BL.BLWP=summary(lm(BL.BLWP ~ lat + bladeL, data = oak.means.scale)),
  sinus.v.width=summary(lm(sinus.v.width ~ lat + bladeL, data = oak.means.scale)),
  sinus.v.width.noOutlier=summary(lm(sinus.v.width ~ lat + bladeL, data = oak.means.scale[-c(9), ]))
)
