library(ggplot2)
require(grid)
require(gridExtra)

# summary(lm(Area ~ lat , data = oak.means))

p1 <- ggplot(oak.means.se, aes(x=lat, y=Area))
p1 <- p1 + geom_point()
p1 <- p1 + geom_smooth(method="lm")
p1 <- p1 + geom_errorbar(aes(ymin = Area-Area.se, ymax = Area+Area.se),
                       width = 0.2)
p1 <- p1 + labs(x = 'Latitude (degrees)',
              y = 'Area (mm2)')
p1 <- p1 + annotate("text", x = 35, y = 4500,
                  label = "p = 0.014, r2 =  0.47",
                  hjust = 0)

p1 <- arrangeGrob(p1, top = textGrob("(a)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(bladeL ~ lat , data = oak.means))

p2 <- ggplot(oak.means.se, aes(x=lat, y=bladeL))
p2 <- p2 + geom_point()
p2 <- p2 + geom_smooth(method="lm")
p2 <- p2 + geom_errorbar(aes(ymin = bladeL-bladeL.se, ymax = bladeL+bladeL.se),
                       width = 0.2)
p2 <- p2 + labs(x = 'Latitude (degrees)',
              y = 'bladeL (mm)')
p2 <- p2 + annotate("text", x = 35, y = 120,
                  label = "p = 0.012, r2 =  0.49",
                  hjust = 0)

p2 <- arrangeGrob(p2, top = textGrob("(b)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(bladeLtoWidestPoint ~ lat , data = oak.means))

p3 <- ggplot(oak.means.se, aes(x=lat, y=bladeLtoWidestPoint))
p3 <- p3 + geom_point()
p3 <- p3 + geom_smooth(method="lm")
p3 <- p3 + geom_errorbar(aes(ymin = bladeLtoWidestPoint-bladeLtoWidestPoint.se, ymax = bladeLtoWidestPoint+bladeLtoWidestPoint.se),
                       width = 0.2)
p3 <- p3 + labs(x = 'Latitude (degrees)',
              y = 'bladeLtoWidestPoint (mm)')
p3 <- p3 + annotate("text", x = 35, y = 80,
                  label = "p = 0.011, r2 =  0.49",
                  hjust = 0)

p3 <- arrangeGrob(p3, top = textGrob("(c)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(TotalL.PL.BL ~ lat , data = oak.means))

p4 <- ggplot(oak.means.se, aes(x=lat, y=TotalL.PL.BL))
p4 <- p4 + geom_point()
p4 <- p4 + geom_smooth(method="lm")
p4 <- p4 + geom_errorbar(aes(ymin = TotalL.PL.BL-TotalL.PL.BL.se, ymax = TotalL.PL.BL+TotalL.PL.BL.se),
                       width = 0.2)
p4 <- p4 + labs(x = 'Latitude (degrees)',
              y = 'Total length (mm)')
p4 <- p4 + annotate("text", x = 35, y = 140,
                  label = "p = 0.019, r2 =  0.44",
                  hjust = 0)

p4 <- arrangeGrob(p4, top = textGrob("(d)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(Area.Mass ~ lat , data = oak.means))

p5 <- ggplot(oak.means.se, aes(x=lat, y=Area.Mass))
p5 <- p5 + geom_point()
p5 <- p5 + geom_smooth(method="lm")
p5 <- p5 + geom_errorbar(aes(ymin = Area.Mass-Area.Mass.se, ymax = Area.Mass+Area.Mass.se),
                       width = 0.2)
p5 <- p5 + labs(x = 'Latitude (degrees)',
              y = 'SLA')
p5 <- p5 + annotate("text", x = 35, y = 14000,
                  label = "p = 0.015, r2 =  0.47",
                  hjust = 0)

p5 <- arrangeGrob(p5, top = textGrob("(e)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(petioleW ~ lat , data = oak.means))

p6 <- ggplot(oak.means.se, aes(x=lat, y=petioleW))
p6 <- p6 + geom_point()
p6 <- p6 + geom_smooth(method="lm")
p6 <- p6 + geom_errorbar(aes(ymin = petioleW-petioleW.se, ymax = petioleW+petioleW.se),
                       width = 0.2)
p6 <- p6 + labs(x = 'Latitude (degrees)',
              y = 'petioleW (mm)')
p6 <- p6 + annotate("text", x = 35, y = 1,
                  label = "p = < 0.001, r2 =  0.69",
                  hjust = 0)

p6 <- arrangeGrob(p6, top = textGrob("(f)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(bladeW ~ lat , data = oak.means))

p7 <- ggplot(oak.means.se, aes(x=lat, y=bladeW))
p7 <- p7 + geom_point()
p7 <- p7 + geom_smooth(method="lm")
p7 <- p7 + geom_errorbar(aes(ymin = bladeW-bladeW.se, ymax = bladeW+bladeW.se),
                       width = 0.2)
p7 <- p7 + labs(x = 'Latitude (degrees)',
              y = 'bladeW (mm)')
p7 <- p7 + annotate("text", x = 35, y = 70,
                  label = "p = 0.014, r2 =  0.47",
                  hjust = 0)

p7 <- arrangeGrob(p7, top = textGrob("(g)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(bladeBaseAngle ~ lat , data = oak.means))

p8 <- ggplot(oak.means.se, aes(x=lat, y=bladeBaseAngle))
p8 <- p8 + geom_point()
p8 <- p8 + geom_smooth(method="lm")
p8 <- p8 + geom_errorbar(aes(ymin = bladeBaseAngle-bladeBaseAngle.se, ymax = bladeBaseAngle+bladeBaseAngle.se),
                       width = 0.2)
p8 <- p8 + labs(x = 'Latitude (degrees)',
              y = 'bladeBaseAngle (degrees)')
p8 <- p8 + annotate("text", x = 40, y = 65,
                  label = "p = 0.15, r2 =  0.19",
                  hjust = 0)

p8 <- arrangeGrob(p8, top = textGrob("(h)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(sinusNextL ~ lat , data = oak.means))

p9 <- ggplot(oak.means.se, aes(x=lat, y=sinusNextL))
p9 <- p9 + geom_point()
p9 <- p9 + geom_smooth(method="lm")
p9 <- p9 + geom_errorbar(aes(ymin = sinusNextL-sinusNextL.se, ymax = sinusNextL+sinusNextL.se),
                       width = 0.2)
p9 <- p9 + labs(x = 'Latitude (degrees)',
              y = 'sinusNextL (mm)')
p9 <- p9 + annotate("text", x = 40, y = 30,
                  label = "p = 0.49, r2 =  0.05",
                  hjust = 0)

p9 <- arrangeGrob(p9, top = textGrob("(i)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(PL.TL ~ lat , data = oak.means))

p10 <- ggplot(oak.means.se, aes(x=lat, y=PL.TL))
p10 <- p10 + geom_point()
p10 <- p10 + geom_smooth(method="lm")
p10 <- p10 + geom_errorbar(aes(ymin = PL.TL-PL.TL.se, ymax = PL.TL+PL.TL.se),
                       width = 0.2)
p10 <- p10 + labs(x = 'Latitude (degrees)',
              y = 'Petiole Length / Total Length')
p10 <- p10 + annotate("text", x = 40, y = 0.07,
                  label = "p = 0.94, r2 =   0.0005",
                  hjust = 0)

p10 <- arrangeGrob(p10, top = textGrob("(j)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(SinusRatio ~ lat , data = oak.means))

p11 <- ggplot(oak.means.se, aes(x=lat, y=SinusRatio))
p11 <- p11 + geom_point()
p11 <- p11 + geom_smooth(method="lm")
p11 <- p11 + geom_errorbar(aes(ymin = SinusRatio-SinusRatio.se, ymax = SinusRatio+SinusRatio.se),
                       width = 0.2)
p11 <- p11 + labs(x = 'Latitude (degrees)',
              y = 'Sinus ratio')
p11 <- p11 + annotate("text", x = 36, y = 0.9,
                  label = "p = 0.42, r2 =   0.065",
                  hjust = 0)

p11 <- arrangeGrob(p11, top = textGrob("(k)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(BL.BW ~ lat , data = oak.means))

p12 <- ggplot(oak.means.se, aes(x=lat, y=BL.BW))
p12 <- p12 + geom_point()
p12 <- p12 + geom_smooth(method="lm")
p12 <- p12 + geom_errorbar(aes(ymin = BL.BW-BL.BW.se, ymax = BL.BW+BL.BW.se),
                       width = 0.2)
p12 <- p12 + labs(x = 'Latitude (degrees)',
              y = 'Blade length / Blade width')
p12 <- p12 + annotate("text", x = 40, y = 1.75,
                  label = "p = 0.31, r2 =   0.10",
                  hjust = 0)

p12 <- arrangeGrob(p12, top = textGrob("(l)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(BL.BLWP ~ lat , data = oak.means))

p13 <- ggplot(oak.means.se, aes(x=lat, y=BL.BLWP))
p13 <- p13 + geom_point()
p13 <- p13 + geom_smooth(method="lm")
p13 <- p13 + geom_errorbar(aes(ymin = BL.BLWP-BL.BLWP.se, ymax = BL.BLWP+BL.BLWP.se),
                       width = 0.2)
p13 <- p13 + labs(x = 'Latitude (degrees)',
              y = 'bladeL / bladeLtoWidestPoint')
p13 <- p13 + annotate("text", x = 35, y = 1.75,
                  label = "p = 0.83, r2 = 0.0046",
                  hjust = 0)

p13 <- arrangeGrob(p13, top = textGrob("(m)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(petioleL ~ lat , data = oak.means))

p14 <- ggplot(oak.means.se, aes(x=lat, y=petioleL))
p14 <- p14 + geom_point()
p14 <- p14 + geom_smooth(method="lm")
p14 <- p14 + geom_errorbar(aes(ymin = petioleL-petioleL.se, ymax = petioleL+petioleL.se),
                       width = 0.2)
p14 <- p14 + labs(x = 'Latitude (degrees)',
              y = 'petioleL (mm)')
p14 <- p14 + annotate("text", x = 43, y = 30,
                  label = "p =  0.26, r2 = 0.12",
                  hjust = 0)

p14 <- arrangeGrob(p14, top = textGrob("(n)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(sinusMinL ~ lat , data = oak.means))

p15 <- ggplot(oak.means.se, aes(x=lat, y=sinusMinL))
p15 <- p15 + geom_point()
p15 <- p15 + geom_smooth(method="lm")
p15 <- p15 + geom_errorbar(aes(ymin = sinusMinL-sinusMinL.se, ymax = sinusMinL+sinusMinL.se),
                       width = 0.2)
p15 <- p15 + labs(x = 'Latitude (degrees)',
              y = 'sinusMinL (mm)')
p15 <- p15 + annotate("text", x = 40, y = 40,
                  label = "p =  0.21, r2 =  0.15",
                  hjust = 0)

p15 <- arrangeGrob(p15, top = textGrob("(o)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))

# summary(lm(sinus.v.width ~ lat , data = oak.means))

p16 <- ggplot(oak.means.se, aes(x=lat, y=sinus.v.width))
p16 <- p16 + geom_point()
p16 <- p16 + geom_smooth(method="lm")
p16 <- p16 + geom_errorbar(aes(ymin = sinus.v.width-sinus.v.width.se, ymax = sinus.v.width+sinus.v.width.se),
                       width = 0.2)
p16 <- p16 + labs(x = 'Latitude (degrees)',
              y = 'sinusMinL/bladeW (mm)')
p16 <- p16 + annotate("text", x = 40, y = 0.5,
                  label = "p =  0.024, r2 =  0.41",
                  hjust = 0)

p16 <- arrangeGrob(p16, top = textGrob("(p)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))


# summary(lm(sinus.v.width ~ lat , data = oak.means[-c(9), ]))

p17 <- ggplot(oak.means.se[-c(9), ], aes(x=lat, y=sinus.v.width))
p17 <- p17 + geom_point()
p17 <- p17 + geom_smooth(method="lm")
p17 <- p17 + geom_errorbar(aes(ymin = sinus.v.width-sinus.v.width.se, ymax = sinus.v.width+sinus.v.width.se),
                       width = 0.2)
p17 <- p17 + labs(x = 'Latitude (degrees)',
              y = 'sinusMinL / bladeW (outlier removed)')
p17 <- p17 + annotate("text", x = 37, y = .35,
                  label = "p = 0.054, r2 = 0.35",
                  hjust = 0)

p17 <- arrangeGrob(p17, top = textGrob("(q)", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, fontfamily = "Times")))


##PANEL OF ALL REGRESSIONS
pdf("../out/SUPPL-FIGS1.Regressions Panel.pdf", 16, 16)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
dev.off()
