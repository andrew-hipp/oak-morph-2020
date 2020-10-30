library(ggplot2)
require(gridExtra)
require(grid)

##SIGNIFICANT REGRESSIONS 
p1 <- ggplot(oak.means.se, aes(x = lat, y = Area))
p1 <- p1 + geom_point()
p1 <- p1 + geom_smooth(method = "lm")
p1 <- p1 + geom_errorbar(aes(ymin = Area - Area.se, ymax = Area + 
	Area.se), width = 0.2)
p1 <- p1 + labs(x = "Latitude (degrees)", y = "Area (mm2)")
p1 <- p1 + annotate("text", x = 35, y = 5000, label = "p = 0.014, r2 =  0.47", 
	hjust = 0)

p1 <- arrangeGrob(p1, top = textGrob("(a)", x = unit(0, "npc"), y = unit(1, 
	"npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, 
	fontfamily = "Times")))

p2 <- ggplot(oak.means.se, aes(x = lat, y = bladeL))
p2 <- p2 + geom_point()
p2 <- p2 + geom_smooth(method = "lm")
p2 <- p2 + geom_errorbar(aes(ymin = bladeL - bladeL.se, ymax = bladeL + 
	bladeL.se), width = 0.2)
p2 <- p2 + labs(x = "Latitude (degrees)", y = "Blade length (mm)")
p2 <- p2 + annotate("text", x = 35, y = 119, label = "p = 0.012, r2 =  0.49", 
	hjust = 0)

p2 <- arrangeGrob(p2, top = textGrob("(b)", x = unit(0, "npc"), y = unit(1, 
	"npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, 
	fontfamily = "Times")))

p3 <- ggplot(oak.means.se, aes(x=lat, y=bladeLtoWidestPoint))
p3 <- p3 + geom_point()
p3 <- p3 + geom_smooth(method="lm")
p3 <- p3 + geom_errorbar(aes(ymin = bladeLtoWidestPoint-bladeLtoWidestPoint.se, ymax = bladeLtoWidestPoint+bladeLtoWidestPoint.se),
                       width = 0.2)
p3 <- p3 + labs(x = 'Latitude (degrees)',
              y = 'bladeLtoWidestPoint (mm)')
p3 <- p3 + annotate("text", x = 35, y = 73,
                  label = "p = 0.011, r2 =  0.49",
                  hjust = 0)

p3 <- arrangeGrob(p3, top = textGrob("(c)", x = unit(0, "npc"), y = unit(1, 
	"npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, 
	fontfamily = "Times")))

p4 <- ggplot(oak.means.se, aes(x=lat, y=Area.Mass))
p4 <- p4 + geom_point()
p4 <- p4 + geom_smooth(method="lm")
p4 <- p4 + geom_errorbar(aes(ymin = Area.Mass-Area.Mass.se, ymax = Area.Mass+Area.Mass.se),
                       width = 0.2)
p4 <- p4 + labs(x = 'Latitude (degrees)', y = 'SLA')
p4 <- p4 + annotate("text", x = 35, y = 14000,
                  label = "p = 0.015, r2 =  0.47",
                  hjust = 0)

p4 <- arrangeGrob(p4, top = textGrob("(d)", x = unit(0, "npc"), y = unit(1, 
	"npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, 
	fontfamily = "Times")))

p5 <- ggplot(oak.means.se, aes(x=lat, y=bladeW))
p5 <- p5 + geom_point()
p5 <- p5 + geom_smooth(method="lm")
p5 <- p5 + geom_errorbar(aes(ymin = bladeW-bladeW.se, ymax = bladeW+bladeW.se),
                       width = 0.2)
p5 <- p5 + labs(x = 'Latitude (degrees)',
              y = 'Blade width (mm)')
p5 <- p5 + annotate("text", x = 35, y = 70,
                  label = "p = 0.014, r2 =  0.47",
                  hjust = 0)

p5 <- arrangeGrob(p5, top = textGrob("(e)", x = unit(0, "npc"), y = unit(1, 
	"npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, 
	fontfamily = "Times")))

p6 <- ggplot(oak.means.se, aes(x=lat, y=petioleW))
p6 <- p6 + geom_point()
p6 <- p6 + geom_smooth(method="lm")
p6 <- p6 + geom_errorbar(aes(ymin = petioleW-petioleW.se, ymax = petioleW+petioleW.se),
                       width = 0.2)
p6 <- p6 + labs(x = 'Latitude (degrees)',
              y = 'Petiole width (mm)')
p6 <- p6 + annotate("text", x = 35, y = 1,
                  label = "p < 0.001, r2 =  0.69",
                  hjust = 0)

p6 <- arrangeGrob(p6, top = textGrob("(f)", x = unit(0, "npc"), y = unit(1, 
	"npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, 
	fontfamily = "Times")))
	
##ARRANGE INTO A PANEL

pdf("Fig 3. Significant Regressions Panel.pdf", 7.25, 9)

grid.arrange(p1, p2, p3, p4, p5, p6)

dev.off()

##panel for bladeL, Area, SLA variance regressions


p7 <- ggplot(oak.var.se, aes(x=lat, y=bladeL))
p7 <- p7 + geom_point()
p7 <- p7 + geom_smooth(method="lm")
p7 <- p7 + geom_errorbar(aes(ymin = bladeL-bladeL.se, ymax = bladeL+bladeL.se),
                       width = 0.2)
p7 <- p7 + labs(x = 'Latitude (degrees)',
              y = 'bladeL variance')
p7 <- p7 + annotate("text", x = 45, y = 900,
                  label = "p =  0.002, r2 = 0.644",
                  hjust = 0)
                  
p7 <- arrangeGrob(p7, top = textGrob("(a)", x = unit(0, "npc"), y = unit(1, 
	"npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, 
	fontfamily = "Times")))

p8 <- ggplot(oak.var.se, aes(x=lat, y=Area))
p8 <- p8 + geom_point()
p8 <- p8 + geom_smooth(method="lm")
p8 <- p8 + geom_errorbar(aes(ymin = Area-Area.se, ymax = Area+Area.se),
                       width = 0.2)
p8 <- p8 + labs(x = 'Latitude (degrees)',
              y = 'Area variance')
p8 <- p8 + annotate("text", x = 44.5, y = 1.5e+07,
                  label = "p =  0.059, r2 = 0.311",
                  hjust = 0)
                  
p8 <- arrangeGrob(p8, top = textGrob("(b)", x = unit(0, "npc"), y = unit(1, 
	"npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, 
	fontfamily = "Times")))


p9 <- ggplot(oak.var.se, aes(x=lat, y=Area.Mass))
p9 <- p9 + geom_point()
p9 <- p9 + geom_smooth(method="lm")
p9 <- p9 + geom_errorbar(aes(ymin = Area.Mass-Area.Mass.se, ymax = Area.Mass+Area.Mass.se),
                       width = 0.2)
p9 <- p9 + labs(x = 'Latitude (degrees)',
              y = 'SLA variance')
p9 <- p9 + annotate("text", x = 44.5, y = 1e+07,
                  label = "p =  0.091, r2 = 0.259",
                  hjust = 0)
                  
p9 <- arrangeGrob(p9, top = textGrob("(c)", x = unit(0, "npc"), y = unit(1, 
	"npc"), just = c("left", "top"), gp = gpar(col = "black", fontsize = 18, 
	fontfamily = "Times")))



pdf("Variance Regressions Panel.pdf", 5, 9)

grid.arrange(p7, p8, p9)

dev.off()

#Panel for climate data

p10 <- ggplot(oak.means.se, aes(x=lat, y=bio1))
p10 <- p10 + geom_point()
p10 <- p10 + geom_smooth(method="lm")
p10 <- p10 + geom_errorbar(aes(ymin = bio1-bio1.se, ymax = bio1+bio1.se),
                       width = 0.2)
p10 <- p10 + labs(x = 'Latitude (degrees)',
              y = 'bio1: Mean annual temperature')
p10 <- p10 + annotate("text", x = 42, y = 150,
                  label = "p =  5.505e-12, r2 = 0.9926",
                  hjust = 0)
                  

p11 <- ggplot(oak.means.se, aes(x=lat, y=bio12))
p11 <- p11 + geom_point()
p11 <- p11 + geom_smooth(method="lm")
p11 <- p11 + geom_errorbar(aes(ymin = bio12-bio12.se, ymax = bio12+bio12.se),
                       width = 0.2)
p11 <- p11 + labs(x = 'Latitude (degrees)',
              y = 'bio12: Mean annual precipitation')
p11 <- p11 + annotate("text", x = 43, y = 1050,
                  label = "p =  0.00096, r2 = 0.68",
                  hjust = 0)

p12 <- ggplot(oak.means.se, aes(x=lat, y=bio4))
p12 <- p12 + geom_point()
p12 <- p12 + geom_smooth(method="lm")
p12 <- p12 + geom_errorbar(aes(ymin = bio4-bio4.se, ymax = bio4+bio4.se),
                       width = 0.2)
p12 <- p12 + labs(x = 'Latitude (degrees)',
              y = 'bio4: Temperature seasonality')
p12 <- p12 + annotate("text", x = 43, y = 9500,
                  label = "p =  7.894e-09, r2 = 0.9684",
                  hjust = 0)
                  
pdf("Fig 5. Climate.Data.Panel.pdf", 5, 9)

grid.arrange(p10, p11, p12)

dev.off()                 


