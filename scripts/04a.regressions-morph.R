# figure 3 -- morphological plots
library(ggplot2)
library(gridExtra)
library(ggrepel)

oak.means.se$site <- row.names(oak.means.se)

# PANEL A
p.a <- ggplot(oak.means.se, aes(x=lat, y=Area, label = site))
p.a <- p.a + geom_point()
p.a <- p.a + geom_smooth(method="lm")
p.a <- p.a + geom_errorbar(aes(ymin = Area-Area.se, ymax = Area+Area.se),
                       width = 0.2)
p.a <- p.a + labs(x = '',
              y = 'Leaf area (mm2)')
p.a <- p.a + annotate("text", x = max(oak.means.se$lat),
                              y = max(oak.means.se$Area + oak.means.se$Area.se),
                  label = "P = 0.014, r2 =  0.47",
                  hjust = 'right', size = 3)
p.a <- p.a + geom_label_repel(size = 2.5, aes(label = site))
p.a <- p.a + annotate("text", x = min(oak.means.se$lat),
                              y = max(oak.means.se$Area + oak.means.se$Area.se),
                  label = "A.",
                  hjust = 'left')

# PANEL B
p.b <- ggplot(oak.means.se, aes(x=lat, y=bladeL))
p.b <- p.b + geom_point()
p.b <- p.b + geom_smooth(method="lm")
p.b <- p.b + geom_errorbar(aes(ymin = bladeL-bladeL.se, ymax = bladeL+bladeL.se),
                       width = 0.2)
p.b <- p.b + labs(x = '',
              y = 'Leaf blade length (mm)')
p.b <- p.b + annotate("text", x = max(oak.means.se$lat),
                              y = max(oak.means.se$bladeL + oak.means.se$bladeL.se),
                  label = "P = 0.012, r2 =  0.49",
                  hjust = 'right', size = 3)
# p.b <- p.b + geom_label_repel(size = 2.5, aes(label = site))
p.b <- p.b + annotate("text", x = min(oak.means.se$lat),
                              y = max(oak.means.se$bladeL + oak.means.se$bladeL.se),
                  label = "B.",
                  hjust = 'left')
# PANEL C
p.c <- ggplot(oak.means.se, aes(x=lat, y=Area.Mass))
p.c <- p.c + geom_point()
p.c <- p.c + geom_smooth(method="lm")
p.c <- p.c + geom_errorbar(aes(ymin = Area.Mass-Area.Mass.se, ymax = Area.Mass+Area.Mass.se),
                       width = 0.2)
p.c <- p.c + labs(x = '',
              y = 'Specific leaf area')
p.c <- p.c + annotate("text", x = max(oak.means.se$lat),
                              y = max(oak.means.se$Area.Mass + oak.means.se$Area.Mass.se),
                  label = "P = 0.015, r2 =  0.47",
                  hjust = 'right', size = 3)
# p.c <- p.c + geom_label_repel(size = 2.5, aes(label = site))
p.c <- p.c + annotate("text", x = min(oak.means.se$lat),
                              y = max(oak.means.se$Area.Mass + oak.means.se$Area.Mass.se),
                  label = "C.",
                  hjust = 'left')
# PANEL D
p.d <- ggplot(oak.means.se, aes(x=lat, y=sinus.v.width))
p.d <- p.d + geom_point()
p.d <- p.d + geom_smooth(method="lm")
p.d <- p.d + geom_errorbar(aes(ymin = sinus.v.width-sinus.v.width.se, ymax = sinus.v.width+sinus.v.width.se),
                       width = 0.2)
p.d <- p.d + labs(x = '',
              y = 'Sinus depth / leaf width')
p.d <- p.d + annotate("text", x = max(oak.means.se$lat),
                              y = max(oak.means.se$sinus.v.width + oak.means.se$sinus.v.width.se),
                  label = "P =  0.024, r2 =  0.41",
                  hjust = 'right', size = 3)
p.d <- p.d + annotate("text", x = min(oak.means.se$lat),
                              y = max(oak.means.se$sinus.v.width + oak.means.se$sinus.v.width.se),
                  label = "D.",
                  hjust = 'left')
# p.d <- p.d + geom_label_repel(size = 2.5, aes(label = site))

## now plot 'em
p.all <- list(p.a, p.b, p.c, p.d)
pdf('../out/Fig3.morphRegressions.pdf', 7.5, 7.5)
grid.arrange(p.a, p.b, p.c, p.d, ncol = 2, bottom = "Latitude (degrees, all plots)")
dev.off()
