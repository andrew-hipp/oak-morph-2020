#CLIMATE REGRESSIONS -- fig 5
# summary(lm(bio1 ~ lat , data = oak.means))

library(ggplot2)
library(ggrepel)
library(gridExtra)

p10 <- ggplot(oak.means.se, aes(x=lat, y=bio1))
p10 <- p10 + geom_point()
p10 <- p10 + geom_smooth(method="lm")
p10 <- p10 + geom_errorbar(aes(ymin = bio1-bio1.se, ymax = bio1+bio1.se),
                       width = 0.2)
p10 <- p10 + labs(x = '',
              y = 'bio1: mean annual temperature')
p10 <- p10 + annotate("text",
                  max(oak.means.se$lat),
                  max(oak.means.se$bio1 + oak.means.se$bio1.se),
                  label = "p < 0.001, r2 = 0.99",
                  hjust = 'right', size = 2)
p10 <- p10 + geom_label_repel(size = 1, aes(label = site))

p11 <- ggplot(oak.means.se, aes(x=lat, y=bio12))
p11 <- p11 + geom_point()
p11 <- p11 + geom_smooth(method="lm")
p11 <- p11 + geom_errorbar(aes(ymin = bio12-bio12.se, ymax = bio12+bio12.se),
                       width = 0.2)
p11 <- p11 + labs(x = '',
              y = 'bio12: mean annual precipitation')
p11 <- p11 + annotate("text",
                  max(oak.means.se$lat),
                  max(oak.means.se$bio12 + oak.means.se$bio12.se),
                  label = "P < 0.001, r2 = 0.68",
                  hjust = 'right', size = 2)

p12 <- ggplot(oak.means.se, aes(x=lat, y=bio4))
p12 <- p12 + geom_point()
p12 <- p12 + geom_smooth(method="lm")
p12 <- p12 + geom_errorbar(aes(ymin = bio4-bio4.se, ymax = bio4+bio4.se),
                       width = 0.2)
p12 <- p12 + labs(x = 'Latitude (degrees)',
              y = 'bio4: temperature seasonality')
p12 <- p12 + annotate("text",
                      min(oak.means.se$lat),
                      max(oak.means.se$bio4 + oak.means.se$bio4.se),
                      label = "P < 0.001, r2 = 0.98",
                      hjust = 'left', size = 2)

pdf("../out/Fig5.climateRegressions.pdf", 3.5, 9)

grid.arrange(p10, p11, p12)

dev.off()
