#CLIMATE REGRESSIONS -- fig 5
# summary(lm(bio1 ~ lat , data = oak.means))


p10 <- ggplot(oak.means.se, aes(x=lat, y=bio1))
p10 <- p10 + geom_point()
p10 <- p10 + geom_smooth(method="lm")
p10 <- p10 + geom_errorbar(aes(ymin = bio1-bio1.se, ymax = bio1+bio1.se),
                       width = 0.2)
p10 <- p10 + labs(x = '',
              y = 'bio1: mean annual temperature')
p10 <- p10 + annotate("text", x = 42, y = 150,
                  label = "p < 0.001, r2 = 0.9926",
                  hjust = 0)


p11 <- ggplot(oak.means.se, aes(x=lat, y=bio12))
p11 <- p11 + geom_point()
p11 <- p11 + geom_smooth(method="lm")
p11 <- p11 + geom_errorbar(aes(ymin = bio12-bio12.se, ymax = bio12+bio12.se),
                       width = 0.2)
p11 <- p11 + labs(x = '',
              y = 'bio12: mean annual precipitation')
p11 <- p11 + annotate("text", x = 43, y = 1050,
                  label = "P < 0.001, r2 = 0.68",
                  hjust = 0)

p12 <- ggplot(oak.means.se, aes(x=lat, y=bio4))
p12 <- p12 + geom_point()
p12 <- p12 + geom_smooth(method="lm")
p12 <- p12 + geom_errorbar(aes(ymin = bio4-bio4.se, ymax = bio4+bio4.se),
                       width = 0.2)
p12 <- p12 + labs(x = '',
              y = 'bio4: temperature seasonality')
p12 <- p12 + annotate("text", x = 43, y = 9500,
                  label = "P < 0.001, r2 = 0.98",
                  hjust = 0)

pdf("../out/Fig 5. Climate.Data.Panel.pdf", 5, 9)

grid.arrange(p10, p11, p12, bottom = "Latitude (degrees)")

dev.off()
