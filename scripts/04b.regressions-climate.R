#CLIMATE REGRESSIONS -- fig 5
# summary(lm(bio1 ~ lat , data = oak.means))

pdf('../out/Fig 4a. MeanAnnualTemp.lat.pdf', 8,8)
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

# summary(lm(bio12 ~ lat , data = oak.means))

pdf('../out/Fig 4b. MeanAnnualPrecip.lat.pdf', 8,8)
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

# summary(lm(bio4 ~ lat, data = oak.means))

pdf('../out/Fig 4c. TempSeasonalaity.lat.pdf', 8,8)
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
