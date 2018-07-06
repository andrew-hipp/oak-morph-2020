#ANOVA FOR BLADE LENGTH AND SLA
anova.1 <- lm(bladeL ~ site + tree, data = oak.dat)
anova(anova.1)

anova.2 <- lm(Area.Mass ~ site + tree, data = oak.dat)
anova(anova.2)

#ANOVA USING PCA SCORES
PC <- predict(temp, newdata = oak.dat)
oak.dat <- cbind(oak.dat, PC)

anova.3 <- lm(PC1 ~ site + tree, data = oak.dat)
anova(anova.3)

anova.4 <- lm(PC2 ~ site + tree, data = oak.dat)
anova(anova.4)







