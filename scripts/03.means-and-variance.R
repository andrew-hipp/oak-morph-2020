oak.tree.means <- as.data.frame(t(sapply(split(oak.dat[c('lat', oak.vars, c('mds1', 'mds2', 'mds3'), paste('bio', 1:19, sep = ''))], oak.dat$tree), function(x) apply(x, 2, mean))))
oak.means <- as.data.frame(t(sapply(split(oak.tree.means, oak.dat$site[match(row.names(oak.tree.means), oak.dat$tree)]), function(x) apply(x, 2, mean))))
oak.se <- as.data.frame(t(sapply(split(oak.tree.means, oak.dat$site[match(row.names(oak.tree.means), oak.dat$tree)]), function(x) apply(x, 2, sd) / (sqrt(3)))))
names(oak.se) <- paste(names(oak.se), 'se', sep = '.')
oak.means.se <- cbind(oak.means, oak.se)

#variance
oak.tree.var <- as.data.frame(t(sapply(split(oak.dat[c('lat', oak.vars, c('mds1', 'mds2', 'mds3'), paste('bio', 1:19, sep = ''))], oak.dat$tree), function(x) apply(x, 2, var))))
oak.tree.var <- oak.tree.var[, -1]
lat <- oak.tree.means$lat
oak.tree.var <- cbind(oak.tree.var, lat)
oak.means.var <- as.data.frame(t(sapply(split(oak.tree.var, oak.dat$site[match(row.names(oak.tree.var), oak.dat$tree)]), function(x) apply(x, 2, mean))))
oak.se.var <- as.data.frame(t(sapply(split(oak.tree.var, oak.dat$site[match(row.names(oak.tree.var), oak.dat$tree)]), function(x) apply(x, 2, sd) / (sqrt(3)))))
names(oak.se.var) <- paste(names(oak.se.var), 'se', sep = '.')
oak.var.se <- cbind(oak.means.var, oak.se.var)
