## checking what the EFA pca is actually cuing in on (PC1 and PC2)
library(magrittr)

od <- oak.dat
row.names(od) <- paste(oak.dat$tree, oak.dat$leaf, sep = '-')
inds.pc <- intersect(row.names(leaf_coords_D$x), row.names(od))
pc.x <- leaf_coords_D$x[inds.pc, 1:2]
od <- od[inds.pc, c(oak.vars, 'lat')]

pc.x.cors <- cbind(
  PC1 = cor(pc.x[, 1], od) %>% t,
  PC2 = cor(pc.x[, 2], od) %>% t
)
