## circularity and area vs latitude

library(Momocs)
library(plyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)
source('../scripts/99.summarySE.R')

## read and format data
oak.sites <- read.delim('../data/Leaf Trait Measurements - data.tsv', as.is = TRUE)
oak.sites <- oak.sites[c('tree', 'leaf', 'site', 'lat', 'long')]
oak.sites <- oak.sites[!duplicated(oak.sites$tree), ]
row.names(oak.sites) <- oak.sites$tree

oak.circ <- outlines.m$coe %>% as.data.frame
row.names(oak.circ) <- names(outlines.out)
oak.circ$tree <-
  oak.circ %>% row.names %>% substr(1, 8)
  # oak.circ %>% row.names %>%
  # strsplit(split = '-', fixed = T) %>%
  # lapply(FUN = '[', (1:2)) %>%
  # sapply(FUN = paste, collapse = '-')

### adding these two lines 1/22/2021 to get rid of two extra sites that snuck in:
oak.sites <- oak.sites[!oak.sites$site %in%
                        c("Pearl King Savana", "Near Shawnee National Forest"),
                        ]
oak.circ <- oak.circ[oak.circ$tree %in% oak.sites$tree, ]

oak.circ <- cbind(oak.circ, oak.sites[oak.circ$tree, ])### use cbind to put the oak sites data on here, indexing by tree

## plot
oc.inds <- summarySE(oak.circ, measurevar = 'circularityharalick', groupvars = 'tree')
oc.inds$site <- oak.circ$site[match(oc.inds$tree, oak.circ$tree)]
oc.inds$lat <- oak.circ$lat[match(oc.inds$site, oak.circ$site)]
oc.sites <- summarySE(oc.inds, measurevar = 'circularityharalick', groupvar = c('site', 'lat'))

oc.inds.circ <- summarySE(oak.circ, measurevar = 'circularity', groupvars = 'tree')
oc.inds.circ$site <- oak.circ$site[match(oc.inds.circ$tree, oak.circ$tree)]
oc.inds.circ$lat <- oak.circ$lat[match(oc.inds.circ$site, oak.circ$site)]
oc.sites.circ <- summarySE(oc.inds.circ, measurevar = 'circularity', groupvar = c('site', 'lat'))


oci.p <- ggplot(oc.inds, aes(x=lat, y=circularityharalick, color = site))
oci.p <- oci.p + geom_errorbar(aes(ymin=circularityharalick-se, ymax=circularityharalick+se), width = 0.2) +
    geom_point()
# print(oci.p)

ocs.p.regression <- lm(circularityharalick ~ lat, oc.sites)

ocs.p <- ggplot(oc.sites, aes(x=lat, y=circularityharalick, label = site))
ocs.p <- ocs.p +
    geom_errorbar(aes(ymin=circularityharalick-se, ymax=circularityharalick+se), width = 0.2) +
    geom_point(size = 2) + geom_smooth(method = 'lm') +
    geom_label_repel(size = 2.5) +
    labs(x = '', y = 'Haralick circularity') +
    annotate("text", x = min(oc.sites$lat),
                     y = max(oc.sites$circularityharalick + oc.sites$se),
                      label = "A.",
                      hjust = 'left') +
    annotate('text', x = max(oc.sites$lat),
                     y = max(oc.sites$circularityharalick + oc.sites$se),
                    label = "P = 0.093, r2 = 0.26    ",
                    hjust = 'right',
                    size = 3)

ocs.c.p.regression <- lm(circularity ~ lat, oc.sites.circ)

ocs.c.p <- ggplot(oc.sites.circ, aes(x=lat, y=circularity, label = site))
ocs.c.p <- ocs.c.p +
    geom_errorbar(aes(ymin=circularity-se, ymax=circularity+se), width = 0.2) +
    geom_point(size = 2) + geom_smooth(method = 'lm') +
    geom_label_repel(size = 2.5) +
    labs(x = '', y = 'Circularity') +
    annotate("text", x = min(oc.sites.circ$lat),
                     y = max(oc.sites.circ$circularity + oc.sites.circ$se),
                      label = "B.",
                      hjust = 'left') +
    annotate('text', x = max(oc.sites.circ$lat),
                     y = max(oc.sites.circ$circularity + oc.sites.circ$se),
                    label = "P = 0.003, r2 = 0.59",
                    hjust = 'right',
                    size = 3)

pdf('../out/FIG4.circularity-v2.PDF', 9, 5)
grid.arrange(ocs.p, ocs.c.p, ncol = 2, bottom = 'Latitude (degrees)')
dev.off()
