library(agricolae)
library(parallel)
library(magrittr)
library(reshape)
library(egg) # requires ggplot2, gridExtra, and a bunch of others

alphaThreshold = 0.001
# Ncores = 1 # ran out of memory with too many cores... not sure what the threshold would be

sims.aov <- function(sl, writeDat = TRUE) {
  ## generate anovas
  aovList <- vector('list')
  for(sites in attr(sl, 'Nsites')) {
    aovList[[sites]] <- vector('list')
    for(trees in attr(sl, 'Ntrees')) {
      aovList[[sites]][[trees]] <- vector('list')
      for(leaves in attr(sl, 'Nleaves')) {
        message(paste('Doing', sites, 'sites of', trees, 'trees, each with', leaves, 'leaves;', length(sl[[sites]][[trees]][[leaves]]), 'replicates'))
        aovList[[sites]][[trees]][[leaves]] <- mclapply(sl[[sites]][[trees]][[leaves]], function(x) {
          aov(bladeL ~ site + tree, x)
          }, mc.cores = Ncores) # close mclapply
      }
    }
  }
  attr(aovList, 'Nsites') <- attr(sl, 'Nsites')
  attr(aovList, 'Ntrees') <- attr(sl, 'Ntrees')
  attr(aovList, 'Nleaves') <- attr(sl, 'Nleaves')
  if(writeDat) save(aovList, file = format(Sys.time(), 'aovList.%Y-%m-%d.Rdata'))
  aovList
  }

sims.hsd <- function(sa, writeDat = TRUE) {
  ## run HSD tests for all anovas
  ## return only number of unique groups at a given alpha
    hsdList <- vector('list')
    for(sites in attr(sa, 'Nsites')) {
      hsdList[[sites]] <- vector('list')
      for(trees in attr(sa, 'Ntrees')) {
        hsdList[[sites]][[trees]] <- vector('list')
        for(leaves in attr(sa, 'Nleaves')) {
          message(paste('Doing', sites, 'sites of', trees, 'trees, each with', leaves, 'leaves;', length(sa[[sites]][[trees]][[leaves]]), 'replicates'))
          hsdList[[sites]][[trees]][[leaves]] <- mclapply(sa[[sites]][[trees]][[leaves]], function(x) {
            HSD.test(x, 'site')$groups$groups
            }, mc.cores = Ncores) # close mclapply
          }
        }
      }
    attr(hsdList, 'Nsites') <- attr(sa, 'Nsites')
    attr(hsdList, 'Ntrees') <- attr(sa, 'Ntrees')
    attr(hsdList, 'Nleaves') <- attr(sa, 'Nleaves')
    if(writeDat) save(hsdList, file = format(Sys.time(), 'hsdList.%Y-%m-%d.Rdata'))
    hsdList
  }

makeMat.sims <- function(x, do = c('Means', 'Proportions'), probThresh = c(0.50), sites = 20, type = 'hsd') {
  if(type == 'hsd') {
    out <- matrix(NA, length(attr(x, 'Ntrees')), length(attr(x, 'Nleaves')),
                  dimnames = list(paste('Ntr', attr(x, 'Ntrees')),
                                  paste('Nlf', attr(x, 'Nleaves'))
                                  ) # close dimnames
                  ) # close out

    if(length(do) > 1) {
      out <- list(out)
      for(i in 2:length(do)) out <- c(out , out)
      names(out) <- do
    }
    for(trees in attr(x, 'Ntrees')) {
      message(paste('... doing Ntrees', trees))
      for(leaves in attr(x, 'Nleaves')) {
        #message(paste('...... doing Nleaves', leaves))
        vect <- sapply(x[[sites]][[trees]][[leaves]], function(a1) {
          a1 %>% levels %>% paste(collapse = '') %>% strsplit("") %>% '[['(1) %>% unique %>% length
          }) # close lapply
        if('Means' %in% do) out$Means[paste('Ntr', trees), paste('Nlf', leaves)] <- mean(vect)
        if('Proportions' %in% do) out$Proportions[paste('Ntr', trees), paste('Nlf', leaves)] <- sum(vect >= (sites * probThresh)) / length(vect)
      }
    }
    out <- lapply(out, as.data.frame)
  } # close if(type == 'hsd')
  else stop('non-hsd summaries not implemented yet')
  attr(out, 'Nsites') <- sites
  attr(out, 'Ntrees') <- attr(x, 'Ntrees')
  attr(out, 'Nleaves') <- attr(x, 'Nleaves')
  attr(out, 'probThresh') <- probThresh
  out
}

plot.hsd <- function(x, whichdo = c('Means', 'Proportions'), useValNames = FALSE, printVals = TRUE, ...) {
  if(class(x[[length(x)]]) == 'list') x <- makeMat.sims(x, whichdo, ...)
  p <- vector('list', 2)
  for(i in 1:length(whichdo)) {
    x.means <- x[[whichdo[i]]]
    x.means$Ntrees <- attr(x, 'Ntrees')
    names(x.means)[grep('Ntrees', names(x.means))] <- "Number_of_trees_per_site"
    x.means <- melt(x.means, id.vars = 'Number_of_trees_per_site',
                             variable_name= 'Number_of_leaves_per_tree')
    if(useValNames) {
      names(x.means)[which(names(x.means) == 'value')] <-
      switch(whichdo, mean = 'Mean_groups_identified_using_Tukey_HSD',
                      prob = paste('Probability_of_distinguishing', attr(x, 'probThresh'), 'proportion_of_populations', sep = '_')
                      )
                    }
    x.means$Number_of_leaves_per_tree <- gsub('Nlf ', '', x.means$Number_of_leaves_per_tree) %>%
      as.integer
    p[[i]] <- ggplot(x.means, aes(x = Number_of_leaves_per_tree, y = Number_of_trees_per_site,
                                   #fill = Mean_groups_identified_using_Tukey_HSD))
                                   fill = value),
                      show.legend = FALSE)
    p[[i]] <- p[[i]] + geom_tile()
    if(printVals) {
      p[[i]] <- p[[i]] + geom_text(aes(label = value), color = 'white', size = 2)
      p[[i]] <- p[[i]] + theme(legend.position = 'none')
    }
    p[[i]] <- p[[i]] +
      ggtitle(label = ifelse(whichdo[i] == 'Proportions',
                paste('Probability of recognizing', attr(x, 'probThresh') * 100, 'percent of populations'),
                paste('Mean groups recognized out of', attr(x, 'Nsites'), 'populations')
                ) # close ifelse
              ) # close ggtitle
    p[[i]] <- p[[i]] + theme(axis.title.x = element_blank(),
                             axis.title.y = element_blank())
    p[[i]] <- p[[i]] + scale_x_continuous(breaks = attr(x, 'Nleaves'))
    p[[i]] <- p[[i]] + scale_y_continuous(breaks = attr(x, 'Ntrees'))
  }
  grid.arrange(grobs = p, nrow = 1,
               left = "Number of trees per site",
               bottom = "Number of leaves per tree")
}
