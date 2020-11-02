## do simulations for Desmond and Hipp
## ah 2018-06-27

source('../scripts/10a.simsGen.v2.R')
source('../scripts/10b.simsAnova.R')

# Ncores <- 8
simsList <- genSims(Nsites = 20,
                    Ntrees = 3:12,
                    Nleaves = 3:12,
                    Nreps = 100
                    )

Ncores <- 1 # fails with more cores... too much memory
simsAOV <- sims.aov(simsList)
simsHSD <- sims.hsd(simsAOV)

pdf('../out/FIG6.sims.plot.v3.pdf', 10, 5)
plot.hsd(simsHSD, probThresh = 0.5)
dev.off()
