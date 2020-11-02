##Normalizing outlines and eFourier analysis
## A Hipp, 2020-10-20: begins after landsmarks and outlines have been written

library(Momocs)
library(ade4)
library(rlang)


leaf_coords_1_50 <- readRDS("../data/ldks/leaf_coords_1_50_w_ldks.rds")
leaf_coords_51_100 <- readRDS("../data/ldks/leaf_coords_51_100_w_ldks.rds")
leaf_coords_101_150 <- readRDS("../data/ldks/leaf_coords_101_150_w_ldks.rds")
leaf_coords_151_200 <- readRDS("../data/ldks/leaf_coords_151_200_w_ldks.rds")
leaf_coords_201_250 <- readRDS("../data/ldks/leaf_coords_201_250_w_ldks.rds")
leaf_coords_251_302 <- readRDS("../data/ldks/leaf_coords_251_302_w_ldks.rds")

coordinates<-c(leaf_coords_1_50$coo, leaf_coords_51_100$coo, leaf_coords_101_150$coo, leaf_coords_151_200$coo, leaf_coords_201_250$coo, leaf_coords_251_302$coo)
landmarks<-c(leaf_coords_1_50$ldk, leaf_coords_51_100$ldk, leaf_coords_101_150$ldk, leaf_coords_151_200$ldk, leaf_coords_201_250$ldk, leaf_coords_251_302$ldk)
names(landmarks) <- names(coordinates)

lvs.mangled <- c("IA-MG243-W-B", "IL-MG603-E-A", "IL-MG603-N-A", "IL-MG604-S-A",
                "IL-SF003-W-A", "MB-MG516-E-A", "MB-MG516-E-B", "MB-MG516-W-A",
                "MB-MG517-E-B", "MB-MG517-N-B", "MB-MG517-S-A", "MB-MG518-N-A",
                "MB-MG518-W-A", "MN-MG789S-B", "MN-MG789W-A", "MN-MG789W-B",
                "MN-MG790E-A", "MN-MG790E-B", "MN-MG790N-A", "MN-MG790N-B", "MN-SD002-E-A",
                "MN-SD002-N-A", "OH-MG799S-B", "OH-MG805S-A", "OH-MG805W-B",
                "OK-MG282-E-A", "OK-MG282-N-A", "OK-MG283-E-B", "OK-MG283-N-A",
                "OK-MG283-N-B", "OK-MG283-S-A", "OK-MG283-S-B", "OK-MG283-W-A",
                "OK-MG347-W-B", "OK-MG350-E-A", "OK-MG350-E-B", "OK-MG350-N-B",
                "OK-MG350-S-B", "OK-MG369-N-B", "OK-MG371-E-A")
lvs.good <- setdiff(names(coordinates), lvs.mangled)
leaf_coords <- Out(coordinates[lvs.good], ldk=landmarks[lvs.good])
saveRDS(leaf_coords,"../out/leaves_4ldks_2020-10-20.rds")

leaf_coords_raw <- list(leaf_coords_1_50 = leaf_coords_1_50,
                        leaf_coords_51_100 = leaf_coords_51_100,
                        leaf_coords_101_150 = leaf_coords_101_150,
                        leaf_coords_151_200 = leaf_coords_151_200,
                        leaf_coords_201_250 = leaf_coords_201_250,
                        leaf_coords_251_302 = leaf_coords_251_302,
                        coordinates = coordinates,
                        landmarks = landmarks,
                        leaves.removed = lvs.mangled
                      )
rm(leaf_coords_1_50, leaf_coords_101_150, leaf_coords_151_200,
  leaf_coords_201_250, leaf_coords_251_302, leaf_coords_51_100,
  lvs.mangled, lvs.good)

leaf_coords_cs <- coo_slide(leaf_coords, ldk=2)
message('doing procrustes')
leaf_coords_P <- fgProcrustes(leaf_coords_cs)
message('doing efa')
leaf_coords_f <- efourier(leaf_coords_P, smooth.it = 0, norm = FALSE, start = FALSE)
leaf_coords_D <- PCA(leaf_coords_f)


##BAD OUTLINES REMOVED
pdf('../out/SUPPL.eFourier Analysis2.pdf', 5,5)
plot(leaf_coords_D, labelspoints = TRUE, axisnames = TRUE)
dev.off()
