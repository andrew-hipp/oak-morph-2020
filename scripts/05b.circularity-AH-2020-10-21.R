##Normalizing outlines and eFourier analysis
## A Hipp, 2020-10-20: begins after landsmarks and outlines have been written

library(Momocs)
library(ade4)
library(rlang)

lvs.mangled <- c("IA-MG243-W-B", "IL-MG603-E-A", "IL-MG603-N-A", "IL-MG604-S-A",
                  "IL-SF003-W-A", "MB-MG516-E-A", "MB-MG516-E-B", "MB-MG516-W-A",
                  "MB-MG517-E-B", "MB-MG517-N-B", "MB-MG517-S-A", "MB-MG518-N-A",
                  "MB-MG518-W-A", "MN-MG789S-B", "MN-MG789W-A", "MN-MG789W-B",
                  "MN-MG790E-A", "MN-MG790E-B", "MN-MG790N-A", "MN-MG790N-B", "MN-SD002-E-A",
                  "MN-SD002-N-A", "OH-MG799S-B", "OH-MG805S-A", "OH-MG805W-B",
                  "OK-MG282-E-A", "OK-MG282-N-A", "OK-MG283-E-B", "OK-MG283-N-A",
                  "OK-MG283-N-B", "OK-MG283-S-A", "OK-MG283-S-B", "OK-MG283-W-A",
                  "OK-MG347-W-B", "OK-MG350-E-A", "OK-MG350-E-B", "OK-MG350-N-B",
                  "OK-MG350-S-B", "OK-MG369-N-B", "OK-MG371-E-A", "OK-MG284-N-B",
                  "IA-MG244-E-B", "IL-MG604-E-B", "IL-SF003-S-A", "IN-MG638-N-B",
                  "MB-MG529-E-A", "MN-MG788W-A", "MN-MG789E-A", "MO-MG403-E-A",
                  "MO-MG403-N-B")

if(!exists('outlines.all')) {
  message('... reading outlines from jpegs... ')
  outlines.all <- import_jpg(dir('../data/leafScans-v2/', patt = 'jpg', full = T))
  lvs.good <- setdiff(names(outlines.all), lvs.mangled)
  outlines.subset <- outlines.all[lvs.good]}

if(!exists('outlines.out')) {
  message('... making Out objects... ')
  outlines.out <- Out(outlines.subset)
  outlines.out$coo <- lapply(outlines.out$coo, coo_interpolate, n=200)
  outlines.out <- coo_center(outlines.out)
  # outlines.out$coo <- lapply(outlines.out$coo, FUN=function(x)(coo_scale(x, max(x[,2]/1000))))
  # outlines.out <- coo_scale(outlines.out, 1000)
}

if(!exists('outlines.m')) {
  outlines.m <- measure(outlines.out, coo_area, coo_circularityharalick, coo_circularity)
  outlines.out$fac <- outlines.m$coe}

# outlines.out %>% arrange(circularity) %>% panel(main = 'sorted by circularity') %>% print
