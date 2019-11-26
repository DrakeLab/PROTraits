This file is intended to help readers navigate the data and methods associated with PRSB publication "Characterizing the phylogenetic specialism-generalism spectrum of mammal parasites" by Park et al. 2018. For outstanding questions, please contact awpark@uga.edu

Files - explanations

z.Rmd - main file with code chunks to recreate analysis associated with all figs/tables (including supplementary material)

WR_2005_ST.txt - mammal supertree

GMPD_main_2016-11-28.csv - host-parasite association data from GMPD
GMPD_parasite_traits_2016-11-28.csv - parasite trait data from GMPD
GMPD_parasite_taxonomy_2016-11-28.csv - parasite taxonomy data from GMPD

ses.maxD.R - code adapted from ses.mpd to calculate custom metric "span"
ses.mntd.to.maxD.R - code adapted from ses.mpd to calculate custom metric "aggregation"

The following Rda files are included to speed up run time. The ses.mpd function (and associated custom functions) are slow to run because it involves many (n=1000) randomizations. Consequently, in z.Rmd the code chunks with such functions are set to eval=F and the proceeding chunks (with eval=T) load the results that would be obtained from such functions, using Rda files

get_nri.Rda - result from regular ses.mpd call
get_nriABUND.Rda - results from call to ses.mpd, but abundance weighted
get_nri6ztax.Rda - results from call to ses.mpd, but random host sets respect observed host taxonomic bias
get_myMaxD_ABUND.Rda - results from call to ses.maxD, abundance weighted
get_myRatio_ABUND.Rda - results from call to ses.mntd.to.maxD, abundance weighted
