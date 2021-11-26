# ———————————————————————————— #
# ——— Create Test data set ——— # 
# ———————————————————————————— #

# ———————————————————————————————————
#  date created: 27-09-21
# last modified: 27-09-21
#       Project: Evaluating European Broad River Types for Macroinvertebrates
#       Purpose: Take a subset of the combined data to prepare a test data set for the 
#       analysis scripts
# ————————————————


# setup -----------------------------------------------------------------------------
devtools::install_github("https://github.com/JonJup/jjmisc", upgrade = "never")
pacman::p_load(data.table,sf,dplyr,jjmisc, parallelDist)

# load data -------------------------------------------------------------------------
data1 <- readRDS("data/02_combined_data/03_2021-09-27__core_taxa_data_aggregated.rds")

# subset data -----------------------------------------------------------------------
#- For this subset, I will use the samples from summer 
data2 <- data1[[2]]
#- sample 100 site from each river type 
for (i in 1:12){
        if (i == 1) all_sites = c()
        i.print = as.character(i)
        if (i < 10) i.print <- paste0("0", i.print) 
        all_sites <- append(all_sites, data2[brt12 == paste0("RT", i.print), sample(gr_sample_id, 100)])
}
#- create subset 
data2 <- data2[gr_sample_id %in% all_sites]
#- display on map 
#st_as_sf(unique(data2, by = "gr_sample_id")) |> mapview::mapview()

# prepare data ----------------------------------------------------------------------
#- test run functions before uploading them to jjmisc 
#source("../../../projects/jjmisc/R/ct_prepare_data_1.R")
#source("../../../projects/jjmisc/R/ct_prepare_data_2.R")
data3 <- ct_prepare_data1(dataset = data2,
                          taxon = "family")
#- compute distance matrix 
data4 <- ct_prepare_data2(data3, ncores = 6)

# save to file ----------------------------------------------------------------------
saveRDS(list(data = data3, dist = data4), "data/02_combined_data/04_test_set.rds")

