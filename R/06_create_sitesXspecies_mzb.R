# ----------------------------------------- #
### --- Create species X sites tables --- ### 
# ----------------------------------------- #

# --------------- #
# date:  17.03.21
# files in 
#               -> 05_final_taxon.rds         | macroinvertebrates at optimal resolution 
# files out
#               <- 06_sxs_list.RDS          | taxa  X sites table    
#               <- 06_sxs_genus.RDS          | genus  X sites table    
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#               Create species X sites table 
# --------------- #

# 01. Setup  --------------------------------------------------------------
source("R/setup_combined_inv.R")

# read in and prepare data ------------------------------------------------
# load data 
dt_mzb  = readRDS("data/05_final_taxon.rds")

# trim names 
dt_mzb[, final_taxon := str_trim(final_taxon, side = "both")]

# remove samples from before 2000
dt_mzb$gr_sample_id %<>% as.character()
dt_mzb[, year := lubridate::year(date)]
dt_mzb = dt_mzb[(is.na(year) | year >= 2000)]

# 03. Drop columns --------------------------------------------------------
dt_genus = dt_mzb[,.(gr_sample_id, genus, final_taxon_level, ls_bd_20, geometry)]
dt_mzb = dt_mzb[,.(gr_sample_id, final_taxon, final_taxon_level, ls_bd_20)]

dt_genus = dt_genus[!is.na(genus)]


## -- different levels 
ls_mzb = list()
ls_mzb$spe = dt_mzb[final_taxon_level == "species"]
ls_mzb$gen = dt_mzb[final_taxon_level == "genus"]
ls_mzb$foh = dt_mzb[final_taxon_level %in% c("family", "order", "subclass", "class")]

ls_mzb$spe[, final_taxon_level := NULL] 
ls_mzb$gen[, final_taxon_level := NULL] 
ls_mzb$foh[, final_taxon_level := NULL]

ls_rt = list(
        spe = unique(ls_mzb$spe[,.(gr_sample_id, ls_bd_20)], by = "gr_sample_id"),
        gen = unique(ls_mzb$gen[,.(gr_sample_id, ls_bd_20)], by = "gr_sample_id"),
        foh = unique(ls_mzb$foh[,.(gr_sample_id, ls_bd_20)], by = "gr_sample_id"),
        gen2 = unique( dt_genus[,.(gr_sample_id, ls_bd_20)], by = "gr_sample_id")
)


# 04. Turn to site X species matrix --------------------------------------------------------
ls_mzb$spe %<>% splist2presabs(sites.col = 1, sp.col = 2) %>%  setDT 
ls_mzb$gen %<>% splist2presabs(sites.col = 1, sp.col = 2) %>%  setDT
ls_mzb$foh %<>% splist2presabs(sites.col = 1, sp.col = 2) %>%  setDT
sxg = dt_genus %>%  splist2presabs(sites.col = 1, sp.col = 2) %>% setDT



ls_mzb$spe = ls_rt$spe[ls_mzb$spe, on = "gr_sample_id"]
ls_mzb$gen = ls_rt$gen[ls_mzb$gen, on = "gr_sample_id"]
ls_mzb$foh = ls_rt$foh[ls_mzb$foh, on = "gr_sample_id"]
sxg = ls_rt$gen2[sxg, on = "gr_sample_id"]


# 05. remove rare species/ sites --------------------------------------------------------
# -- low richness sites -- #

# Compute number of taxa across levels for each site and output a vector of gr_sample_ids with empty sites
# for (i in seq_along(unique(dt_mzb$gr_sample_id))){
#         if (i == 1) ch_empty = ch_1 = ch_5 = c()
#         ls_loop        = list()
#         ls_loop$id     = unique(dt_mzb$gr_sample_id)[i]
#         ls_loop$spe    = ls_mzb$spe[gr_sample_id == ls_loop$id]
#         ls_loop$gen    = ls_mzb$gen[gr_sample_id == ls_loop$id]
#         ls_loop$foh    = ls_mzb$foh[gr_sample_id == ls_loop$id]
#         ls_loop$spe = sum(rowSums(ls_loop$spe[,-c(1:2)]))
#         ls_loop$gen = sum(rowSums(ls_loop$gen[,-c(1:2)]))
#         ls_loop$foh = sum(rowSums(ls_loop$foh[,-c(1:2)]))
#         ls_loop$sum = ls_loop$spe + ls_loop$gen + ls_loop$foh
#         if (ls_loop$sum == 0) ch_empty = append(ch_empty, ls_loop$id)
#         #if (ls_loop$sum == 1) ch_1     = append(ch_1, ls_loop$id)
#         #if (ls_loop$sum < 5)  ch_5     = append(ch_5, ls_loop$id)
#         print(i)
#         rm(ls_loop)
# }

# any empty sites ? 
# ch_empty
# -> no 

# 08. Save data to file ---------------------------------------------------
saveRDS(dt_genus, "data/05_final_taxon_genus.rds")
saveRDS(ls_mzb,   "data/06_sxs_list.RDS")
saveRDS(sxg,     "data/06_sxs_genus.RDS")
