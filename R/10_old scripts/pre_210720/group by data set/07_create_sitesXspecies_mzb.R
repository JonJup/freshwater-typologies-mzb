### ————————————————————————————————————— ###
### ——— Create species X sites tables ——— ### 
### ——— group by data set             ——— ### 
### ————————————————————————————————————— ### 

### —————————————————————
# date: 
#       (08+09).07.21
# files in 
#               -> 06_final_taxon_all_typologies.rds         | macroinvertebrates at optimal resolution 
# files out
# Project: 
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#               Create species X sites table 
# Comment: 
#       The taxonomic resolution varies a little between data sets. 
#       Hence it might make a slight difference which typology is chosen here. 
#       I use Illies and BRT12. 
### —————————————————————

# 01. Setup  --------------------------------------------------------------
pacman::p_load(
    data.table, 
    fuzzySim,
    purrr, 
    dplyr, 
    magrittr,
    stringr
)

# load data ------------------------------------------------
data  <- readRDS("data/06_final_taxon_all_typologies.rds")

# prepare data ----------------------------------------------------------------------

data %<>% 
    #- trim names
    map(.f = ~ .x[, final_taxon := str_trim(final_taxon, side = "both")]) |>
    #- reformat gr_sample_id to charachter string 
    map(.f = ~ .x[, gr_sample_id := as.character(gr_sample_id)]) |> 
    #- add a year variable
    map(.f = ~ .x[, year := lubridate::year(date)]) |> 
    #- remove samples from before 2000
    map(.f = ~ .x[(is.na(year) | year >= 2000)]) 

#- select only the focal typologies 
brt12 <- data$brt12
illie <- data$illies


#- create data set with taxon information on genus level irrespective of final taxon level (i.e. optimal taxonomic resolution) 
brt12_genus <- brt12[,.(gr_sample_id, genus, data.set, final_taxon_level,  geometry)]
illie_genus <- illie[,.(gr_sample_id, genus, data.set, final_taxon_level,  geometry)]
#- remove entries that do not have genus level information form genus data set
brt12_genus <- brt12_genus[!is.na(genus)]
illie_genus <- illie_genus[!is.na(genus)]


#- create data set that represents at optimal taxonomic level  
brt12_2 <- brt12[,.(gr_sample_id, data.set, final_taxon, final_taxon_level)]
illie_2 <- illie[,.(gr_sample_id, data.set, final_taxon, final_taxon_level)]

#- separate into different taxonomic levels 
brt12_mzb <- list()
illie_mzb <- list()

brt12_mzb$spe <- brt12_2[final_taxon_level == "species"]
illie_mzb$spe <- illie_2[final_taxon_level == "species"]
brt12_mzb$gen <- brt12_2[final_taxon_level == "genus"]
illie_mzb$gen <- illie_2[final_taxon_level == "genus"]
brt12_mzb$foh <- brt12_2[final_taxon_level %in% c("family", "order", "subclass", "class")]
illie_mzb$foh <- illie_2[final_taxon_level %in% c("family", "order", "subclass", "class")]

#- remove final_taxon_level variable 
brt12_mzb  %<>% map(.f = ~ .x[, final_taxon_level := NULL]) 
illie_mzb  %<>% map(.f = ~ .x[, final_taxon_level := NULL]) 

# Turn to site X species matrix --------------------------------------------------------
brt12.sxs <- map(.x = brt12_mzb, .f = ~ setDT(splist2presabs(data = .x, sites.col = 1, sp.col = 3)))
illie.sxs <- map(.x = illie_mzb, .f = ~ setDT(splist2presabs(data = .x, sites.col = 1, sp.col = 3)))
brt12.g.sxs <- setDT(splist2presabs(data = brt12_genus, sites.col = 1, sp.col = 2))
illie.g.sxs <- setDT(splist2presabs(data = illie_genus, sites.col = 1, sp.col = 2))


# join to data set  -----------------------------------------------------------------

#- create data.set data set 
brt12.ds <- unique(brt12[, c("gr_sample_id", "data.set"),  with = F], by = "gr_sample_id")
illie.ds <- unique(illie[, c("gr_sample_id", "data.set"),  with = F], by = "gr_sample_id")

#- join 
brt12.g.sxs <- brt12.ds[brt12.g.sxs, on = "gr_sample_id"]
illie.g.sxs <- illie.ds[illie.g.sxs, on = "gr_sample_id"]
brt12.sxs <-   map(.x = brt12.sxs, .f = ~ brt12.ds[.x, on = "gr_sample_id"])
illie.sxs <-   map(.x = illie.sxs, .f = ~ illie.ds[.x, on = "gr_sample_id"])

# 08. Save data to file ---------------------------------------------------
saveRDS(brt12_genus, "data/group by data set/06_ft_genus_brt12.rds")
saveRDS(illie_genus, "data/group by data set/06_ft_genus_illies.rds")
saveRDS(brt12.sxs,   "data/group by data set/07_sxs_list_brt12.rds")
saveRDS(illie.sxs,   "data/group by data set/07_sxs_list_illies.rds")
saveRDS(brt12.g.sxs, "data/group by data set/07_sxs_genus_brt12.rds")
saveRDS(illie.g.sxs, "data/group by data set/07_sxs_genus_illies.rds")
