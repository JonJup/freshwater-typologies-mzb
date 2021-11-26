# ----------------------------------------- #
### --- Create species X sites tables --- ### 
### --- Only summer samples           --- ### 
# ----------------------------------------- #

# --------------- #
# files in 
#               -> 06_final_taxon_all_typologies.rds         | macroinvertebrates at optimal resolution 
# files out
#               <- 07_sxs_list.RDS           | taxa  X sites table    
#               <- 07_sxs_genus.RDS          | genus  X sites table    
# Purpose:
#               Create species X sites table 
# --------------- #

# 01. Setup  --------------------------------------------------------------
pacman::p_load(
    data.table, 
    fuzzySim,
    purrr, 
    dplyr, 
    magrittr,
    stringr
)

# read in and prepare data ------------------------------------------------
## -- load data 
dt_mzb  = readRDS("data/summer season//06_final_taxon_all_typologies.rds")

## -- trim names
dt_mzb %<>% map(.f = ~ .x[, final_taxon := str_trim(final_taxon, side = "both")])

## -- remove samples from before 2000
dt_mzb %<>% map(.f = ~ .x[, gr_sample_id := as.character(gr_sample_id)])
dt_mzb %<>% map(.f = ~ .x[, year := lubridate::year(date)])
dt_mzb %<>% map(.f = ~ .x[(is.na(year) | year >= 2000)])

# -------------------------- # 
# --- summary statistics --- #  
# -------------------------- #

#- number of sites 
uniqueN(dt_mzb$brt12$gr_sample_id)
dt_mzb$brt12$gr_sample_id |> 
    unique() |> 
    str_detect("combined") |> 
    sum()

#- number of species 
dt_mzb$brt12[ final_taxon_level == "species", uniqueN(species)]
dt_mzb$brt20[ final_taxon_level == "species", uniqueN(species)]
dt_mzb$gloric[final_taxon_level == "species", uniqueN(species)]
dt_mzb$bgr[   final_taxon_level == "species", uniqueN(species)]
dt_mzb$illies[final_taxon_level == "species", uniqueN(species)]

#- number of genera  
dt_mzb$brt12[ final_taxon_level == "genus", uniqueN(genus)]
dt_mzb$brt20[ final_taxon_level == "genus", uniqueN(genus)]
dt_mzb$gloric[final_taxon_level == "genus", uniqueN(genus)]
dt_mzb$bgr[   final_taxon_level == "genus", uniqueN(genus)]
dt_mzb$illies[final_taxon_level == "genus", uniqueN(genus)]

#- number of species 
dt_mzb$brt12[ final_taxon_level == "family", uniqueN(family)]
dt_mzb$brt20[ final_taxon_level == "family", uniqueN(family)]
dt_mzb$gloric[final_taxon_level == "family", uniqueN(family)]
dt_mzb$bgr[   final_taxon_level == "family", uniqueN(family)]
dt_mzb$illies[final_taxon_level == "family", uniqueN(family)]

# 03. Drop columns --------------------------------------------------------
dt_genus <- map(
        .x = dt_mzb, 
        .f = ~ .x[,.(gr_sample_id, genus, final_taxon_level, brt12, brt20, gloric, illies, eea, geometry)]
        )
dt_mzb2 <- map(
        .x = dt_mzb, 
        .f = ~ .x[,.(gr_sample_id, final_taxon, final_taxon_level, brt12, brt20, gloric, illies, eea)]
)
dt_genus %<>% map(.f = ~ .x[!is.na(genus)])

## -- different levels 
ls_mzb = list()

ls_mzb$spe <-
    map(.x = dt_mzb2,
        .f = ~ .x[final_taxon_level == "species"]
    )
ls_mzb$gen <-
    map(.x = dt_mzb2,
        .f = ~ .x[final_taxon_level == "genus"]
    )
ls_mzb$foh <-
    map(.x = dt_mzb2,
        .f = ~ .x[final_taxon_level %in% c("family", "order", "subclass", "class")]
    )

ls_mzb$spe%<>% map(.f = ~ .x[, final_taxon_level := NULL]) 
ls_mzb$gen%<>% map(.f = ~ .x[, final_taxon_level := NULL]) 
ls_mzb$foh%<>% map(.f = ~ .x[, final_taxon_level := NULL])

ls_rt <- 
    vector(mode = "list", length = 4)
names(ls_rt) <- c("spe", "gen", "foh", "gen2")
type_placeholder <-  vector(mode = "list", length = 5)
names(type_placeholder) <- c("brt12", "brt20", "gloric", "illies", "eea")

ls_rt$spe <- type_placeholder
ls_rt$gen <- type_placeholder
ls_rt$foh <- type_placeholder
ls_rt$gen2 <- type_placeholder

ls_rt$spe$brt12  <- unique(ls_mzb$spe$brt12[ ,.(gr_sample_id, brt12)] , by = "gr_sample_id")
ls_rt$spe$brt20  <- unique(ls_mzb$spe$brt20[ ,.(gr_sample_id, brt20)] , by = "gr_sample_id")
ls_rt$spe$gloric <- unique(ls_mzb$spe$gloric[,.(gr_sample_id, gloric)], by = "gr_sample_id")
ls_rt$spe$illies <- unique(ls_mzb$spe$illies[,.(gr_sample_id, illies)], by = "gr_sample_id")
ls_rt$spe$eea    <- unique(ls_mzb$spe$bgr[   ,.(gr_sample_id, eea)]   , by = "gr_sample_id")
ls_rt$gen$brt12  <- unique(ls_mzb$gen$brt12[ ,.(gr_sample_id, brt12)] , by = "gr_sample_id")
ls_rt$gen$brt20  <- unique(ls_mzb$gen$brt20[ ,.(gr_sample_id, brt20)] , by = "gr_sample_id")
ls_rt$gen$gloric <- unique(ls_mzb$gen$gloric[,.(gr_sample_id, gloric)], by = "gr_sample_id")
ls_rt$gen$illies <- unique(ls_mzb$gen$illies[,.(gr_sample_id, illies)], by = "gr_sample_id")
ls_rt$gen$eea    <- unique(ls_mzb$gen$bgr[   ,.(gr_sample_id, eea)]   , by = "gr_sample_id")
ls_rt$foh$brt12  <- unique(ls_mzb$foh$brt12[ ,.(gr_sample_id, brt12)] , by = "gr_sample_id")
ls_rt$foh$brt20  <- unique(ls_mzb$foh$brt20[ ,.(gr_sample_id, brt20)] , by = "gr_sample_id")
ls_rt$foh$gloric <- unique(ls_mzb$foh$gloric[,.(gr_sample_id, gloric)], by = "gr_sample_id")
ls_rt$foh$illies <- unique(ls_mzb$foh$illies[,.(gr_sample_id, illies)], by = "gr_sample_id")
ls_rt$foh$eea    <- unique(ls_mzb$foh$bgr[   ,.(gr_sample_id, eea)]   , by = "gr_sample_id")
ls_rt$gen2$brt12  <- unique(dt_genus$brt12[ ,.(gr_sample_id, brt12)] , by = "gr_sample_id")
ls_rt$gen2$brt20  <- unique(dt_genus$brt20[ ,.(gr_sample_id, brt20)] , by = "gr_sample_id")
ls_rt$gen2$gloric <- unique(dt_genus$gloric[,.(gr_sample_id, gloric)], by = "gr_sample_id")
ls_rt$gen2$illies <- unique(dt_genus$illies[,.(gr_sample_id, illies)], by = "gr_sample_id")
ls_rt$gen2$eea    <- unique(dt_genus$bgr[   ,.(gr_sample_id, eea)]   , by = "gr_sample_id")

# 04. Turn to site X species matrix --------------------------------------------------------
ls_mzb$spe %<>% 
    lapply(splist2presabs,sites.col = 1, sp.col = 2) %>% 
    lapply(setDT)
ls_mzb$gen %<>% 
    lapply(splist2presabs,sites.col = 1, sp.col = 2) %>% 
    lapply(setDT)
ls_mzb$foh %<>% 
    lapply(splist2presabs,sites.col = 1, sp.col = 2) %>% 
    lapply(setDT)
dt_genus %>% 
    lapply(splist2presabs,sites.col = 1, sp.col = 2) %>% 
    lapply(setDT) -> 
    sxg

for (j in 1:3){
    for (i in 1:5) ls_mzb[[j]][[i]] <- ls_mzb[[j]][[i]][ls_rt[[j]][[i]], on = "gr_sample_id"]   
}
for (i in 1:5) sxg[[i]] <- sxg[[i]][ls_rt$gen2[[i]], on = "gr_sample_id"]


# 08. Save data to file ---------------------------------------------------
saveRDS(dt_genus, "data/summer season/06_final_taxon_genus_all_typologies.rds")
saveRDS(ls_mzb,   "data/summer season/07_sxs_list_all_typologies.rds")
saveRDS(sxg,      "data/summer season/07_sxs_genus_all_typologies.rds")
