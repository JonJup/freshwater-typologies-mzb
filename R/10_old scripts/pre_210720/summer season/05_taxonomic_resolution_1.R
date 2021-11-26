# -------------------------------- #
### --- Taxonomic Resolution --- ### 
### --- All Typologies       --- ### 
### --- Only summer samples  --- ### 
# -------------------------------- #

# --------------- #
# date:  
#       01.07.21
# files in 
#       -> 04_invertebrates_w_typologies.rds   | macroinvertebrate observations from least impacted sites with all typologies 
# files out
#       <- 05_taxon_resolution_list_brt12.rds
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates 
# Purpose: 
#       Lists of taxa per BRT12-type
# --------------- #

# # Setup -------------------------------------------------------------------
pacman::p_load(
        data.table,
        magrittr,
        mapview,
        dplyr,
        sf
)

# load data ---------------------------------------------------------------
set_all = readRDS("data/04_invertebrates_w_typologies.rds")
rp_summer_id <- readRDS("data/summer season/rivpacs_summer_ids.rds")
# prepare data ------------------------------------------------------------------

#- quick taxonomy fix 
set_all[order == "Plagiorchiida",  `:=`  (subclass = NA, class = "Rhabditophora", phylum = "Platyhelminthes")]
set_all[, site_id := stringr::str_remove(gr_sample_id, "date_[0-9]+")]
# --------------------------- #
## -- Explore the seasons -- ## 
# --------------------------- #

#- how many have a date 
sum(is.na(set_all$date))/nrow(set_all)
set_all[, month := lubridate::month(date)]
set_all[, table(month)]
set_all2 <- set_all[month %in% c(6:9) | gr_sample_id %in% rp_summer_id]
saveRDS(set_all2, "data/summer season/04_invertebrates_w_typologies_summer.rds")
#- keeps ~76% of data 
nrow(set_all2)/nrow(set_all) 

#- maps 
set_all |> 
        unique(by = "site_id") |> 
        st_as_sf() |> 
        mapview() -> 
        all.map 
set_all2 |> 
        unique(by = "site_id") |> 
        st_as_sf() |> 
        mapview() -> 
        season.map 
## -- reduce to adequately represented river types

brt20_acc    = paste0("RT", c(1,2,3,4,5,8,9, 10, 11, 14, 15, 16, 18))
brt12_acc    = paste0("RT", c(1,2,3, 4, 5, 6, 7, 8, 9, 10))
set_all2 = set_all2[brt12 %in% brt12_acc & brt20 %in% brt20_acc]


n_types = c(uniqueN(set_all2$brt12),uniqueN(set_all2$brt20), uniqueN(set_all2$gloric), uniqueN(set_all2$illies), uniqueN(set_all2$eea))
names(n_types) = c("brt12", "brt20", "gloric", "illies", "eea")

rm(brt20_acc, brt12_acc)

## -- split into one data frame per type 
all_typologies = list(
        brt12 =  split.data.frame(x = set_all2, f = factor(set_all2$brt12)),
        brt20 =  split.data.frame(x = set_all2, f = factor(set_all2$brt20)),
        gloric = split.data.frame(x = set_all2, f = factor(set_all2$gloric)),
        illies = split.data.frame(x = set_all2, f = factor(set_all2$illies)),
        bgr =    split.data.frame(x = set_all2, f = factor(set_all2$eea))
)


## -- extract number of unique entries per taxonomic level 
n_tax = list (
        phy      = lapply(all_typologies, function(x) lapply(x, function(y) y[                 ,uniqueN(phylum, na.rm =T)])),
        class    = lapply(all_typologies, function(x) lapply(x, function(y) y[!is.na(class),    uniqueN(class, na.rm =T)])),
        subclass = lapply(all_typologies, function(x) lapply(x, function(y) y[!is.na(subclass), uniqueN(subclass, na.rm =T)])),
        order    = lapply(all_typologies, function(x) lapply(x, function(y) y[!is.na(order),    uniqueN(order, na.rm =T)])),
        family   = lapply(all_typologies, function(x) lapply(x, function(y) y[!is.na(family),   uniqueN(family, na.rm =T)])),
        genus    = lapply(all_typologies, function(x) lapply(x, function(y) y[!is.na(genus),    uniqueN(genus, na.rm =T)]))
)


## --  prepare empty lists for loops below
a = vector(mode = "list", length = n_types["brt12"])
b = vector(mode = "list", length = n_types["brt20"])
c = vector(mode = "list", length = n_types["gloric"])
d = vector(mode = "list", length = n_types["illies"])
e = vector(mode = "list", length = n_types["eea"])

lvl_proto = list(brt12 = a, brt20 = b, gloric = c, illies = d, eea = e)

lvl_data = list(
        phyl = lvl_proto,
        class = lvl_proto,
        subclass = lvl_proto,
        order = lvl_proto,
        family = lvl_proto,
        genus = lvl_proto
)

for (i in seq_along(lvl_data)){
        for (k in 1:5){
                names(lvl_data[[i]][[k]]) = names(n_tax[[i]][[k]]) 
        }
        rm(i)
        gc()
}

rm(lvl_proto, a, b, c, d, e)
gc()



# Phylum ------------------------------------------------------------------

## -- loop over typologies 
for(typ in 1:5){
        
        ### ------------- ###
        ### --- PHYLUM -- ###
        ### ------------- ###
        
        ## -- LOOP OVER TYPES 
        
        for (k in seq_along(n_tax$phy[[typ]])) {
                level_data_phylum <-
                        data.table(
                                phylum_name     = character(n_tax$phy[[typ]][[k]]),
                                species           = numeric(n_tax$phy[[typ]][[k]]),
                                genus             = numeric(n_tax$phy[[typ]][[k]]),
                                family            = numeric(n_tax$phy[[typ]][[k]]),
                                order             = numeric(n_tax$phy[[typ]][[k]]),
                                subclass          = numeric(n_tax$phy[[typ]][[k]]),
                                class             = numeric(n_tax$phy[[typ]][[k]]),
                                phylum            = numeric(n_tax$phy[[typ]][[k]]),
                                n_observations    = numeric(n_tax$phy[[typ]][[k]])
                        )
                
                for (i in 1:n_tax$phy[[typ]][[k]]) {
                        level_data_phylum[i, phylum_name := all_typologies[[typ]][[k]][, unique(phylum)][i]]
                        
                        loop_sub <-
                                all_typologies[[typ]][[k]][phylum == all_typologies[[typ]][[k]][, unique(phylum)][i]]
                        loop_obs <- nrow(loop_sub)
                        level_data_phylum[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                        level_data_phylum[i, genus          := round(loop_sub[is.na(species) &
                                                                                      !is.na(genus), .N] / loop_obs * 100, 2)]
                        level_data_phylum[i, family         := round(loop_sub[is.na(species) &
                                                                                      is.na(genus) &
                                                                                      !is.na(family), .N] / loop_obs * 100, 2)]
                        level_data_phylum[i, order          := round(loop_sub[is.na(species) &
                                                                                      is.na(genus) &
                                                                                      is.na(family) &
                                                                                      !is.na(order), .N] / loop_obs * 100, 2)]
                        level_data_phylum[i, subclass       := round(loop_sub[is.na(species) &
                                                                                      is.na(genus) &
                                                                                      is.na(family) &
                                                                                      is.na(order) &
                                                                                      !is.na(subclass), .N] / loop_obs * 100, 2)]
                        level_data_phylum[i, class          := round(loop_sub[is.na(species) &
                                                                                      is.na(genus) &
                                                                                      is.na(family) &
                                                                                      is.na(order) &
                                                                                      is.na(subclass) &
                                                                                      !is.na(class), .N] / loop_obs * 100, 2)]
                        level_data_phylum[i, phylum         := round(loop_sub[is.na(species) &
                                                                                      is.na(genus) &
                                                                                      is.na(family) &
                                                                                      is.na(order) &
                                                                                      is.na(subclass) &
                                                                                      is.na(class) &
                                                                                      !is.na(phylum), .N] / loop_obs * 100, 2)]
                        level_data_phylum[i, n_observations := nrow(loop_sub)]
                        
                } # END LOOP i OVER PHYLA
                
                lvl_data$phyl[[typ]][[k]] = level_data_phylum
                rm(level_data_phylum)
                gc()
        
        } # END LOOP k OVER TYPES 
        
        ### ------------- ### 
        ### --- CLASS --- ### 
        ### ------------- ### 
        
        ## -- loop over all classes
        
        for (k in seq_along(n_tax$class[[typ]])) {
                level_data_class <- data.table(
                        phylum_name    = character(n_tax$class[[typ]][[k]]),
                        class_name     = character(n_tax$class[[typ]][[k]]),
                        species          = numeric(n_tax$class[[typ]][[k]]),
                        genus            = numeric(n_tax$class[[typ]][[k]]),
                        family           = numeric(n_tax$class[[typ]][[k]]),
                        order            = numeric(n_tax$class[[typ]][[k]]),
                        subclass         = numeric(n_tax$class[[typ]][[k]]),
                        class            = numeric(n_tax$class[[typ]][[k]]),
                        n_observations   = numeric(n_tax$class[[typ]][[k]])
                )
                ## -- LOOP OVER NUMBER OF CLASSES IN EACH TYPE
                for (i in 1:n_tax$class[[typ]][[k]]) {
                        
                        ## -- select a focal class for this iteration of k
                        loop_class <-
                                all_typologies[[typ]][[k]][!is.na(class), unique(class)][i]
                        ## -- enter this class as class name for row i
                        level_data_class[i, class_name :=  loop_class]
                        ## -- what is the phylum that this class belongs to?
                          # - check that there is only one phylum
                        unique_phylum = all_typologies[[typ]][[k]][class == loop_class, unique(phylum)]
                        if (length(unique_phylum)  != 1) {
                                error_message = paste(
                                        "There are",
                                        length(unique_phylum),
                                        "phyla for",
                                        loop_class,
                                        ": \n",
                                        paste(unique_phylum, collapse = " and ")
                                )
                                stop(error_message)
                        }
                        # - assign that phylum
                        level_data_class[i, phylum_name := all_typologies[[typ]][[k]][class == class_name, unique(phylum)]]
                        
                        loop_sub <- all_typologies[[typ]][[k]][class == loop_class]
                        loop_obs <- nrow(loop_sub)
                        
                        level_data_class[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                        level_data_class[i, genus          := round(loop_sub[is.na(species) &
                                                                                     !is.na(genus), .N] / loop_obs * 100, 2)]
                        level_data_class[i, family         := round(loop_sub[is.na(species) &
                                                                                     is.na(genus) &
                                                                                     !is.na(family), .N] / loop_obs * 100, 2)]
                        level_data_class[i, order          := round(loop_sub[is.na(species) &
                                                                                     is.na(genus) &
                                                                                     is.na(family) &
                                                                                     !is.na(order), .N] / loop_obs * 100, 2)]
                        level_data_class[i, subclass       := round(loop_sub[is.na(species) &
                                                                                     is.na(genus) &
                                                                                     is.na(family) &
                                                                                     is.na(order) &
                                                                                     !is.na(subclass), .N] / loop_obs * 100, 2)]
                        level_data_class[i, class          := round(loop_sub[is.na(species) &
                                                                                     is.na(genus) &
                                                                                     is.na(family) &
                                                                                     is.na(order) &
                                                                                     is.na(subclass) &
                                                                                     !is.na(class), .N] / loop_obs * 100, 2)]
                        level_data_class[i, n_observations := nrow(loop_sub)]
                        rm(loop_class)
                        if("unique_phylum" %in% ls()) rm(unique_phylum)
                        gc()
                }
                lvl_data$class[[typ]][[k]] = level_data_class
                rm(level_data_class)

        } # END OF LOOP K over all classes
        
        lvl_data$class[[typ]] =
                lapply(lvl_data$class[[typ]],
                       function (x)
                               setorderv(x,
                                         cols = c("phylum_name", "class_name")
                                         )
                       )
        ### ---------------- ###
        ### --- Subclass --- ### 
        ### ---------------- ###
        ## -- LOOP OVER SUBCLASSES         
        for (k in seq_along(n_tax$subclass[[typ]])) {
                level_data_subclass <- data.table(
                        phylum_name    = character(n_tax$subclass[[typ]][[k]]),
                        class_name     = character(n_tax$subclass[[typ]][[k]]),
                        subclass_name  = character(n_tax$subclass[[typ]][[k]]),
                        species          = numeric(n_tax$subclass[[typ]][[k]]),
                        genus            = numeric(n_tax$subclass[[typ]][[k]]),
                        family           = numeric(n_tax$subclass[[typ]][[k]]),
                        order            = numeric(n_tax$subclass[[typ]][[k]]),
                        subclass         = numeric(n_tax$subclass[[typ]][[k]]),
                        n_observations   = numeric(n_tax$subclass[[typ]][[k]])
                )
                for (i in 1:n_tax$subclass[[typ]][[k]]) {
                        loop_subclass <-
                                all_typologies[[typ]][[k]][!is.na(subclass), unique(subclass)][i]
                        
                        level_data_subclass[i, subclass_name := loop_subclass]
                        level_data_subclass[i, class_name    := all_typologies[[typ]][[k]][subclass == loop_subclass, unique(class)]]
                        level_data_subclass[i, phylum_name   := all_typologies[[typ]][[k]][subclass == loop_subclass, unique(phylum)]]
                        
                        loop_sub <- all_typologies[[typ]][[k]][subclass == loop_subclass]
                        loop_obs <- nrow(loop_sub)
                        
                        level_data_subclass[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                        level_data_subclass[i, genus          := round(loop_sub[is.na(species) &
                                                                                        !is.na(genus), .N] / loop_obs * 100, 2)]
                        level_data_subclass[i, family         := round(loop_sub[is.na(species) &
                                                                                        is.na(genus) &
                                                                                        !is.na(family), .N] / loop_obs * 100, 2)]
                        level_data_subclass[i, order          := round(loop_sub[is.na(species) &
                                                                                        is.na(genus) &
                                                                                        is.na(family) &
                                                                                        !is.na(order), .N] / loop_obs * 100, 2)]
                        level_data_subclass[i, subclass       := round(loop_sub[is.na(species) &
                                                                                        is.na(genus) &
                                                                                        is.na(family) &
                                                                                        is.na(order) &
                                                                                        !is.na(subclass), .N] / loop_obs * 100, 2)]
                        level_data_subclass[i, n_observations := nrow(loop_sub)]
                }
                lvl_data$subclass[[typ]][[k]] = level_data_subclass
                rm(level_data_subclass)
        }
        lvl_data$subclass[[typ]] =
                lapply(lvl_data$subclass[[typ]], function(x)
                        setorderv(x,
                                  cols = c(
                                          "phylum_name", "class_name", "subclass_name"
                                  )))
        
        ### ------------- ###
        ### --- ORDER --- ### 
        ### ------------- ###
        
        for (k in seq_along(n_tax$order[[typ]])) {
                level_data_order <- data.table(
                        phylum_name  = character(n_tax$order[[typ]][[k]]),
                        class_name   = character(n_tax$order[[typ]][[k]]),
                        subclass_name= character(n_tax$order[[typ]][[k]]),
                        order_name   = character(n_tax$order[[typ]][[k]]),
                        species        = numeric(n_tax$order[[typ]][[k]]),
                        genus          = numeric(n_tax$order[[typ]][[k]]),
                        family         = numeric(n_tax$order[[typ]][[k]]),
                        order          = numeric(n_tax$order[[typ]][[k]]),
                        n_observations = numeric(n_tax$order[[typ]][[k]])
                )
                for (i in 1:n_tax$order[[typ]][[k]]) {
                        loop_order <-  all_typologies[[typ]][[k]][!is.na(order), unique(order)][i]
                        
                        level_data_order[i, order_name    := loop_order]
                        level_data_order[i, subclass_name := all_typologies[[typ]][[k]][order == loop_order, unique(subclass)]]
                        level_data_order[i, class_name    := all_typologies[[typ]][[k]][order == loop_order, unique(class)]]
                        level_data_order[i, phylum_name   := all_typologies[[typ]][[k]][order == loop_order, unique(phylum)]]
                        
                        loop_sub <- all_typologies[[typ]][[k]][order == loop_order]
                        loop_obs <- nrow(loop_sub)
                        
                        level_data_order[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                        level_data_order[i, genus          := round(loop_sub[is.na(species) &
                                                                                     !is.na(genus), .N] / loop_obs * 100, 2)]
                        level_data_order[i, family         := round(loop_sub[is.na(species) &
                                                                                     is.na(genus) &
                                                                                     !is.na(family), .N] / loop_obs * 100, 2)]
                        level_data_order[i, order          := round(loop_sub[is.na(species) &
                                                                                     is.na(genus) &
                                                                                     is.na(family) &
                                                                                     !is.na(order), .N] / loop_obs * 100, 2)]
                        level_data_order[i, n_observations := nrow(loop_sub)]
                }
                lvl_data$order[[typ]][[k]] = level_data_order
                rm(level_data_order)
        }
        
        lvl_data$order[[typ]] = lapply(lvl_data$order[[typ]], function(x)
                setorderv(
                        x,
                        cols = c("phylum_name", "class_name", "subclass_name", "order_name")
                ))

        ### -------------- ###
        ### --- FAMILY --- ### 
        ### -------------- ###
        
        ## -- LOOP OVER FAMILIES
        for (k in seq_along(n_tax$family[[typ]])) {
                level_data_family <- data.table(
                        phylum_name  = character(n_tax$family[[typ]][[k]]),
                        class_name   = character(n_tax$family[[typ]][[k]]),
                        subclass_name= character(n_tax$family[[typ]][[k]]),
                        order_name   = character(n_tax$family[[typ]][[k]]),
                        family_name  = character(n_tax$family[[typ]][[k]]),
                        species        = numeric(n_tax$family[[typ]][[k]]),
                        genus          = numeric(n_tax$family[[typ]][[k]]),
                        family         = numeric(n_tax$family[[typ]][[k]]),
                        n_observations = numeric(n_tax$family[[typ]][[k]])
                )
                for (i in 1:n_tax$family[[typ]][[k]]) {
                        loop_family <- all_typologies[[typ]][[k]][!is.na(family), unique(family)][i]
                        
                        level_data_family[i, family_name   := loop_family]
                        level_data_family[i, order_name    := all_typologies[[typ]][[k]][family == loop_family, unique(order)]]
                        level_data_family[i, subclass_name := all_typologies[[typ]][[k]][family == loop_family, unique(subclass)]]
                        level_data_family[i, class_name    := all_typologies[[typ]][[k]][family == loop_family, unique(class)]]
                        level_data_family[i, phylum_name   := all_typologies[[typ]][[k]][family == loop_family, unique(phylum)]]
                        
                        loop_sub <- all_typologies[[typ]][[k]][family == loop_family]
                        loop_obs <- nrow(loop_sub)
                        
                        level_data_family[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                        level_data_family[i, genus          := round(loop_sub[is.na(species) &
                                                                                      !is.na(genus), .N] / loop_obs * 100, 2)]
                        level_data_family[i, family         := round(loop_sub[is.na(species) &
                                                                                      is.na(genus) &
                                                                                      !is.na(family), .N] / loop_obs * 100, 2)]
                        level_data_family[i, n_observations := nrow(loop_sub)]
                }
                lvl_data$family[[typ]][[k]] = level_data_family
                rm(level_data_family)
        }
        
        lvl_data$family[[typ]] = lapply(lvl_data$family[[typ]], function(x)
                setorderv(
                        x,
                        cols = c(
                                "phylum_name",
                                "class_name",
                                "subclass_name",
                                "order_name",
                                "family_name"
                        )
                ))
        
        ### ------------- ###
        ### --- GENUS --- ###
        ### ------------- ###
        
        ## -- LOOP OVER TYPES
        
        for (k in seq_along(n_tax$genus[[typ]])) {
                level_data_genus <- data.table(
                        phylum_name    = character(n_tax$genus[[typ]][[k]]),
                        class_name     = character(n_tax$genus[[typ]][[k]]),
                        subclass_name  = character(n_tax$genus[[typ]][[k]]),
                        order_name     = character(n_tax$genus[[typ]][[k]]),
                        family_name    = character(n_tax$genus[[typ]][[k]]),
                        genus_name     = character(n_tax$genus[[typ]][[k]]),
                        species          = numeric(n_tax$genus[[typ]][[k]]),
                        genus            = numeric(n_tax$genus[[typ]][[k]]),
                        n_observations   = numeric(n_tax$genus[[typ]][[k]])
                )
                
                ## -- LOOP OVER GENERA
                
                for (i in 1:n_tax$genus[[typ]][[k]]) {
                        loop_genus <- all_typologies[[typ]][[k]][!is.na(genus), unique(genus)][i]
                        
                        level_data_genus[i, genus_name    := loop_genus]
                        level_data_genus[i, family_name   := all_typologies[[typ]][[k]][genus == loop_genus, unique(family)]]
                        level_data_genus[i, order_name    := all_typologies[[typ]][[k]][genus == loop_genus, unique(order)]]
                        level_data_genus[i, subclass_name := all_typologies[[typ]][[k]][genus == loop_genus, unique(subclass)]]
                        level_data_genus[i, class_name    := all_typologies[[typ]][[k]][genus == loop_genus, unique(class)]]
                        level_data_genus[i, phylum_name   := all_typologies[[typ]][[k]][genus == loop_genus, unique(phylum)]]
                        
                        loop_sub <- all_typologies[[typ]][[k]][genus == loop_genus]
                        loop_obs <- nrow(loop_sub)
                        
                        level_data_genus[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                        level_data_genus[i, genus          := round(loop_sub[is.na(species) &
                                                                                     !is.na(genus), .N] / loop_obs * 100, 2)]
                        level_data_genus[i, n_observations := nrow(loop_sub)]
                        print(paste(i, n_tax$genus[[typ]][[k]]))
                } # END LOOP I OVER GENERA 
                
                lvl_data$genus[[typ]][[k]] = level_data_genus
                rm(level_data_genus)
                gc()
                
        } # END LOOP K OVER TYPES  
        
        lvl_data$genus[[typ]] = lapply(lvl_data$genus[[typ]], function(x)
                setorderv(
                        x,
                        cols = c(
                                "phylum_name",
                                "class_name",
                                "subclass_name",
                                "order_name",
                                "family_name",
                                "genus_name"
                        )
                )
        )

} # END LOOP typ OVER typologies 



# Save to file  -----------------------------------------------------------

saveRDS(lvl_data, "data/summer season/05_taxon_resolution_list_all_typologies.rds")
