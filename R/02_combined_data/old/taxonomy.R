### —————————————————————————— ###
### ——— harmonize Taxonomy ——— ### 
### —————————————————————————— ###

# ————————————————
# date:
#       23.07.21
# files in: 
#       <- 
# files out:
#
# Project:
# 
# Purpose:
#  
# ————————————————

# TEST 
# Dont Repeat Yourself  
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# Design by Contract 
# Fail fast/ defensive programming 

# setup -----------------------------------------------
pacman::p_load(sf, data.table, dplyr, mapview, parallelDist, tidyr)
# load data -------------------------------------------
data <- readRDS("data/combined_data/01_2021-07-26_combined_data.rds")

# taxonomy --------------------------------------------
ukingdom <- unique(data$kingdom)
uphyllum <- unique(data$phylum)
data[, uniqueN(data.set), by = "phylum"]
data[family == "Stratiomyidae", phylum := "Arthropoda"]
data[lowest.taxon == "Stratiomyiidae", c("family", "order", "class", "phylum") := .("Stratiomyidae", "Diptera", "Insecta", "Arthropoda")]
data[is.na(phylum), unique(lowest.taxon)]
data <- data[phylum %in% c("Arthropoda", "Mollusca")]
data[, uniqueN(data.set), by = "class"]
#- taxa have class NA 
data[is.na(class), unique(lowest.taxon)]
data <- data[lowest.taxon != "Mollusca"]
data[order == "Trombidiformes", class := "Arachnida"]
data[order == "Araneae", class := "Arachnida"]
data <- data[class %in% c("Insecta", "Gastropoda", "Bivalvia", "Malacostraca", "Arachnida", NA)]
data <- data[order %in% c("Trombidiformes",
                          "Megaloptera",
                          "Lepidoptera",
                          "Isopoda",
                          "Littorinimorpha",
                          "Odonata",
                          "Sphaeriida",
                          "Hemiptera",
                          "Amphipoda",
                          "Coleoptera",
                          "Trichoptera",
                          "Diptera",
                          "Ephemeroptera",
                          NA,
                          "Plecoptera")]

n_tax = list (
        phy      = data[                 ,uniqueN(phylum, na.rm =T)],
        class    = data[!is.na(class),    uniqueN(class, na.rm =T)],
        subclass = data[!is.na(subclass), uniqueN(subclass, na.rm =T)],
        order    = data[!is.na(order),    uniqueN(order, na.rm =T)],
        family   = data[!is.na(family),   uniqueN(family, na.rm =T)],
        genus    = data[!is.na(genus),    uniqueN(genus, na.rm =T)])
)

uord <- unique(data$order)
for (k in 1:n_tax$order) {
        if (k == 1) {
                level_data_order <- data.table(
                        order_name   = character(n_tax$order[[k]]),
                        species        = numeric(n_tax$order[[k]]),
                        genus          = numeric(n_tax$order[[k]]),
                        family         = numeric(n_tax$order[[k]]),
                        order          = numeric(n_tax$order[[k]]),
                        n_observations = numeric(n_tax$order[[k]])
                )
        }
        loop_order <-  uord[k]
        level_data_order[k, order_name := loop_order]
        loop_sub <- data[order == loop_order]
        loop_obs <- nrow(loop_sub)
        level_data_order[k, species := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
        level_data_order[k, genus   := round(loop_sub[is.na(species) & !is.na(genus), .N] / loop_obs * 100, 2)]
        level_data_order[k, family  := round(loop_sub[is.na(species) & is.na(genus) &!is.na(family), .N] / loop_obs * 100, 2)]
        level_data_order[k, order   := round(loop_sub[is.na(species) & is.na(genus) & is.na(family) & !is.na(order), .N] / loop_obs * 100, 2)]
        level_data_order[k, n_observations := nrow(loop_sub)]
}
ufam <- unique(data$family)
for (k in 1:n_tax$family) {
        if (k == 1) {
                level_data_family <- data.table(
                        family_name   = character(n_tax$family),
                        species        = numeric(n_tax$family),
                        genus          = numeric(n_tax$family),
                        family         = numeric(n_tax$family),
                        n_observations = numeric(n_tax$family)
                )
        }
        loop_fam <-  ufam[k]
        level_data_family[k, family_name := loop_fam]
        loop_sub <- data[family == loop_fam]
        loop_obs <- nrow(loop_sub)
        level_data_family[k, species := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
        level_data_family[k, genus   := round(loop_sub[is.na(species) & !is.na(genus), .N] / loop_obs * 100, 2)]
        level_data_family[k, family  := round(loop_sub[is.na(species) & is.na(genus) &!is.na(family), .N] / loop_obs * 100, 2)]
        level_data_family[k, n_observations := nrow(loop_sub)]
}
setorderv(level_data_family, cols = c("family_name"))
ugen <- unique(data$genus)
for (k in 1:n_tax$genus) {
        if (k == 1) {
                level_data_genus <- data.table(
                        genus_name    = character(n_tax$genus),
                         species        = numeric(n_tax$genus),
                         genus          = numeric(n_tax$genus),
                         family         = numeric(n_tax$genus),
                         n_observations = numeric(n_tax$genus)
                )
        }
        loop_gen <-  ugen[k]
        level_data_genus[k, genus_name := loop_gen]
        loop_sub <- data[genus == loop_gen]
        loop_obs <- nrow(loop_sub)
        level_data_genus[k, species := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
        level_data_genus[k, genus   := round(loop_sub[is.na(species) & !is.na(genus), .N] / loop_obs * 100, 2)]
        level_data_genus[k, n_observations := nrow(loop_sub)]
}
setorderv(level_data_genus, cols = c("genus_name"))



# SAVE DATA TO FILE -------------------------------------------------------------------------
saveRDS(sxs.genus.pa, paste0("data/combined_data/03_",Sys.Date(),"_genus_rel_abu.rds"))
saveRDS(sxs.genus.rel.abu, paste0("data/combined_data/03_",Sys.Date(),"_genus_presence.rds"))
saveRDS(data.genus, paste0("data/combined_data/03_", Sys.Date(),"_genus_data.rds"))
