### ---------------------- ###
### --- ADD TRAITS    ---- ### 
### --- TO             --- ### 
### --- TYPICAL TAXA   --- ### 
### ---------------------- ###

# --------------- #
# date:  23.03.21
# files in: 
#               -> Trait_freshecol_2020_pp_harmonized.rds | Harmonized trait data base provided by Stefan Kunz 
#               -> 08_typical_assemblages.rds             | data table with typical assemblages. 
# files out
#               <- 17_typical_assemblages_w_traits.rds
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#               Add traits to typical assemblages
# --------------- #

  

# setup -----------------------------------------------
source("R/setup.R")

# load data -------------------------------------------
trait = readRDS("data/Trait_freshecol_2020_pp_harmonized.rds")
typas = readRDS("data/08_typical_assemblages.rds")



# prepare data ----------------------------------------
all_taxa <-
        typas %>% 
        .[level != "fol", "taxon"] %>% 
        pull() %>% 
        unique() %>% 
        str_replace_all("\\.", "\\ ") %>% 
        sort

# add ID to traits - this makes it easier to check which taxa have a row in the
# trait data assigned to them, before actually joining the data. Due to the high
# number of columns in the trait data, it become harder to see once the data are
# joined. 
trait[, my_id := .I]

# Are all taxa represented?
all(all_taxa %in% c(trait$genus, trait$species, trait$family, trait$taxon_cp))
# -> no 
# which ones are missing? 
missing = all_taxa[which(!all_taxa %in% c(trait$genus, trait$species, trait$family, trait$taxon_cp))]
## -- 9 taxa are not included in the trait data. 
9/length(all_taxa)
## -- That's 15 % of all taxa. 
## -- Most are species (8). The only exception is Oligochaeta. 
## -- I will check later (line: 78) whether genus level info is available.  


# create master trait data  --------------------------------------------
master = data.table(taxon = all_taxa, 
                    genus_from_species = str_remove_all(all_taxa, "\\ .*"))

## -- clean suffixes from taxa names in trait 
trait[,genus := word(genus, 1 )]
trait[,family := word(family, 1 )]
trait[,taxon_cp := word(taxon_cp, 1 )]

## -  create trait subsets for taxonomic levels to join below 
trait_species = trait[!is.na(species),                                 c("species", "my_id")]
trait_genus   = trait[is.na(species) & !is.na(genus),                  c("genus", "my_id")]
#trait_family  = trait[is.na(species) &  is.na(genus) & !is.na(family), c("family", "my_id")]
trait_taxon  = trait[, c("taxon_cp", "my_id")]

## -- join master with traits
master[trait_species, on = c("taxon" = "species"), ID := my_id]
master[trait_genus,   on = c("taxon" = "genus"  ), ID := my_id]
#master[trait_family,  on = c("taxon" = "family" ), ID := my_id]
master[trait_taxon,  on = c("taxon" = "taxon_cp" ), ID_tax := my_id]
master[, ID := ifelse(is.na(ID), ID_tax, ID)]
master[, ID_tax := NULL]

#rm(trait_species, trait_genus, trait_family, trait_taxon);gc()
rm(trait_species, trait_genus, trait_taxon);gc()

## -- Fix missing taxa
missing_taxa = master[is.na(ID), taxon]
missing_info = classification(missing_taxa, db = "gbif")

missing_info2 =
        map_df(
                .x = 1:length(missing_info),
                .f = ~ data.table(
                        species  = missing_info[[.x]][which(missing_info[[.x]][, 2] == "species"), 1],
                        genus  = missing_info[[.x]][which(missing_info[[.x]][, 2] == "genus"), 1],
                        family = missing_info[[.x]][which(missing_info[[.x]][, 2] == "family"), 1],
                        order  = missing_info[[.x]][which(missing_info[[.x]][, 2] == "order"), 1]
                )
        ) %>%
        setDT %>%
        setorderv(c("order", "family", "genus")) 

for (i in 1:nrow(missing_info2)) {
        ## extract new family 
        lp_gen = missing_info2$genus[i]
        ## check if new family is in trait data 
        genus_in_traits = case_when(
                lp_gen %in% trait$taxon_cp ~ 1,
                lp_gen %in% trait$genus ~ 2,
                TRUE ~ 3
        )
        if (genus_in_traits == 3){
                next()
        } else if (genus_in_traits == 2){
                lp_gen2 = trait[genus == lp_gen]
                lp_gen3 = copy(lp_gen2)
                lp_gen3[, c(1:7, 36,37) := NULL]
                lp_gen3 = lp_gen3[,lapply(.SD, median, na.rm = TRUE)]
                lp_new_data =  data.table(species = NA, 
                                          genus = lp_gen, 
                                          family = NA, 
                                          order = NA, 
                                          taxon_cp = lp_gen,
                                          taxa = lp_gen,
                                          ID_AQEM = NA
                )
                lp_new_data = cbind(lp_new_data, lp_gen3)
                lp_new_data[, c("taxa_adjusted", "my_id") := .(lp_gen, 
                                                            max(trait$my_id) + 1)]
                trait = rbind(trait, lp_new_data)
                master[genus == missing_info2$genus[i], ID_family := trait[taxon_cp == lp_gen, "my_id"]]
        } else if (genus_in_traits == 1){
                
                if (nrow(trait[taxon_cp == lp_gen, "my_id"]) > 1){
                        lp_gen2 = trait[taxon_cp == lp_gen]
                        lp_gen3 = copy(lp_gen2)
                        lp_gen3[, c(1:7, 36,37) := NULL]
                        lp_gen3 = lp_gen3[,lapply(.SD, median, na.rm = TRUE)]
                        lp_new_data =  data.table(species = NA, 
                                                  genus = lp_gen, 
                                                  family = NA, 
                                                  order = NA, 
                                                  taxon_cp = lp_gen,
                                                  taxa = lp_gen,
                                                  ID_AQEM = NA
                        )
                        lp_new_data = cbind(lp_new_data, lp_gen3)
                        lp_new_data[, c("taxa_adjusted", "my_id") := .(lp_gen, 
                                                                       max(trait$my_id) + 1)]
                        trait = rbind(trait, lp_new_data)
                        master[genus_from_species == missing_info2$genus[i], 
                               ID_genus := max(trait$my_id)]
                } else {
                        master[genus_from_species == missing_info2$genus[i], 
                               ID_genus := trait[taxon_cp == lp_gen, "my_id"]]         
                }
                
               
        }
        
        rm(list = ls()[grepl(pattern = "^lp_", x = ls())])
        rm(genus_in_traits)
        gc()
}
master[is.na(ID), ID := ID_genus]
missing_taxa = master[is.na(ID), taxon]
missing_info2 = missing_info2[species %in% missing_taxa]

## remove missing taxa 
master = master[!taxon %in% missing_info2$species]


## -- three are still missing. Try family level

# for (i in 1:nrow(missing_info2)) {
#         ## extract new family 
#         lp_gen = missing_info2$family[i]
#         ## check if new family is in trait data 
#         genus_in_traits = case_when(
#                 lp_gen %in% trait$taxon_cp ~ 1,
#                 lp_gen %in% trait$family ~ 2,
#                 TRUE ~ 3
#         )
#         if (genus_in_traits == 3){
#                 next()
#         } else if (genus_in_traits == 2){
#                 lp_gen2 = trait[family == lp_gen]
#                 lp_gen3 = copy(lp_gen2)
#                 lp_gen3[, c(1:7, 36,37) := NULL]
#                 lp_gen3 = lp_gen3[,lapply(.SD, median, na.rm = TRUE)]
#                 lp_new_data =  data.table(species = NA, 
#                                           genus = NA, 
#                                           family = lp_gen, 
#                                           order = NA, 
#                                           taxon_cp = lp_gen,
#                                           taxa = lp_gen,
#                                           ID_AQEM = NA
#                 )
#                 lp_new_data = cbind(lp_new_data, lp_gen3)
#                 lp_new_data[, c("taxa_adjusted", "my_id") := .(lp_gen, 
#                                                                max(trait$my_id) + 1)]
#                 trait = rbind(trait, lp_new_data)
#                 master[taxon == missing_info2$species[i], ID_family := trait[taxon_cp == lp_gen, "my_id"]]
#         } else if (genus_in_traits == 1){
#                 
#                 if (nrow(trait[taxon_cp == lp_gen, "my_id"]) > 1){
#                         lp_gen2 = trait[taxon_cp == lp_gen]
#                         lp_gen3 = copy(lp_gen2)
#                         lp_gen3[, c(1:7, 36,37) := NULL]
#                         lp_gen3 = lp_gen3[,lapply(.SD, median, na.rm = TRUE)]
#                         lp_new_data =  data.table(species = NA, 
#                                                   genus = NA, 
#                                                   family = lp_gen, 
#                                                   order = NA, 
#                                                   taxon_cp = lp_gen,
#                                                   taxa = lp_gen,
#                                                   ID_AQEM = NA
#                         )
#                         lp_new_data = cbind(lp_new_data, lp_gen3)
#                         lp_new_data[, c("taxa_adjusted", "my_id") := .(lp_gen, 
#                                                                        max(trait$my_id) + 1)]
#                         trait = rbind(trait, lp_new_data)
#                         master[taxon == missing_info2$species[i], 
#                                ID_family := max(trait$my_id)]
#                 } else {
#                         master[genus_from_species == missing_info2$genus[i], 
#                                ID_family := trait[taxon_cp == lp_gen, "my_id"]]         
#                 }
#                 
#                 
#         }
#         
#         rm(list = ls()[grepl(pattern = "^lp_", x = ls())])
#         rm(genus_in_traits)
#         gc()
# }
# master[is.na(ID), ID := ID_family]
# missing_taxa = master[is.na(ID), taxon]

## -- join to trait data 
master[trait, on = c("ID" = "my_id"), 
        str_subset(string = names(trait), pattern = c("feed|resp|volt|locom|ovip|size|bf")) 
        := 
                .(feed_shredder, feed_gatherer, feed_predator, feed_parasite, feed_filter,   feed_herbivore,
                  resp_teg,      resp_gil,      resp_pls_spi,  volt_semi,     volt_uni,      volt_bi_multi,
                  locom_burrow,  locom_sessil,  locom_swim,    locom_crawl,   ovip_aqu,      ovip_ovo,     
                  ovip_ter,      size_medium,   size_small,    size_large,    bf_flattened,  bf_spherical, 
                  bf_cylindrical,bf_streamlined)
]


# Assign traits to river types  -------------------------------------------------------------------------
typas[, taxon := str_replace(taxon, "\\.", "\\ ")]
typas2 = master[typas, on = "taxon"]

# Feature engineering -----------------------------------------------------
typas2[, c("A", "B", "mechanism", "level", "ID", "ID_genus") := NULL]
# Which traits are always NA? 
drop_cols = typas2[, lapply(.SD, function(x) all(is.na(x)))] %>% 
        c() %>% 
        unlist() %>% 
        which() %>% 
        names()
if(length(drop_cols)>0)
        typas2[, (drop_cols) := NULL]

# save data -------------------------------------------
saveRDS(typas2, "data/17_typical_assemblages_w_traits.rds")

