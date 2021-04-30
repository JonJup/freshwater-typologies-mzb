### ------------------------ ###
### --- ADD TRAITS      ---- ### 
### --- TO               --- ### 
### --- INDICATOR TAXA   --- ### 
### ------------------------ ###

# --------------- #
# date:  
#               23.03.21
# files in:
#               15_indicator_list.rds                  | List of indicator genera for each river type 
#               Trait_freshecol_2020_pp_harmonized.rds | Harmonized trait data base provided by Stefan Kunz 
# files out:
#               
# Project: 
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose: 
#               Add traits to indicator taxa 
# --------------- #

# setup -----------------------------------------------
source("R/setup.R")

# load data -------------------------------------------
ind   = readRDS("data/15_indicator_list.rds")
trait = readRDS("data/Trait_freshecol_2020_pp_harmonized.rds")

# prepare data ----------------------------------------
ind_taxa        = lapply(ind, function(x) x$taxon)
names(ind_taxa) = paste0("RT", c(1:5, 8:11, 14:16,18))
all_taxa        = unique(unlist(ind_taxa))

# are all taxa represented? 
all(all_taxa %in% trait$genus) 
# -> no 
# which ones are missing? 
missing = all_taxa[which(!all_taxa %in% trait$genus)]
length(missing)
#58 genera. That's ...
length(missing)/length(all_taxa)
# .. 13.5 % 

# create master trait data  --------------------------------------------
## -- Master table with one row per genus 
master = data.table(genus = sort(all_taxa))
## -- check genera in trait data 
unique(trait$genus)
## -- there is some suffixes like numbers, sp., or alternative names. 
## -- I will remove all of them by sub-setting to the first word.
trait[, genus := word(trait$genus, 1)]
## -- there is also a taxon_cp variable which shows the best resolved name for a given row. 
## -- Many taxa are entered as genus sp. (e.g. Ephydatia sp.). I will drop the sp. to make a join possible. 
trait[,taxon_cp := str_replace(taxon_cp, "\\ sp\\.", "")]
## -- add an ID column. This will be used to join columns later. 
trait[, ID := 1:.N]
master[trait, on = c("taxon_cp"), ID_cp  := ID]
## -- subset of traits where species is missing                                               
 rait_mod = trait[genus %in% all_taxa & is.na(species)]
## -- same join again 
master[trait_mod, on = c("genus"), ID_genus  := ID]

## -- which taxa are still not accounted for? 
id = which(master[, apply(.SD, 1, function(x) sum(is.na(x)))] == 2)
missing_taxa = master$genus[id]
 
## --                                
missing_info = classification(missing_taxa, db = "gbif")

missing_info2 =
        map_df(
                .x = 1:length(missing_info),
                .f = ~ data.table(
                        genus  = missing_info[[.x]][which(missing_info[[.x]][, 2] == "genus"), 1],
                        family = missing_info[[.x]][which(missing_info[[.x]][, 2] == "family"), 1],
                        order  = missing_info[[.x]][which(missing_info[[.x]][, 2] == "order"), 1]
                )
        ) %>%
        setDT %>%
        setorderv(c("order", "family", "genus")) 

## -- Family level data is missing for Zygoptera. 
## -- Zygoptera is no genus, but a suborder. It will be removed from further analyses. 
missing_info2 = missing_info2[genus != "Zygoptera"]
master = master[genus != "Zygoptera"]

## -- Order level information is missing from the families: Capitellidae, 
## -- Lymnaeidae, Melanopsidae and Planorbidae.
## -- Capitellidae is a family of Polychaeta. Wikipedia assigns Capitellida.
## -- Are the two most common genera (according to Wikipedia) present in the data? 
trait[genus == "Capitella"]
trait[genus == "Heteromastus"]
trait[family == "Capitellidae"]
## -- No. Neither is any member of the family. Must be removed. 
missing_info2 = missing_info2[genus != "Capitella"]
master = master[genus != "Capitella"]
## -- Lymnaeidae, Melanopsidae and Planorbidae are snails which used to belong
## -- to the no longer accepted order of Pulmonata 

# loop over entries of missing info to assign id. 
for (i in 1:nrow(missing_info2)) {
        ## extract new family 
        lp_fm = missing_info2$family[i]
        ## check if new family is in trait data 
        family_in_traits = case_when(
                lp_fm %in% trait$taxon_cp ~ 1,
                lp_fm %in% trait$family ~ 2,
                TRUE ~ 3
        )
        if (family_in_traits == 3){
                next()
        } else if (family_in_traits == 2){
                lp_fm2 = trait[family == lp_fm]
                lp_fm3 = copy(lp_fm2)
                lp_fm3[, c(1:7, 36,37) := NULL]
                lp_fm3 = lp_fm3[,lapply(.SD, median, na.rm = TRUE)]
                lp_new_data =  data.table(species = NA, 
                                          genus = NA, 
                                          family = lp_fm, 
                                          order = NA, 
                                          taxon_cp = lp_fm,
                                          taxa = lp_fm,
                                          ID_AQEM = NA
                                          )
                lp_new_data = cbind(lp_new_data, lp_fm3)
                lp_new_data[, c("taxa_adjusted", "ID") := .(lp_fm, 
                                                            max(trait$ID) + 1)]
                trait = rbind(trait, lp_new_data)
                master[genus == missing_info2$genus[i], ID_family := trait[taxon_cp == lp_fm, "ID"]]
        } else if (family_in_traits == 1){
                master[genus == missing_info2$genus[i], ID_family := trait[taxon_cp == lp_fm, "ID"]]
        }
        
        rm(list = ls()[grepl(pattern = "^lp_", x = ls())])
        rm(family_in_traits)
        gc()
}

## -- which taxa are still not accounted for? 
id = which(master[, apply(.SD, 1, function(x) sum(is.na(x)))] == 3)
missing_taxa = master$genus[id]

missing_info3 = copy(missing_info2)
missing_info3 = missing_info3[genus %in% missing_taxa]

## -- 12 taxa remain which is equal to 
12/length(all_taxa)
## -- ... 3% of the taxa. 
## -- For these 12 taxa the families are not entered in the trait data base and 
## -- they will be removed from further consideration
master = master[!genus %in% missing_info3$genus]

## -- create final ID used to join tables 
master[, ID_family := as.integer(ID_family)]
master[, ID_final := case_when(!is.na(ID_cp) ~ ID_cp,
                               is.na(ID_cp) & !is.na(ID_genus) ~ ID_genus,
                               TRUE ~ ID_family)] 

master2 = copy(master)
master2 %<>% dplyr::select(!starts_with("ID"))
master2[, ID := master$ID_final]
master2[trait, on = "ID",
        str_subset(string = names(trait), pattern = c("feed|resp|volt|locom|ovip|size|bf")) 
        := 
        .(feed_shredder, feed_gatherer, feed_predator, feed_parasite, feed_filter,   feed_herbivore,
          resp_teg,      resp_gil,      resp_pls_spi,  volt_semi,     volt_uni,      volt_bi_multi,
          locom_burrow,  locom_sessil,  locom_swim,    locom_crawl,   ovip_aqu,      ovip_ovo,     
          ovip_ter,      size_medium,   size_small,    size_large,    bf_flattened,  bf_spherical, 
          bf_cylindrical,bf_streamlined)
        ]


 
# Assign traits to river types  -------------------------------------------------------------------------
ind_taxa2 = lapply(ind_taxa, function(x)
        data.table(genus = x))
ind_taxa2 %<>% map(.f = ~ master2[.x, on = "genus"])
ind_taxa2 = map(.x = 1:length(ind_taxa2),
                .f = ~ ind_taxa2[[.x]][, river_type := names(ind_taxa2)[.x]])
ind_taxa3 = rbindlist(ind_taxa2)

ind_taxa3[, final_taxon := genus]
ind_taxa3[, c("genus", "ID") := NULL]

## -- Which traits are always NA? 
drop_cols = 
        ind_taxa3[, lapply(.SD, function(x) all(is.na(x)))] %>% 
        c() %>% 
        unlist() %>% 
        which() %>% 
        names()

if (length(drop_cols) > 0)
        ind_taxa3[, (drop_cols) := NULL]


# save data -------------------------------------------
saveRDS(ind_taxa3, "data/16_indicator_taxa_w_traits.rds")

