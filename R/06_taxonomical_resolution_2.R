# ----------------------------------- #
### --- Taxonomical Resolution 2--- ### 
# ----------------------------------- #

# --------------- #
# files in 
        #-> 04_invertebrates_w_typologies.rds             | macroinvertebrate observations from reference sites 
        #-> 04_taxon_resolution_list_all_typologies.rds   | percent of taxa that are represented at different levels 
# files out
        #<- 04_taxon_resolution_list.rds
#  more taxonomic cleaning 
# --------------- #

# Setup -------------------------------------------------------------------
source("R/setup_combined_inv.R")
rm(fill_season, plot_list, prepare_plot, subset_with_sites)
# data IO  ----------------------------------------------------------------
set_all = readRDS("data/04_invertebrates_w_typologies.rds")
ls_res  = readRDS("data/04_taxon_resolution_list_all_typologies.rds")

## -- add x and y coordinates as variables to extract unique sites 
set_all_sf  <- st_as_sf(set_all)
set_all_sf$x_coord <- st_coordinates(set_all_sf)[,1]
set_all_sf$y_coord <- st_coordinates(set_all_sf)[,2]
set_all <- set_all_sf
setDT(set_all)

## -- reduce to adequately represented river types
brt20_acc    = paste0("RT", c(1,2,3,4,5,8,9, 10, 11, 14, 15, 16, 18))
brt12_acc    = paste0("RT", c(1,2,3, 4, 5, 6, 7, 8, 9, 10))
set_all = set_all[brt12 %in% brt12_acc & brt20 %in% brt20_acc]


## -- split into one data frame per type 
all_typologies = list(
        brt12 =  split.data.frame(x = set_all, f = factor(set_all$brt12)),
        brt20 =  split.data.frame(x = set_all, f = factor(set_all$brt20)),
        gloric = split.data.frame(x = set_all, f = factor(set_all$gloric)),
        illies = split.data.frame(x = set_all, f = factor(set_all$illies)),
        bgr =    split.data.frame(x = set_all, f = factor(set_all$eea))
)

list_all2 = list(
        brt12 =   vector(mode="list",  length = uniqueN(set_all$brt12)),
        brt20 =   vector(mode="list", length = uniqueN(set_all$brt20)),
        gloric =  vector(mode="list", length = uniqueN(set_all$gloric)),
        illies =  vector(mode="list", length = uniqueN(set_all$illies)),
        bgr =     vector(mode="list", length = uniqueN(set_all$eea))
)
names(list_all2$brt12) = unique(set_all$brt12)
names(list_all2$brt20) = unique(set_all$brt20)
names(list_all2$gloric) = as.character(unique(set_all$gloric))
names(list_all2$illies) = unique(set_all$illies)
names(list_all2$bgr) = unique(set_all$eea)

# Create Lists  -----------------------------------------------------------
# Phyla to keep 
phyl_keep = c("Annelida", "Arthropoda", "Mollusca")
# Classes to keep 
class_keep = c("Clitellata","Insecta","Malacostraca","Bivalvia","Gastropoda")

# recent required to go to higher taxonomic levels. 
required_percent = 50


## -- LOOP OVER TYPOLOGIES 
for (typ in 1:5){
        
        ## -- character vector with types of this loops typology 
        lptyp_typ = unique(names(all_typologies[[typ]]))
        lptyp_data = all_typologies[[typ]]
        
        ## -- LOOP OVER TYPES 
        for (k in seq_along(lptyp_typ)){
                
                ### ------------- ###
                ### --- SETUP --- ### 
                ### ------------- ###
                
                print(paste("## -- STARTING ", lptyp_typ[k], "-- ##"))
                #list_all_loop_id = which(names(all_typologies[[k]]) == lptyp_typ[k])
                lp_k_res_id        = which(names(ls_res$phyl[[typ]]) == lptyp_typ[k])
                
                lp_k_set_all = lptyp_data[[k]][phylum %in% phyl_keep | is.na(phylum)]  
                lp_k_set_all = lp_k_set_all[class %in% class_keep | is.na(class)]  
                lp_k_set_all$final_taxon = character()
                lp_k_set_all$final_taxon_level = character()
                
                ### ---------------- ###
                ### --- SUBCLASS --- ###
                ### ---------------- ###
                lp_k_subclass_id   = which(ls_res$subclass[[typ]][[lp_k_res_id]][           , (species + genus + family + order) < required_percent])
                lp_k_subclass_taxa =       ls_res$subclass[[typ]][[lp_k_res_id]][lp_k_subclass_id, subclass_name]
                lp_k_set_all[subclass %in% lp_k_subclass_taxa & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(subclass, "subclass")]
                
                ### ------------- ###
                ### --- ORDER --- ### 
                ### ------------- ###
                lp_k_order_id   = which(ls_res$order[[typ]][[lp_k_res_id]][                , (species + genus + family) < required_percent])
                lp_k_order_taxa =       ls_res$order[[typ]][[lp_k_res_id]][lp_k_order_id, order_name]
                lp_k_set_all[order %in% lp_k_order_taxa & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(order, "order")]
                
                ### -------------- ###
                ### --- FAMILY --- ###
                ### -------------- ###
                lp_k_family_id   = which(ls_res$family[[typ]][[lp_k_res_id]][                , (species + genus) < required_percent])
                lp_k_family_taxa =       ls_res$family[[typ]][[lp_k_res_id]][lp_k_family_id, family_name]
                lp_k_set_all[family %in% lp_k_family_taxa & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(family, "family")]
                
                ### ------------- ###
                ### --- GENUS --- ### 
                ### ------------- ###
                lp_k_genus_id   = which(ls_res$genus[[typ]][[lp_k_res_id]][                , (species) < required_percent])
                lp_k_genus_taxa =       ls_res$genus[[typ]][[lp_k_res_id]][lp_k_genus_id, genus_name]
                lp_k_set_all[genus %in% lp_k_genus_taxa & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(genus, "genus")]
                
                ### --------------- ###
                ### --- SPECIES --- ### 
                ### --------------- ###
                lp_k_species_id   = which(ls_res$genus[[typ]][[lp_k_res_id]][                , species >= required_percent])
                lp_k_species_taxa =       ls_res$genus[[typ]][[lp_k_res_id]][lp_k_species_id, genus_name]
                lp_k_set_all[genus %in% lp_k_species_taxa & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(species, "species")]
                
                ### ---------------- ###
                ### --- CLEAN UP --- ###
                ### ---------------- ###
                lp_k_set_all[final_taxon_level == "species" & is.na(final_taxon) & !is.na(genus), c("final_taxon", "final_taxon_level") := .(genus, "genus")]
                lp_k_set_all[final_taxon_level == "species" & is.na(final_taxon) & is.na(genus) & !is.na(family), c("final_taxon", "final_taxon_level") := .(family, "family")]
                lp_k_set_all[final_taxon_level == "genus" & is.na(final_taxon) & !is.na(family), c("final_taxon", "final_taxon_level") := .(family, "family")]
                lp_k_set_all[is.na(final_taxon) & !is.na(family), c("final_taxon_level", "final_taxon") := .("family", family)]
                lp_k_set_all[is.na(final_taxon) & !is.na(order), c("final_taxon_level", "final_taxon") := .("order", order)]
                lp_k_set_all[is.na(final_taxon) & !is.na(subclass), c("final_taxon_level", "final_taxon") := .("subclass", subclass)]
                lp_k_set_all[is.na(final_taxon) & !is.na(class), c("final_taxon_level", "final_taxon") := .("class", class)]
                lp_k_set_all[is.na(final_taxon) & !is.na(phylum), c("final_taxon_level", "final_taxon") := .("phylum", phylum)]
                
                list_all2[[typ]][[k]] = lp_k_set_all
                
                rm(list = ls()[grep(x = ls(), pattern = "lp_k_")])
        }   # END OF LOOP K OVER TYPES 
        rm(list = ls()[grep(x = ls(), pattern = "lptyp_")])
} # END OF LOOP typ OVER TYPOLOGIES

dt_list = vector(mode = "list", length = 5)
for (i in 1:5) {
        dt_list[[i]] = rbindlist(list_all2[[i]])
        dt_list[[i]] = dt_list[[i]][!is.na(final_taxon)]
        
}
names(dt_list) = names(list_all2)


# save to file ------------------------------------------------------------
saveRDS(file = "data/05_final_taxon_all_typologies.rds", dt_list)


