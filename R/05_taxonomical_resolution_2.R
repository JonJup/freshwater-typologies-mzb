# ----------------------------------- #
### --- Taxonomical Resolution 2--- ### 
# ----------------------------------- #

# --------------- #
# date:  17.03.21
# files in 
        #-> 03_data_low_impact.rds         | macroinvertebrate observations from reference sites 
        #-> 04_taxon_resolution_list.rds   | percent of taxa that are represented at different levels 
# files out
        #<- 04_taxon_resolution_list.rds
# Evaluating European Broad River Types for Macroinvertebrates 
# 1. some more taxonomic cleaning 
# --------------- #

# Setup -------------------------------------------------------------------
source("R/setup_combined_inv.R")

# data IO  ----------------------------------------------------------------
set_all = readRDS("data/03_data_low_impact.rds")
ls_res  = readRDS("data/04_taxon_resolution_list.rds")

ls_res$genus$RT1[genus_name == "Dreissena"]


# add x and y coordinates as variables to extract unique sites 
set_all_sf  <- st_as_sf(set_all)
set_all_sf$x_coord <- st_coordinates(set_all_sf)[,1]
set_all_sf$y_coord <- st_coordinates(set_all_sf)[,2]
set_all <- set_all_sf
setDT(set_all)

#reduce to adequately represented river types 
ch_acc    = paste0("RT", c(1,2,3,4,5,8,9, 10, 11, 14, 15, 16, 18))
set_all_sub = set_all[ls_bd_20 %in% ch_acc]


list_all = split.data.frame(x = set_all_sub, f = factor(set_all_sub$ls_bd_20))
list_all2 = vector(mode="list", length = length(ch_acc))

# Create Lists  -----------------------------------------------------------
# Phyla to keep 
phyl_keep = c("Annelida", "Arthropoda", "Mollusca")
# Classes to keep 
class_keep = c("Clitellata","Insecta","Malacostraca","Bivalvia","Gastropoda")

# recent required to go to higher taxonomic levels. 
required_percent = 50

for (k in seq_along(ch_acc)){
        
        print(paste("## -- STARTING ", ch_acc[k], "-- ##"))
        list_all_loop_id = which(names(list_all) == ch_acc[k])
        ls_res_id        = which(names(ls_res$phylum) == ch_acc[k])
        
        set_all_mod = list_all[[list_all_loop_id]][phylum %in% phyl_keep | is.na(phylum)]  
        set_all_mod = set_all_mod[class %in% class_keep | is.na(class)]
        set_all_mod$final_taxon = character()
        set_all_mod$final_taxon_level = character()
        ## -- SUBCLASS -- ## 
        # Pteriomorpha is only in Edwin Peters and Landau. The sublcass contains
        # Mytiulus edulis and I suspect them to be brackish. A check of the data in QGIS
        # confirms this. Other data sets do not have coastal waters so I remove these
        # observations.
        set_all_mod   = set_all_mod[subclass != "Pteriomorphia" | is.na(subclass)]
        subclass_id   = which(ls_res$subclass[[ls_res_id]][, (species + genus + family + order) < required_percent])
        subclass_taxa = ls_res$subclass[[ls_res_id]][subclass_id, subclass_name]
        set_all_mod[subclass %in% subclass_taxa & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(subclass, "subclass")]
        rm(subclass_taxa, subclass_id)
        ## -- ORDER -- ## 
        order_id   = which(ls_res$order[[ls_res_id]][, (species + genus + family) < required_percent])
        order_taxa = ls_res$order[[ls_res_id]][order_id, order_name]
        set_all_mod[order %in% order_taxa & is.na(final_taxon_level), 
                    c("final_taxon", "final_taxon_level") := .(order, "order") ]
        rm(order_taxa, order_id)
        ## -- FAMILY -- ## 
        family_lvl_id = ls_res$family[[ls_res_id]][,(species + genus) < required_percent]
        family_taxa   = ls_res$family[[ls_res_id]][family_lvl_id, family_name]
        set_all_mod[family %in% family_taxa & is.na(final_taxon_level), 
                    c("final_taxon", "final_taxon_level") := .(family, "family") ]
        rm(family_taxa, family_lvl_id)
        ## -- GENUS -- ## 
        genus_lvl_id   = ls_res$genus[[ls_res_id]][,species < required_percent]
        
        genus_taxa     = sort(ls_res$genus[[ls_res_id]][genus_lvl_id, genus_name])
        set_all_mod[genus %in% genus_taxa & is.na(final_taxon_level), 
                    c("final_taxon", "final_taxon_level") := .(genus, "genus") ]
        rm(genus_taxa, genus_lvl_id)
        ## -- SPECIES -- ## 
        species_lvl_id = ls_res$genus[[ls_res_id]][,species >= required_percent]
        species_taxa   = ls_res$genus[[ls_res_id]][species_lvl_id, genus_name]
        set_all_mod[genus %in% species_taxa & is.na(final_taxon_level), 
                    c("final_taxon", "final_taxon_level") := .(species, "species") ]
        rm(species_taxa, species_lvl_id)
        # now clean from bottom up 
        set_all_mod[final_taxon_level == "species" & is.na(final_taxon) & !is.na(genus), c("final_taxon", "final_taxon_level") := .(genus, "genus")]
        set_all_mod[final_taxon_level == "species" & is.na(final_taxon) & is.na(genus) & !is.na(family), c("final_taxon", "final_taxon_level") := .(family, "family")]
        set_all_mod[final_taxon_level == "genus" & is.na(final_taxon) & !is.na(family), c("final_taxon", "final_taxon_level") := .(family, "family")]
        
        set_all_mod[is.na(final_taxon) & !is.na(family), c("final_taxon_level", "final_taxon") := .("family", family)]
        set_all_mod[is.na(final_taxon) & !is.na(order), c("final_taxon_level", "final_taxon") := .("order", order)]
        set_all_mod[is.na(final_taxon) & !is.na(subclass), c("final_taxon_level", "final_taxon") := .("subclass", subclass)]
        set_all_mod[is.na(final_taxon) & !is.na(class), c("final_taxon_level", "final_taxon") := .("class", class)]
        set_all_mod[is.na(final_taxon) & !is.na(phylum), c("final_taxon_level", "final_taxon") := .("phylum", phylum)]
        
        list_all2[[k]] = set_all_mod
        
        rm(set_all_mod)
        
}

# combine 
set_all = rbindlist(list_all2)
# test 
set_all[is.na(final_taxon)]
set_all = set_all[!is.na(final_taxon)]
# save to file ------------------------------------------------------------
saveRDS(file = "data/05_final_taxon.rds", set_all)


