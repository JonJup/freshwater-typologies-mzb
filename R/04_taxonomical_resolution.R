# ---------------------------------- #
### --- Taxonomical Resolution --- ### 
# ---------------------------------- #

# --------------- #
# date:  17.03.21
# files in 
        #-> 03_data_low_impact.rds   | macroinvertebrate observations from reference sites 
# files out
        #<- 04_taxon_resolution_list.rds
# Evaluating European Broad River Types for Macroinvertebrates 
# 1. some more taxonomic cleaning 
# --------------- #

# # Setup -------------------------------------------------------------------
source("R/setup_combined_inv.R")

# load data ---------------------------------------------------------------
set_all = readRDS("data/03_data_low_impact.rds")

# prepare data ------------------------------------------------------------------
#reduce to adequately represented river types
ch_acc    = paste0("RT", c(1,2,3,4,5,8,9, 10, 11, 14, 15, 16, 18))
set_all = set_all[ls_bd_20 %in% ch_acc]



# same taxonomic cleaning
set_all[genus == "Hydracarina", `:=` (genus = NA, family = NA)]
set_all[phylum == "Platyhelminthes", phylum := "Plathelminthes"]
set_all[phylum == "Nematomorpha", phylum := "Nematophora"]
set_all[order == "Arhynchobdellida", subclass := "Hirudinea"]
set_all[order == "Rhynchobdellida", subclass := "Hirudinea"]
set_all[order == "Isopoda", subclass := "Eumalacostraca"]
set_all[order == "Amphipoda", subclass := "Eumalacostraca"]
set_all[order == "Spongillida", subclass := "Heteroscleromorpha"]
set_all[order == "Littorinimorpha", subclass := "Caenogastropoda"]
set_all[class == "Clitella", class := "Clitellata"]
set_all[order == "Decapoda", subclass := "Eumalacostraca"]
set_all[order == "Architaenioglossa", subclass := "Caenogastropoda"]
set_all[order == "Terebellida", class := "Polychaeta"]
set_all[order == "Mysida", subclass := "Eumalacostraca"]
set_all[order == "Trombidiformes", subclass := "Acari"]
set_all[order == "Trombidiformes", class := "Arachnida"]
set_all[order == "Anthoathecata", subclass := "Hydroidolina"]
set_all[order == "Stylommatophora", subclass := "Heterobranchia"]
set_all[order == "Cladocera", subclass := "Phyllopoda"]
set_all[order == "Cyclopoida", subclass := "Copepoda"]
set_all[order == "Arguloida", subclass := "Branchiura"]
set_all[subclass == "Branchiura", class := "Maxillopoda"]
set_all[order == "Mermithida", subclass := "Dorylaimia"]
set_all[subclass == "Dorylaimia", class := "Enoplea"]
set_all[order == "Monostilifera", subclass := "Hoplonemertea"]
set_all[subclass == "Hoplonemertea", class := "Enopla"]
set_all[order == "Araneae", class := "Arachnida"]
set_all[order == "Calanoida", subclass := "Copepoda"]
set_all[family == "Capniidae", order := "Plecoptera"]
set_all[order == "Plecoptera", `:=`  (subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
set_all[family == "Valvatidae",`:=`  (order = NA, subclass = NA,  class = "Gastropoda", phylum = "Mollusca", kingdom = "Animalia")]
set_all[family == "Grynidae", family := "Gyrinidae"]
set_all[genus == "Stylaria",    `:=` (family = "Naididae", order = "Haplotaxida", subclass = NA, class = "Clitellata", phylum = "Annelida", kingdom = "Animalia")]
set_all[genus == "Micronecta",  `:=` (family = "Corixidae", order = "Hemiptera", subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
set_all[genus == "Stylodrilus", `:=` (family = "Lumbriculidae", order = "Lumbriculida", subclass = NA, class = "Clitellata", kingdom = "Animalia")]
set_all[genus == "Propappus",   `:=` (family = "Propappidae", order = "Enchytraeida", subclass = NA, class = "Clitellata", phylum = "Annelida", kingdom = "Animalia")]
set_all[genus == "Brachionus",  `:=` (family = "Brachionidae", order = "Ploima", subclass = NA, class = "Monogononta", phylum = "Rotifera", kingdom = "Animalia")]
set_all[genus == "Anisus",      `:=` (family = "Planorbidae", order = NA, subclass = NA, class = "Gastropoda", phylum = "Mollusca", kingdom = "Animalia")]
set_all[genus == "Hydaticus",   `:=` (family = "Dytiscidae", order = "Coleoptera", class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
set_all[order == "Ploima", `:=` (subclass = NA, class = "Monogononta", phylum = "Rotifera", kingdom = "Animalia")]
set_all[family == "Planorbidae",      `:=` (order = NA, subclass = NA, class = "Gastropoda", phylum = "Mollusca", kingdom = "Animalia")]

## -- split into one data frame per river type 
list_all = split.data.frame(x = set_all, f = factor(set_all$ls_bd_20))

# extract number of unique entries per taxonomic level 
n_phyl     = lapply(list_all, function(x)x[,uniqueN(phylum)])
n_class    = lapply(list_all, function(x)x[!is.na(class),    uniqueN(class)])
n_subclass = lapply(list_all, function(x)x[!is.na(subclass), uniqueN(subclass)])
n_order    = lapply(list_all, function(x)x[!is.na(order),    uniqueN(order)])
n_family   = lapply(list_all, function(x)x[!is.na(family),   uniqueN(family)])
n_genus    = lapply(list_all, function(x)x[!is.na(genus),    uniqueN(genus)])

## --  prepare empty lists for loops below 
lvl_data_phyl     = vector(mode = "list", length = length(ch_acc))
lvl_data_class    = vector(mode = "list", length = length(ch_acc))
lvl_data_subclass = vector(mode = "list", length = length(ch_acc))
lvl_data_order    = vector(mode = "list", length = length(ch_acc))
lvl_data_family   = vector(mode = "list", length = length(ch_acc))
lvl_data_genus    = vector(mode = "list", length = length(ch_acc))
names(lvl_data_phyl)     = names(n_phyl)
names(lvl_data_class)    = names(n_class)
names(lvl_data_subclass) = names(n_subclass)
names(lvl_data_order)    = names(n_order)
names(lvl_data_family)   = names(n_family)
names(lvl_data_genus)    = names(n_genus)

# Phylum ------------------------------------------------------------------
for (k in seq_along(n_phyl)) {
        level_data_phylum <-
                data.table(
                        phylum_name       = character(n_phyl[[k]]),
                        species           = numeric(n_phyl[[k]]),
                        genus             = numeric(n_phyl[[k]]),
                        family            = numeric(n_phyl[[k]]),
                        order             = numeric(n_phyl[[k]]),
                        subclass          = numeric(n_phyl[[k]]),
                        class             = numeric(n_phyl[[k]]),
                        phylum            = numeric(n_phyl[[k]]),
                        n_observations    = numeric(n_phyl[[k]])
                )
        
        for (i in 1:n_phyl[[k]]) {
                
                level_data_phylum[i, phylum_name := list_all[[k]][, unique(phylum)][i]]
                
                loop_sub <-
                        list_all[[k]][phylum == list_all[[k]][, unique(phylum)][i]]
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
                
        }
        lvl_data_phyl[[k]] = level_data_phylum
        rm(level_data_phylum)
}

# Class -------------------------------------------------------------------
## -- loop over all classes 
for (k in seq_along(n_class)) {
        
        level_data_class <- data.table(
                phylum_name      = character(n_class[[k]]),
                class_name       = character(n_class[[k]]),
                species          = numeric(n_class[[k]]),
                genus            = numeric(n_class[[k]]),
                family           = numeric(n_class[[k]]),
                order            = numeric(n_class[[k]]),
                subclass         = numeric(n_class[[k]]),
                class            = numeric(n_class[[k]]),
                n_observations   = numeric(n_class[[k]])
        )
        
        for (i in 1:n_class[[k]]) { # LOOP OVER NUMBER OF CLASSES IN EACH RIVER TYPE
                
                ## -- select a focal class for this iteration of k 
                loop_class <- list_all[[k]][!is.na(class), unique(class)][i]
                ## -- enter this class as class name for row i  
                level_data_class[i, class_name :=  loop_class]
                ## -- what is the phylum that this class belongs to? 
                # - check that there is only one phylum 
                unique_phylum = list_all[[k]][class == loop_class, unique(phylum)]
                if (length(unique_phylum)  != 1){
                        error_message = paste("There are", length(unique_phylum), "phyla for", 
                                              loop_class, ": \n", 
                                              paste(unique_phylum, collapse = " and "))
                        stop(error_message)
                }
                # - assign that phylum
                level_data_class[i, phylum_name := list_all[[k]][class == class_name, unique(phylum)]]
                
                loop_sub <- list_all[[k]][class == loop_class]
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
                rm(unique_pyhlum, loop_class);gc()
        }
        lvl_data_class[[k]] = level_data_class
        rm(level_data_class)
} # END OF LOOP K over all classes

lvl_data_class = 
        lapply(lvl_data_class, 
               function (x) setorderv(x, 
                                      cols = c("phylum_name", "class_name")
                                      )
               )

# Subclass  ----------------------------------------------------------------

for (k in seq_along(n_subclass)) {
        level_data_subclass <- data.table(
                phylum_name      = character(n_subclass[[k]]),
                class_name       = character(n_subclass[[k]]),
                subclass_name    = character(n_subclass[[k]]),
                species          = numeric(n_subclass[[k]]),
                genus            = numeric(n_subclass[[k]]),
                family           = numeric(n_subclass[[k]]),
                order            = numeric(n_subclass[[k]]),
                subclass         = numeric(n_subclass[[k]]),
                n_observations   = numeric(n_subclass[[k]])
        ) 
        for (i in 1:n_subclass[[k]]) {
                loop_subclass <- list_all[[k]][!is.na(subclass), unique(subclass)][i]
                
                level_data_subclass[i, subclass_name := loop_subclass]
                level_data_subclass[i, class_name    := list_all[[k]][subclass == loop_subclass, unique(class)]]
                level_data_subclass[i, phylum_name   := list_all[[k]][subclass == loop_subclass, unique(phylum)]]
                
                loop_sub <- list_all[[k]][subclass == loop_subclass]
                loop_obs <- nrow(loop_sub)
                
                level_data_subclass[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                level_data_subclass[i, genus          := round(loop_sub[is.na(species) &
                                                                                !is.na(genus), .N] / loop_obs * 100, 2)]
                level_data_subclass[i, family         := round(loop_sub[is.na(species) &
                                                                                is.na(genus) & !is.na(family), .N] / loop_obs * 100, 2)]
                level_data_subclass[i, order          := round(loop_sub[is.na(species) &
                                                                                is.na(genus) &
                                                                                is.na(family) & !is.na(order), .N] / loop_obs * 100, 2)]
                level_data_subclass[i, subclass       := round(loop_sub[is.na(species) &
                                                                                is.na(genus) &
                                                                                is.na(family) &
                                                                                is.na(order) & !is.na(subclass), .N] / loop_obs * 100, 2)]
                level_data_subclass[i, n_observations := nrow(loop_sub)]
        }
        lvl_data_subclass[[k]] = level_data_subclass
        rm(level_data_subclass)
}
lvl_data_subclass =
        lapply(lvl_data_subclass, function(x)
                setorderv(x,
                          cols = c(
                                  "phylum_name", "class_name", "subclass_name"
                          )))

# Order  -------------------------------------------------------------------

for (k in seq_along(n_order)){
        level_data_order <- data.table(
                phylum_name    = character(n_order[[k]]),
                class_name     = character(n_order[[k]]),
                subclass_name  = character(n_order[[k]]),
                order_name     = character(n_order[[k]]),
                species        = numeric(n_order[[k]]),
                genus          = numeric(n_order[[k]]),
                family         = numeric(n_order[[k]]),
                order          = numeric(n_order[[k]]),
                n_observations = numeric(n_order[[k]])
        )  
        for (i in 1:n_order[[k]]) {
                loop_order <-  list_all[[k]][!is.na(order), unique(order)][i]
                
                level_data_order[i, order_name    := loop_order]
                level_data_order[i, subclass_name := list_all[[k]][order == loop_order, unique(subclass)]]
                level_data_order[i, class_name    := list_all[[k]][order == loop_order, unique(class)]]
                level_data_order[i, phylum_name   := list_all[[k]][order == loop_order, unique(phylum)]]
                
                loop_sub <- list_all[[k]][order == loop_order]
                loop_obs <- nrow(loop_sub)
                
                level_data_order[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                level_data_order[i, genus          := round(loop_sub[is.na(species) &
                                                                             !is.na(genus), .N] / loop_obs * 100, 2)]
                level_data_order[i, family         := round(loop_sub[is.na(species) &
                                                                             is.na(genus) & !is.na(family), .N] / loop_obs * 100, 2)]
                level_data_order[i, order          := round(loop_sub[is.na(species) &
                                                                             is.na(genus) &
                                                                             is.na(family) & !is.na(order), .N] / loop_obs * 100, 2)]
                level_data_order[i, n_observations := nrow(loop_sub)]
        }
        lvl_data_order[[k]] = level_data_order
        rm(level_data_order)
}

lvl_data_order = lapply(lvl_data_order, function(x)
setorderv(
        x,
        cols = c("phylum_name", "class_name", "subclass_name", "order_name")
))



# Family ------------------------------------------------------------------

for (k in seq_along(n_family)) {
        level_data_family <- data.table(
                phylum_name    = character(n_family[[k]]),
                class_name     = character(n_family[[k]]),
                subclass_name  = character(n_family[[k]]),
                order_name     = character(n_family[[k]]),
                family_name    = character(n_family[[k]]),
                species        = numeric(n_family[[k]]),
                genus          = numeric(n_family[[k]]),
                family         = numeric(n_family[[k]]),
                n_observations = numeric(n_family[[k]])
        )
        for (i in 1:n_family[[k]]) {
                loop_family <- list_all[[k]][!is.na(family), unique(family)][i]
                
                level_data_family[i, family_name   := loop_family]
                level_data_family[i, order_name    := list_all[[k]][family == loop_family, unique(order)]]
                level_data_family[i, subclass_name := list_all[[k]][family == loop_family, unique(subclass)]]
                level_data_family[i, class_name    := list_all[[k]][family == loop_family, unique(class)]]
                level_data_family[i, phylum_name   := list_all[[k]][family == loop_family, unique(phylum)]]
                
                loop_sub <- list_all[[k]][family == loop_family]
                loop_obs <- nrow(loop_sub)
                
                level_data_family[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                level_data_family[i, genus          := round(loop_sub[is.na(species) &
                                                                              !is.na(genus), .N] / loop_obs * 100, 2)]
                level_data_family[i, family         := round(loop_sub[is.na(species) &
                                                                              is.na(genus) &
                                                                              !is.na(family), .N] / loop_obs * 100, 2)]
                level_data_family[i, n_observations := nrow(loop_sub)]
        }
        lvl_data_family[[k]] = level_data_family
        rm(level_data_family)
}

lvl_data_family = lapply(lvl_data_family, function(x)
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

# Genus -------------------------------------------------------------------
for (k in seq_along(n_genus)) {
        level_data_genus <- data.table(
                phylum_name    = character(n_genus[[k]]),
                class_name     = character(n_genus[[k]]),
                subclass_name  = character(n_genus[[k]]),
                order_name     = character(n_genus[[k]]),
                family_name    = character(n_genus[[k]]),
                genus_name     = character(n_genus[[k]]),
                species          = numeric(n_genus[[k]]),
                genus            = numeric(n_genus[[k]]),
                n_observations   = numeric(n_genus[[k]])
        )
        for (i in 1:n_genus[[k]]) {
                loop_genus <- list_all[[k]][!is.na(genus), unique(genus)][i]
                
                level_data_genus[i, genus_name    := loop_genus]
                level_data_genus[i, family_name   := list_all[[k]][genus == loop_genus, unique(family)]]
                level_data_genus[i, order_name    := list_all[[k]][genus == loop_genus, unique(order)]]
                level_data_genus[i, subclass_name := list_all[[k]][genus == loop_genus, unique(subclass)]]
                level_data_genus[i, class_name    := list_all[[k]][genus == loop_genus, unique(class)]]
                level_data_genus[i, phylum_name   := list_all[[k]][genus == loop_genus, unique(phylum)]]
                
                loop_sub <- list_all[[k]][genus == loop_genus]
                loop_obs <- nrow(loop_sub)
                
                level_data_genus[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100, 2)]
                level_data_genus[i, genus          := round(loop_sub[is.na(species) &
                                                                             !is.na(genus), .N] / loop_obs * 100, 2)]
                level_data_genus[i, n_observations := nrow(loop_sub)]
                print(paste(i, n_genus[[k]]))
        }
        lvl_data_genus[[k]] = level_data_genus
        rm(level_data_genus)
}
lvl_data_genus = lapply(lvl_data_genus, function(x)
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
        ))


# Save to file  -----------------------------------------------------------

result_list = list(phylum  = lvl_data_phyl, 
                  class    = lvl_data_class,
                  subclass = lvl_data_subclass,
                  order    = lvl_data_order,
                  family   = lvl_data_family,
                  genus    = lvl_data_genus)

saveRDS(result_list, "data/04_taxon_resolution_list.rds")
