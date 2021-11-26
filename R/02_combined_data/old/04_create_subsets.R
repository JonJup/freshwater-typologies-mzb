# ——————————————————————— #
# ——— Create subsets  ——— # 
# ——————————————————————— #

# ————————————————
# date:
#       26.07.21
# files in: 
#       -> combined data of core taxa (*date*_core_taxa_data.rds)
# files out:
#       <- subsets (*date*_subsets.rds)
#       <- look up [sample id to type] (*date*_lookup.rds)
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Create subsets:
#               1. Genus level data + Sites x Species (sxs)
#               2. Family level data + Sites x Species (sxs)
#               3. German data at genus level 
#               4. French data at genus level 
# ————————————————


# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, tidyr)

# LOAD DATA  -------------------------------------------
data <- readRDS("data/combined_data/03_2021-07-28_core_taxa_data.rds")


# DROP OBSERVATIONS WITH NA IN TYPES ------------------------------------------------
data <- lapply(1:3, function(i) data[[i]][!is.na(brt20) & !is.na(brt20) & !is.na(illies)]

# CREATE GENUS LEVEL DATA  ------------------------------------------------------------------
data.genus <- copy(data)
data.genus[, final.taxon := genus]
data.genus <- data.genus[!is.na(genus)]

# CREATE FAMILY LEVEL DATA ----------------------------------------------------------
data.family <- copy(data)
data.family[, final.taxon := family]
data.family <- data.family[!is.na(family)]

subsets <- list(genus = data.genus, family = data.family)

anyNA(subsets$family$brt12)
anyNA(subsets$family$brt20)
anyNA(subsets$family$brt12_illies)
anyNA(subsets$family$brt20_illies)
anyNA(subsets$family$illies)


# CREATE SITES X SPECIES TABLES -------------------------------------------------------------------
subsets.sxs  <- lapply(subsets, pivot_wider, id_cols = gr_sample_id,names_from = final.taxon,values_from = abundance,values_fill = 0,values_fn = sum)
subsets.sxs2 <- lapply(subsets.sxs, function(x) apply(x[,-1],2,function(x) ifelse(x==0, 0,1)))
subsets.sxs2 <- lapply(subsets.sxs2, as.data.frame)
subsets.sxs2 <- lapply(subsets.sxs2, setDT)
for (i in seq_along(subsets.sxs2)) subsets.sxs2[[i]]$gr_sample_id <- subsets.sxs[[i]]$gr_sample_id

# CREATE GERMAN SUBSET --------------------------------------------------------------
id.german.data <- data.genus[!is.na(german_type), unique(gr_sample_id)]
subsets.sxs2$genus.german = subsets.sxs2$genus[gr_sample_id %in% id.german.data]
subsets.sxs2$family.german = subsets.sxs2$family[gr_sample_id %in% id.german.data]
# CREATE FRENCH SUBSET --------------------------------------------------------------
id.french.data <- data.genus[!is.na(her), unique(gr_sample_id)]
subsets.sxs2$genus.french = subsets.sxs2$genus[gr_sample_id %in% id.french.data]
subsets.sxs2$family.french = subsets.sxs2$family[gr_sample_id %in% id.french.data]

# CREATE CLASSIFICATION LOOKUP TABLE ------------------------------------------------
lookup1 <- data.genus[, c("gr_sample_id", "brt12", "brt20", "illies", "brt12_illies", "brt20_illies", "german_type", "her","data.set")]
lookup2 <- data.family[, c("gr_sample_id", "brt12", "brt20", "illies", "brt12_illies", "brt20_illies", "german_type", "her","data.set")]
lookup <- rbindlist(list(lookup1, lookup2)) |> unique(by="gr_sample_id")
apply(lookup, 2, anyNA)
# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(subsets.sxs2, paste0("data/combined_data/04_",Sys.Date(),"_subsets.rds"))
saveRDS(lookup,       paste0("data/combined_data/04_",Sys.Date(),"_lookup.rds"))

