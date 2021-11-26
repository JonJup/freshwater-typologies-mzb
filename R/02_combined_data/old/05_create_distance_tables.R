# ——————————————————————————————— #
# ——— Create distance tables  ——— # 
# ——————————————————————————————— #

# ————————————————
# date:
#       26.07.21
# files in: 
#       -> subsets (*date*_subsets.rds)
#       -> look up [sample id to type] (*date*_lookup.rds)
# files out:
#       <- 
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
        # Create distance tables for each subset 
# ————————————————


# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, parallelDist)

# LOAD DATA  -------------------------------------------
data <- readRDS("data/combined_data/04_2021-07-28_subsets.rds")
lookup <- readRDS("data/combined_data/04_2021-07-28_lookup.rds")

id_position <- sapply(data, function(x)which(names(x) == "gr_sample_id"))
data <- lapply(data, as.data.frame)


pre_distance  <- 
        lapply(1:length(data), function(x) as.matrix(data[[x]][,-id_position[x]])) 
distance.jaccard <- 
        lapply(pre_distance, parallelDist, method = "binary", threads = 8)
distance.dice <- 
        lapply(pre_distance, parallelDist, method = "dice", threads = 8)
distance.ochiai <- 
        lapply(pre_distance, parallelDist, method = "ochiai", threads = 8)
        
# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(distance.jaccard, paste0("data/combined_data/05_",Sys.Date(),"_distance_jaccard.rds"))
saveRDS(distance.dice,    paste0("data/combined_data/05_",Sys.Date(),"_distance_dice.rds"))
saveRDS(distance.ochiai,  paste0("data/combined_data/05_",Sys.Date(),"_distance_ochiai.rds"))

