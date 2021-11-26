# ———————————————————————— #
# ——— Drop Rare Types  ——— # 
# ———————————————————————— #

# ————————————————
# date:
#       26.07.21
# files in: 
#       -> combined invertebrate data (*date*_combined_data.rds)
# files out:
#       <- combined data without under-sampled types (*date*_well_sampled_data.rds)
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Drop freshwater types that have less than 10 samples 
# ————————————————

# setup -----------------------------------------------
pacman::p_load(data.table)
# load data -------------------------------------------
data <- readRDS("data/combined_data/01_2021-07-28_combined_data.rds")
# combine BRT and Illies ----------------------------------------

#- Create sites data 
sites <- unique(data, by ="gr_sample_id")
sites <- lapply(data, unique, by = "gr_sample_id")
#- First we remove samples from types that are not well sampled (<10 samples)

# ———————— BRT12 ———————— # 
# —— spring 
brt12.tbl <- table(sites$spring$brt12)
any(brt12.tbl<10)
# —— summer 
brt12.tbl <- table(sites$summer$brt12)
any(brt12.tbl<10)
# —— autumn 
brt12.tbl <- table(sites$autumn$brt12)
any(brt12.tbl<10)

# ————> all good 

# ———————— BRT20 ———————— # 
# —— spring ——> RT 6,7,12 are rare  
brt20.tbl <- table(sites$spring$brt20)
any(brt20.tbl<10)
brt20.rm  <- names(brt20.tbl)[which(brt20.tbl<10)]
sites$spring <- sites$spring[!brt20 %in% brt20.rm]
# —— summer ——> RT 6,12 are rare  
brt20.tbl <- table(sites$summer$brt20)
any(brt20.tbl<10)
(brt20.rm  <- names(brt20.tbl)[which(brt20.tbl<10)])
sites$summer <- sites$summer[!brt20 %in% brt20.rm]
# —— autumn ——> RT 6,12 are rare  
brt20.tbl <- table(sites$autumn$brt20)
any(brt20.tbl<10)
(brt20.rm  <- names(brt20.tbl)[which(brt20.tbl<10)])
sites$autumn <- sites$autumn[!brt20 %in% brt20.rm]

# ———————— Illies ———————— # 
# —— spring ——> "Italy and Corsica" is rare  
illies.tbl <- table(sites$spring$illies)
any(illies.tbl<10)
(illies.rm  <- names(illies.tbl)[which(illies.tbl<10)])
sites$spring <- sites$spring[!illies %in% illies.rm]
# —— summer ——> "Fenno-scandian shield" and "Hungarian lowlands" are rare  
illies.tbl <- table(sites$summer$illies)
any(illies.tbl<10)
(illies.rm  <- names(illies.tbl)[which(illies.tbl<10)])
sites$summer <- sites$summer[!illies %in% illies.rm]
# —— autumn ——> "Borealic uplands" and "Hungarian lowlands" are rare  
illies.tbl <- table(sites$autumn$illies)
any(illies.tbl<10)
(illies.rm  <- names(illies.tbl)[which(illies.tbl<10)])
sites$autumn <- sites$autumn[!illies %in% illies.rm]

# ———————— BRT12 Illies ———————— # 
# —— spring ——> too many to list are rare
brt12_illies.tbl <- table(sites$spring$brt12_illies)
any(brt12_illies.tbl<10)
(brt12_illies.rm  <- names(brt12_illies.tbl)[which(brt12_illies.tbl<10)])
sites$spring <- sites$spring[!brt12_illies %in% brt12_illies.rm]
# —— summer ——> "Fenno-scandian shield" and "Hungarian lowlands" are rare
brt12_illies.tbl <- table(sites$summer$brt12_illies)
any(brt12_illies.tbl<10)
(brt12_illies.rm  <- names(brt12_illies.tbl)[which(brt12_illies.tbl<10)])
sites$summer <- sites$summer[!brt12_illies %in% brt12_illies.rm]
# —— autumn ——> "Borealic uplands" and "Hungarian lowlands" are rare
brt12_illies.tbl <- table(sites$autumn$brt12_illies)
any(brt12_illies.tbl<10)
(brt12_illies.rm  <- names(brt12_illies.tbl)[which(brt12_illies.tbl<10)])
sites$autumn <- sites$autumn[!brt12_illies %in% brt12_illies.rm]


data$spring <- data$spring[gr_sample_id %in% sites$spring$gr_sample_id]
data$summer <- data$summer[gr_sample_id %in% sites$summer$gr_sample_id]
data$autumn <- data$autumn[gr_sample_id %in% sites$autumn$gr_sample_id]

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data, paste0("data/combined_data/02_", Sys.Date(), "_well_sampled_data.rds"))

