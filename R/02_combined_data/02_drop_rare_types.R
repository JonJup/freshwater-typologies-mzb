# ------------------------ #
# --- Drop Rare Types  --- # 
# ------------------------ #

# date written: 04.05.22
# date last modified: 04.05.22
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: Drop freshwater types that have less than 15 samples 

# setup -----------------------------------------------
pacman::p_load(data.table, rstudioapi)

x<-getActiveDocumentContext()
sink(file = paste0("R/02_combined_data/log_files/02","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)


# load data -------------------------------------------
data <- readRDS("data/02_combined_data/01_2022-05-04_combined_data_aggregated.rds")
# combine BRT and ife ----------------------------------------
#- Create sites data 
sites <- lapply(data, unique, by = "gr_sample_id")

threshold <- 15

# -------- BRT12 -------- # 
# -- spring 
brt.tbl <- table(sites$spring$brt12)
any(brt.tbl< threshold)
# -- summer 
brt.tbl <- table(sites$summer$brt12)
any(brt.tbl< threshold)
# -- autumn - drop RT12  
brt.tbl <- table(sites$autumn$brt12)
any(brt.tbl< threshold)
(brt.rm  <- names(brt.tbl)[which(brt.tbl < threshold)])
sites$autumn <- sites$autumn[!brt12 %in% brt.rm]

rm(brt.tbl, brt.rm)

# -------- ife -------- # 
# -- spring 
# --> "Baltic province"
# --> "Eastern Balkan"
# --> "Fenno-scandian shield" 
# --> "Italy and Corsica" 
# --> "Pontic province"
# --> "Pyrenees"   
ife.tbl <- table(sites$spring$ife)
any(ife.tbl< threshold)
(ife.rm  <- names(ife.tbl)[which(ife.tbl< threshold)])
sites$spring <- sites$spring[!ife %in% ife.rm]
# -- summer 
#--> 
#--> "Borealic uplands"
#--> "Dinaric western Balkan"  
#--> "Eastern Balkan"          
#--> "England"                
#--> "Hellenic western Balkan" 
#--> "Hungarian lowlands"
#--> "The Carpathiens"
ife.tbl <- table(sites$summer$ife)
any(ife.tbl< threshold)
(ife.rm  <- names(ife.tbl)[which(ife.tbl< threshold)])
sites$summer <- sites$summer[!ife %in% ife.rm]
# -- autumn 
# --> "Eastern Balkan"
# --> "Eastern Plains"
# --> "Hungarian lowlands" 
# --> "The Carpathiens"    
# --> "Tundra"
ife.tbl <- table(sites$autumn$ife)
any(ife.tbl< threshold)
(ife.rm  <- names(ife.tbl)[which(ife.tbl< threshold)])
sites$autumn <- sites$autumn[!ife %in% ife.rm]

rm(ife.tbl, ife.rm)

# -------- BGR -------- # 
# -- spring 
# --> "boreal" 
#-->  "pannonian"
bgr.tbl <- table(sites$spring$bgr)
any(bgr.tbl< threshold)
(bgr.rm  <- names(bgr.tbl)[which(bgr.tbl< threshold)])
sites$spring <- sites$spring[!bgr %in% bgr.rm]
# -- summer 
# --> nothing 
bgr.tbl <- table(sites$summer$bgr)
any(bgr.tbl< threshold)
(bgr.rm  <- names(bgr.tbl)[which(bgr.tbl< threshold)])
sites$summer <- sites$summer[!bgr %in% bgr.rm]
# -- autumn 
# --> "arctic"    
bgr.tbl <- table(sites$autumn$bgr)
any(bgr.tbl< threshold)
(bgr.rm  <- names(bgr.tbl)[which(bgr.tbl< threshold)])
sites$autumn <- sites$autumn[!bgr %in% bgr.rm]


data$spring <- data$spring[gr_sample_id %in% sites$spring$gr_sample_id]
data$summer <- data$summer[gr_sample_id %in% sites$summer$gr_sample_id]
data$autumn <- data$autumn[gr_sample_id %in% sites$autumn$gr_sample_id]

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data, paste0("data/02_combined_data/02_", Sys.Date(), "_data_well_sampled_types.rds"))

