# ----------------------------- #
# --- Analysis: Typical ------- # 
# ----------------------------- #


#  date created: 28-09-21
# last modified: 03-05-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute Typical communities 


# setup -----------------------------------------------------------------------------
#devtools::install_github(repo = "https://github.com/JonJup/jjmisc")
library(data.table)
library(indicspecies)
library(jjmisc)
library(rstudioapi)

x<-getActiveDocumentContext()
sink(file = paste0("R/02_combined_data/log_files/typical","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
sp.i <- readRDS("data/02_combined_data/04_2022-05-04_community_data_spring.rds")
su.i <- readRDS("data/02_combined_data/04_2022-05-04_community_data_summer.rds")
au.i <- readRDS("data/02_combined_data/04_2022-05-04_community_data_autumn.rds")
id   <- readRDS("data/02_combined_data/04_2022-05-04_distance_ids.rds")

# analysis --------------------------------------------------------------------------
sp.brt.i <- typical_comm(community = sp.i[,-1], grouping = id$sp.i$brt,  perm = 99, season = "spring",  typology = "brt")
sp.bgr.i <- typical_comm(community = sp.i[,-1], grouping = id$sp.i$bgr,  perm = 99, season = "spring",  typology = "bgr")
sp.ife.i <- typical_comm(community = sp.i[,-1], grouping = id$sp.i$ife,  perm = 99, season = "spring",  typology = "ife")
su.brt.i <- typical_comm(community = su.i[,-1], grouping = id$su.i$brt,  perm = 99, season = "summer",  typology = "brt")
su.bgr.i <- typical_comm(community = su.i[,-1], grouping = id$su.i$bgr,  perm = 99, season = "summer",  typology = "bgr")
su.ife.i <- typical_comm(community = su.i[,-1], grouping = id$su.i$ife,  perm = 99, season = "summer",  typology = "ife")
au.brt.i <- typical_comm(community = au.i[,-1], grouping = id$au.i$brt,  perm = 99, season = "autumn",  typology = "brt")
au.bgr.i <- typical_comm(community = au.i[,-1], grouping = id$au.i$bgr,  perm = 99, season = "autumn",  typology = "bgr")
au.ife.i <- typical_comm(community = au.i[,-1], grouping = id$au.i$ife,  perm = 99, season = "autumn",  typology = "ife")

all <- rbindlist(
        list(
                sp.brt.i,
                sp.bgr.i,
                sp.ife.i,
                su.brt.i,
                su.bgr.i,
                su.ife.i,
                au.brt.i,
                au.bgr.i,
                au.ife.i
        )
)

# save to file ----------------------------------------------------------------------
saveRDS(all, paste0("data/02_combined_data/",Sys.Date(),"_results_typical.rds"))


