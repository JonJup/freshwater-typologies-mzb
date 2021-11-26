# ———————————————————————————— #
# ——— Analysis: Typical ——————— # 
# ———————————————————————————— #

# ———————————————————————————————————
#  date created: 28-09-21
# last modified: 18-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute Typical communities 
# ———————————————————————————————————

# setup -----------------------------------------------------------------------------
devtools::install_github(repo = "https://github.com/JonJup/jjmisc")
library(data.table)
library(indicspecies)
library(jjmisc)
# load data -------------------------------------------------------------------------
sp.i <- readRDS("data/02_combined_data/2021-10-14_community_data_spring_impacted.rds")
su.i <- readRDS("data/02_combined_data/2021-10-14_community_data_summer_impacted.rds")
au.i <- readRDS("data/02_combined_data/2021-10-14_community_data_autumn_impacted.rds")
sp.l <- readRDS("data/02_combined_data/2021-10-14_community_data_spring_least_impacted.rds")
su.l <- readRDS("data/02_combined_data/2021-10-14_community_data_summer_least_impacted.rds")
au.l <- readRDS("data/02_combined_data/2021-10-14_community_data_autumn_least_impacted.rds")
id   <- readRDS("data/02_combined_data/2021-10-14_distance_ids.rds")

# analysis --------------------------------------------------------------------------

sp.brt.i <- typical_comm(community = sp.i[,-1], grouping = id$sp.i$brt,  perm = 99, season = "spring",  typology = "brt")
sp.bgr.i <- typical_comm(community = sp.i[,-1], grouping = id$sp.i$bgr,  perm = 99, season = "spring",  typology = "bgr")
sp.ife.i <- typical_comm(community = sp.i[,-1], grouping = id$sp.i$ife,  perm = 99, season = "spring",  typology = "ife")
sp.brt.l <- typical_comm(community = sp.l[,-1], grouping = id$sp.l$brt,  perm = 99, season = "spring",  typology = "brt")
sp.bgr.l <- typical_comm(community = sp.l[,-1], grouping = id$sp.l$bgr,  perm = 99, season = "spring",  typology = "bgr")
sp.ife.l <- typical_comm(community = sp.l[,-1], grouping = id$sp.l$ife,  perm = 99, season = "spring",  typology = "ife")
su.brt.i <- typical_comm(community = su.i[,-1], grouping = id$su.i$brt,  perm = 99, season = "summer",  typology = "brt")
su.bgr.i <- typical_comm(community = su.i[,-1], grouping = id$su.i$bgr,  perm = 99, season = "summer",  typology = "bgr")
su.ife.i <- typical_comm(community = su.i[,-1], grouping = id$su.i$ife,  perm = 99, season = "summer",  typology = "ife")
su.brt.l <- typical_comm(community = su.l[,-1], grouping = id$su.l$brt,  perm = 99, season = "summer",  typology = "brt")
su.bgr.l <- typical_comm(community = su.l[,-1], grouping = id$su.l$bgr,  perm = 99, season = "summer",  typology = "bgr")
su.ife.l <- typical_comm(community = su.l[,-1], grouping = id$su.l$ife,  perm = 99, season = "summer",  typology = "ife")
au.brt.i <- typical_comm(community = au.i[,-1], grouping = id$au.i$brt,  perm = 99, season = "autumn",  typology = "brt")
au.bgr.i <- typical_comm(community = au.i[,-1], grouping = id$au.i$bgr,  perm = 99, season = "autumn",  typology = "bgr")
au.ife.i <- typical_comm(community = au.i[,-1], grouping = id$au.i$ife,  perm = 99, season = "autumn",  typology = "ife")
au.brt.l <- typical_comm(community = au.l[,-1], grouping = id$au.l$brt,  perm = 99, season = "autumn",  typology = "brt")
au.bgr.l <- typical_comm(community = au.l[,-1], grouping = id$au.l$bgr,  perm = 99, season = "autumn",  typology = "bgr")
au.ife.l <- typical_comm(community = au.l[,-1], grouping = id$au.l$ife,  perm = 99, season = "autumn",  typology = "ife")

sp.brt.i$least_impaired <- FALSE
sp.bgr.i$least_impaired <- FALSE
sp.ife.i$least_impaired <- FALSE
sp.brt.l$least_impaired <- TRUE
sp.bgr.l$least_impaired <- TRUE
sp.ife.l$least_impaired <- TRUE
su.brt.i$least_impaired <- FALSE
su.bgr.i$least_impaired <- FALSE
su.ife.i$least_impaired <- FALSE
su.brt.l$least_impaired <- TRUE
su.bgr.l$least_impaired <- TRUE
su.ife.l$least_impaired <- TRUE
au.brt.i$least_impaired <- FALSE
au.bgr.i$least_impaired <- FALSE
au.ife.i$least_impaired <- FALSE
au.brt.l$least_impaired <- TRUE
au.bgr.l$least_impaired <- TRUE
au.ife.l$least_impaired <- TRUE


all <- rbindlist(
        list(
                sp.brt.i,
                sp.bgr.i,
                sp.ife.i,
                sp.brt.l,
                sp.bgr.l,
                sp.ife.l,
                su.brt.i,
                su.bgr.i,
                su.ife.i,
                su.brt.l,
                su.bgr.l,
                su.ife.l,
                au.brt.i,
                au.bgr.i,
                au.ife.i,
                au.brt.l,
                au.bgr.l,
                au.ife.l
        )
)

# save to file ----------------------------------------------------------------------
saveRDS(all, "data/02_combined_data/2021-10-14_results_typical.rds")


