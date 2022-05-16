# ---------------------------- #
# --- Analysis: Typical ------ #
# --- REVIEW            ------ #
# ---------------------------- #

# -----------------------------------
#  date created: 29-04-22
# last modified: 29-04-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute Typical communities for genues level relative abundance data. 
# -----------------------------------

# setup -----------------------------------------------------------------------------
#devtools::install_github(repo = "https://github.com/JonJup/jjmisc")

library(pacman)

p_load(
        data.table,
        indicspecies,
        jjmisc,
        rstudioapi
        )

x<-getActiveDocumentContext()
sink(file = paste0("R/STOTEN-reivew/009","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)


# load data -------------------------------------------------------------------------
sp.i <- readRDS("data/review/2022-04-29_community_data_spring.rds")
su.i <- readRDS("data/review/2022-04-29_community_data_summer.rds")
au.i <- readRDS("data/review/2022-04-29_community_data_autumn.rds")

id   <- readRDS("data/review/2022-04-29_distance_ids.rds")

# analysis --------------------------------------------------------------------------

sp.brt.i <- typical_comm(community = sp.i[,-1], grouping = id$sp.i$brt,  perm = 99, season = "spring",  typology = "brt", threshold = 0.33)
sp.bgr.i <- typical_comm(community = sp.i[,-1], grouping = id$sp.i$bgr,  perm = 99, season = "spring",  typology = "bgr", threshold = 0.33)
sp.ife.i <- typical_comm(community = sp.i[,-1], grouping = id$sp.i$ife,  perm = 99, season = "spring",  typology = "ife", threshold = 0.33)
su.brt.i <- typical_comm(community = su.i[,-1], grouping = id$su.i$brt,  perm = 99, season = "summer",  typology = "brt", threshold = 0.33)
su.bgr.i <- typical_comm(community = su.i[,-1], grouping = id$su.i$bgr,  perm = 99, season = "summer",  typology = "bgr", threshold = 0.33)
su.ife.i <- typical_comm(community = su.i[,-1], grouping = id$su.i$ife,  perm = 99, season = "summer",  typology = "ife", threshold = 0.33)
au.brt.i <- typical_comm(community = au.i[,-1], grouping = id$au.i$brt,  perm = 99, season = "autumn",  typology = "brt", threshold = 0.33)
au.bgr.i <- typical_comm(community = au.i[,-1], grouping = id$au.i$bgr,  perm = 99, season = "autumn",  typology = "bgr", threshold = 0.33)
au.ife.i <- typical_comm(community = au.i[,-1], grouping = id$au.i$ife,  perm = 99, season = "autumn",  typology = "ife", threshold = 0.33)


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
saveRDS(all, "test.rds")


