# ------------------------ #
# --- Analysis: CS ------- # 
# ------------------------ #

#  date created: 27-09-21
# last modified: 03-05-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute Classification strength

# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/JonJup/jjmisc")
library(data.table)
library(ggplot2)
library(dplyr)
library(jjmisc)
library(rstudioapi)

x<-getActiveDocumentContext()
sink(file = paste0("R/02_combined_data/log_files/cs","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
id     <- readRDS("data/02_combined_Data/04_2022-05-04_distance_ids.rds")
d.sp.i <- readRDS("data/02_combined_data/04_2022-05-04_distance_spring.rds")
d.su.i <- readRDS("data/02_combined_data/04_2022-05-04_distance_summer.rds")
d.au.i <- readRDS("data/02_combined_data/04_2022-05-04_distance_autumn.rds")

# analysis --------------------------------------------------------------------------

#- test function before uploading it 
# source("../../../projects/jjmisc/R/compute_cs.R")
# source("../../../projects/jjmisc/R/classification_strength.R")

# compute_cs(dist = d.sp.i, grouping = id$sp.i$brt, season = "spring", typology = "brt")
# classification_strength(dist = d.sp.i, grouping = id$sp.i$brt, season = "spring", typology = "brt", permutations = 10)

sp.i.brt <- classification_strength(dist = d.sp.i, grouping = id$sp.i$brt, season = "spring", typology = "brt", permutations = 99)
su.i.brt <- classification_strength(dist = d.su.i, grouping = id$su.i$brt, season = "summer", typology = "brt", permutations = 99)
au.i.brt <- classification_strength(dist = d.au.i, grouping = id$au.i$brt, season = "autumn", typology = "brt", permutations = 99)
sp.i.bgr <- classification_strength(dist = d.sp.i, grouping = id$sp.i$bgr, season = "spring", typology = "bgr", permutations = 99)
su.i.bgr <- classification_strength(dist = d.su.i, grouping = id$su.i$bgr, season = "summer", typology = "bgr", permutations = 99)
au.i.bgr <- classification_strength(dist = d.au.i, grouping = id$au.i$bgr, season = "autumn", typology = "bgr", permutations = 99)
sp.i.ife <- classification_strength(dist = d.sp.i, grouping = id$sp.i$ife, season = "spring", typology = "ife", permutations = 99)
su.i.ife <- classification_strength(dist = d.su.i, grouping = id$su.i$ife, season = "summer", typology = "ife", permutations = 99)
au.i.ife <- classification_strength(dist = d.au.i, grouping = id$au.i$ife, season = "autumn", typology = "ife", permutations = 99)

table(id$au.i$ife)

brt.i <- rbindlist(list(sp.i.brt, su.i.brt, au.i.brt)) |> {\(x) x[,impaired := TRUE]}()

bgr.i <- rbindlist(list(sp.i.bgr, su.i.bgr, au.i.bgr)) |> {\(x) x[,impaired := TRUE]}()

ife.i <- rbindlist(list(sp.i.ife, su.i.ife, au.i.ife)) |> {\(x) x[,impaired := TRUE]}()

all <- rbindlist(list(brt.i, bgr.i, ife.i))



saveRDS(all, paste0("data/02_combined_data/",Sys.Date(),"_cs_results.rds"))
#all <- readRDS("data/02_combined_data/2021-10-12_cs_results.rds")

