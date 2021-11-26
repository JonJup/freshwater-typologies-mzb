# ————————————————————————— #
# ——— Combine data sets ——— # 
# ——— unaggregated      ——— # 
# ————————————————————————— #

# ———————————————————————————————————
#       date created: 21-10-21
# date last modified: 21-10-21
#            Project: Evaluating European Broad River Types for Macroinvertebrates
#            Purpose: Combine unaggregated data sets
# ————————————————

# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, fs, magrittr, sf, stringr, tmap)

# LOAD DATA -------------------------------------------------------------------------
#- illies freshwater ecoregions 
# illies <- st_read("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp") |> st_transform(crs = 3035)# illies <- st_read("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp") |> st_transform(crs = 3035)
# bgr    <- st_read("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp") |> st_transform(crs = 3035)# bgr    <- st_read("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp") |> st_transform(crs = 3035)
#- list of all data sets 
data.sets <- dir_ls("data/01_original_data", type = "directory", regexp = "pre_", invert = TRUE)

#- At this point several data sets are omitted from the analysis. 
#- Belgium monitoring: no least impacted sites 
data.sets <- data.sets[- which(str_detect(data.sets, "belgium"))]
#- Ecosurv: no dipterans
data.sets <- data.sets[- which(str_detect(data.sets, "ecosurv"))]
#- missing 6 orders, noticeably it is the only data set missing "Pulmonata" and Isopoda"
data.sets <- data.sets[- which(str_detect(data.sets, "oscar_belmar"))]
#- Monitoring Romania: No Snails 
data.sets <- data.sets[-which(str_detect(data.sets, "monitoring_romania"))]

data      <- list()

#- loop over all (currently 23) data sets to load them as elements of the list 
#- "data"
for (i in seq_along(data.sets)){
        i.ds <- data.sets[i]
        print(paste("LOADING", str_remove(i.ds, "data/01_original_data/")))
        i.files <- dir_ls(i.ds, regexp = "final_non_aggregated")
        i.x     <- readRDS(i.files)
        data[[i]] <- i.x 
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
}

# PREPARE DATA ----------------------------------------------------------------------
#- In these next steps, I apply several functions to all elements of the list "data", 
#- i.e. data sets to ensure that they are harmonized. 

#- Make sure all date variables are formatted as such:
data2    <- lapply(data, function(x) x[, date := as.Date(date)])
#- Remove the newest_date column that was created in an old version of the 
#- newest_sample() function. Not all data sets have this column so warning: 
#- "Column 'newest_date' does not exist to remove" is thrown (currently 
#- for 11 data sets).
data2    <- lapply(data2, function(x) x[, newest_date := NULL])
#- Make data spatial (sf) and transform to common coordinate reference system (LAEA Europe). 
data.st <- lapply(data2, function(x) st_as_sf(x, coords = c("x.coord", "y.coord"), crs = x$EPSG[1]))
data.st <- lapply(data.st, function(x) st_transform(x, crs = 3035))
#- Turn back into data.table to bind rows of list elements 
data2   <- lapply(data.st, setDT)
data2   <- rbindlist(data2, fill = TRUE, use.names = TRUE)
#- Remove unnecessary columns
# add both bgr and ife again 
data2[, c("sampling.events", "bgr", "ife") := NULL]

# #- add Illies freshwater ecoregion 
# sites <- unique(data2, by = "gr_sample_id") |> st_as_sf()
# illies %<>% select(NAME)
# bgr    %<>% select(short_name)
# 
# 
# sites_illies <- st_join(sites, illies)
# data3 <-
#   sites_illies |>
#   select(gr_sample_id, NAME) |>
#   st_drop_geometry() |>
#   rename(ife = NAME) |>
#   right_join(data2, by = "gr_sample_id") |>
#   setDT()  
# sites_bgr <- st_join(sites, bgr)
# data4 <-
#   sites_bgr |>
#   select(gr_sample_id, short_name) |>
#   st_drop_geometry() |>
#   rename(bgr = short_name) |>
#   right_join(data3, by = "gr_sample_id") |>
#   setDT()  


#- Norway is outside the data set of Lemm et al. 21. According to an email from 
#- Leonard Sandin, who provided the data, on the 01-10-21 all sites are in reference 
#- condition. 
data2[data.set == "monitoring_norway", least.impacted := T]


# save to file  ---------------------------------------------------------------------


saveRDS(data2, paste0("data/02_combined_data/01_", Sys.Date(), "_all_data_non_aggregated.rds"))
data2 <- readRDS("data/02_combined_data/01_2021-10-21_all_data_non_aggregated.rds")

library(dplyr)
data2[, richness := .N, by = c("data.set", "site_id")]
data3 <- data2[season != "winter"]
nrow(unique(data3, by = c("site_id", "data.set")))

