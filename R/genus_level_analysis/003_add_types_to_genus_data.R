### -------------------------------------------- ###
### -------- ADD TYPES TO GENUS DATA ----------- ### 
### -------------------------------------------- ###

# ____________________________
# date created: 28.04.22
# date last modified: 28.04.22
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: Add Illies and BGR types to genus data
#___________________________


# setup -----------------------------------------------------------------------------
library(pacman)
p_load(data.table, sf, rstudioapi, magrittr, dplyr)

x<-getActiveDocumentContext()
sink(file = paste0("R/STOTEN-reivew/003","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
biogeographic_regions <-        st_read("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
illies_freshwater_ecoregions <- st_read("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
macroinvertebrates           <- readRDS("data/review/01_2022-04-28_combined_data_genus_realtive_abundance.rds")

# add types -------------------------------------------------------------------------
biogeographic_regions %<>% st_make_valid()
biogeographic_regions %<>% st_transform(crs = "EPSG:3035")
illies_freshwater_ecoregions %<>% st_transform(crs = "EPSG:3035")

macroinvertebrates %<>%rbindlist()

biogeographic_regions %<>% select(short_name)
biogeographic_regions2 <- st_cast(biogeographic_regions, "POLYGON")
illies_freshwater_ecoregions%<>%select(NAME)

sites <- macroinvertebrates  |> unique(by = "gr_sample_id") |> st_as_sf()
sites%<>%st_join(illies_freshwater_ecoregions)
sites%<>%rename(illies = NAME)
sites%<>%st_join(biogeographic_regions2)
sites%<>%rename(bgr = short_name)
sites%<>%select(illies, bgr, gr_sample_id) %>% st_drop_geometry()
setDT(sites)
macroinvertebrates <- sites[macroinvertebrates, on = "gr_sample_id"]

macroinvertebrates <- list(
        spring = macroinvertebrates[season == "spring"],
        summer = macroinvertebrates[season == "summer"],
        autumn = macroinvertebrates[season == "autumn"]
)

# save to file ----------------------------------------------------------------------
saveRDS(macroinvertebrates, paste0("data/review/02_",Sys.Date(), "_genus_w_added_types.rds"))

