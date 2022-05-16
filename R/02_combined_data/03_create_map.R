# --------------------------------------- #
# --- Create maps of sampling sites ----- # 
# --------------------------------------- #

# 
#       date created: 29.04.22
# date last modified: 02.05.22
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: Create map of sampling sites
# 

# setup -----------------------------------------------------------------------------

pacman::p_load(sf, data.table, terra, tmap)

# prepare data ----------------------------------------------------------------------

# - load data

mzb <- readRDS("data/02_combined_data/02_2022-05-04_data_well_sampled_types.rds")
basemap.tile <- rast("fig/basemap.tif")


# - combine list elements to one table 
mzb <- rbindlist(mzb)

# - drop very old samples 
mzb <- mzb[year >= 2005 | data.set %in% c("Project STAR","Project AQEM (Sweden)", "Project AQEM (Romania)")]

# - turn season into ordered factor to ensure proper order in facetted map
mzb[, season := factor(season, levels = c("spring", "summer", "autumn"))]

# - reduce to one entry per site
sites <- unique(mzb, by = "gr_sample_id")

# - turn into sf object
sites <- st_as_sf(sites)

sites <- dplyr::filter(sites, least.impacted == TRUE)

# plot map --------------------------------------------------------------------------

# - first glance at data
mapview::mapview(sites)

# - plot map 
mzb_map <- 
        tm_shape(basemap.tile) + 
        tm_rgb() + 
        tm_shape(sites) + 
        tm_dots(col = "#CD5C5C",
                shape = 21,
                size = .05) + 
        tm_facets(by = "season", free.coords = FALSE, nrow = 1) + 
        tm_layout() + 
        tm_compass(type = "4star", size = 2, position = c("left", "top")) + 
        tm_scale_bar(text.size = 1)


# save to file ----------------------------------------------------------------------
tmap_save(tm = mzb_map, filename = paste0("fig/",Sys.Date(),"_map_of_samples.png"))

