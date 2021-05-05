# ---------------------------------------------------------- #
### --- Remove sites with large distance to next river --- ### 
# ---------------------------------------------------------- #

# --------------- #
# date:  17.03.21 
#               + 04.05.21 (add BRT12 here)
# files in 
        #-> m_river_fec_broad_type.shp | Broad River Types Shape file 
        #-> 01_all_mzb_combined.rds    | Combined Invertebrate data sets 
# files out
        #<- 02_data_close.rds
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates  
# Purpose: 
#       Remove sites that are to far away from closest river and assign river 
#       types to remaining sites.
# --------------- #

# Setup ----------------------------------------------------------------
source("R/setup_combined_inv.R")

# maximal distance to next river (in meters)
cut_off = 500 

# load data  -----------------------------------------------------------
sf_river = st_read(file.path(dir$ls, "m_river_fec_broad_type.shp"))
dt_data  = readRDS("data/01_all_mzb_combined.rds")

# clean data -----------------------------------------------------------
dt_sites = unique(dt_data, by = "gr_sample_id")
sf_sites = st_as_sf(dt_sites)
sf_sites %<>% dplyr::select(gr_sample_id)
sf_sites %<>% st_transform(crs = 3035)
sf_river %<>% dplyr::select(m_btype20c, m_btype12)
sf_river %<>% st_transform(crs = 3035)

# add catchment info to bio data ------------------------------------------

nn = st_nearest_feature(sf_sites, sf_river); beep()
sum(is.na(nn))
rivers_resorted <- sf_river[nn,]

# the code that is commented out below is time consuing to run and the resulting object (distance_list)
# is provided as .rds file in the data directory. 

# distance_list <-
#         map(.x = 1:nrow(sf_sites),
#             .f = ~ as.numeric(st_distance(x = sf_sites[.x, ],
#                                           y = rivers_resorted[.x, ])))
# 
# saveRDS(distance_list, "data/02_distance_list.rds")

# load distance list 
distance_list = readRDS("data/02_distance_list.rds")

distance_table <- data.table("gr_sample_id" = sf_sites$gr_sample_id,
                              "nn_distance" = unlist(distance_list),
                              "brt20"    = rivers_resorted$m_btype20c,
                              "brt12"    = rivers_resorted$m_btype12
                             )

hist(distance_table$nn_distance, breaks = 100)

# investigate far away sites
distance_table %>% 
        filter(nn_distance > cut_off) %>% 
        pull(gr_sample_id) -> 
        far_away_sites

# write to file to investigate in QGIS or similar GIS software. 
# sf_sites %>%
#         filter(gr_sample_id %in% far_away_sites) %>%
#         st_write("test2.gpkg")

# subset to close sites 
distance_table2 <- distance_table[nn_distance <= cut_off]
hist(distance_table2$nn_distance)


# Filter data -------------------------------------------------------------
# filter site data to close sites 
sf_sites2 = filter(sf_sites, gr_sample_id %in% distance_table2$gr_sample_id)
# join site data with distance which holds the river type 
sf_sites3 <- left_join(
        sf_sites2,
        distance_table2,
        by = "gr_sample_id"
        )

# remove distance column 
sf_sites4 = dplyr::select(sf_sites3, -nn_distance)
# subset observations to close sites
dt_data2 <- dt_data[gr_sample_id %in% sf_sites4$gr_sample_id]
# join river type and observation 
tb_data3 <- left_join(dt_data2, 
                     distance_table2)
# remove distance column 
tb_data3 %<>% dplyr::select(-nn_distance)
setDT(tb_data3)

# Save to File  -----------------------------------------------------------
saveRDS(object = tb_data3,
        file = "data/02_data_close.rds")
