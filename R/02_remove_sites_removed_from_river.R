# ---------------------------------------------------------- #
### --- Remove sites with large distance to next river --- ### 
# ---------------------------------------------------------- #

# --------------- #
# files in 
        #-> m_river_fec_broad_type.shp | Broad River Types Shape file 
        #-> 01_all_mzb_combined.rds    | Combined Invertebrate data sets 
# files out
        #<- 02_data_close.rds
# Purpose: 
#       Remove sites that are to far away from closest river and assign river 
#       types to remaining sites.
# --------------- #

# Setup ----------------------------------------------------------------
pacman::p_load(
        data.table, dplyr, sf
)

dir <- list()
## -- enter directory of broad river types shape file 
dir$ls = 

# load data  -----------------------------------------------------------
sf_river = st_read(file.path(dir$ls, "m_river_fec_broad_type.shp"))
dt_data  = readRDS("data/01_all_mzb_combined.rds")

# clean data -----------------------------------------------------------

## -- maximal distance to next river (in meters)
cut_off = 500 

dt_sites = unique(dt_data, by = "gr_sample_id")
sf_sites = st_as_sf(dt_sites)
sf_sites %<>% dplyr::select(gr_sample_id)
sf_sites %<>% st_transform(crs = 3035)
sf_river %<>% dplyr::select(m_btype20c, m_btype12)
sf_river %<>% st_transform(crs = 3035)

# add catchment info to bio data ------------------------------------------

nn = st_nearest_feature(sf_sites, sf_river)
rivers_resorted <- sf_river[nn,]

## - The code that is commented out below is time consuming to run and the resulting object (distance_list)
## - is provided as .rds file in the data directory. 

# distance_list <-
#         map(.x = 1:nrow(sf_sites),
#             .f = ~ as.numeric(st_distance(x = sf_sites[.x, ],
#                                           y = rivers_resorted[.x, ])))
# 
# saveRDS(distance_list, "data/02_distance_list.rds")

## - load distance list 
distance_list  <- readRDS("data/02_distance_list.rds")
distance_table <- data.table(
        "gr_sample_id" = sf_sites$gr_sample_id,
        "nn_distance" = unlist(distance_list),
        "brt20"    = rivers_resorted$m_btype20c,
        "brt12"    = rivers_resorted$m_btype12
)

## - Histogram of distances 
hist(distance_table$nn_distance, breaks = 100)

## - A closer look at far away sites
distance_table %>% 
        filter(nn_distance > cut_off) %>% 
        pull(gr_sample_id) -> 
        far_away_sites

## - write to file to investigate in QGIS or similar GIS software. 
sf_sites %>%
        filter(gr_sample_id %in% far_away_sites) %>%
        st_write("test2.gpkg")

## - subset to close sites 
distance_table2 <- distance_table[nn_distance <= cut_off]

# Filter data -------------------------------------------------------------
## - filter site data to close sites 
sf_sites2 = filter(sf_sites, gr_sample_id %in% distance_table2$gr_sample_id)
## - join site data with distance which holds the river type 
sf_sites3 <- left_join(
        sf_sites2,
        distance_table2,
        by = "gr_sample_id"
        )

## - remove distance column 
sf_sites4 = dplyr::select(sf_sites3, -nn_distance)
## - subset observations to close sites
dt_data2 <- dt_data[gr_sample_id %in% sf_sites4$gr_sample_id]
## - join river type and observation 
tb_data3 <- left_join(dt_data2, 
                     distance_table2)
## - remove distance column 
tb_data3 %<>% dplyr::select(-nn_distance)
setDT(tb_data3)

# Save to File  -----------------------------------------------------------
saveRDS(object = tb_data3,
        file = "data/02_data_close.rds")
