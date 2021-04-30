# ------------------------------------------ #
### --- Determine Impact via landcover --- ### 
# ------------------------------------------ # 

# --------------- #
# date:  17.03.21
# files in 
        #-> reduced_clc.RDS   | Corine land cover of CCM2 catchments 
        #-> 02_data_close.RDS | data close to rivers 
# files out
        #<- 03_data_low_impact.rds
# Project: 
#       Evaluating European Broad River Types for Macroinvertebrates 
# Purpose: 
#       1. add CCM catchment ID (WSO) to sites
#       2. add landcover information to sites 
#       3. judge impact based on landcover
#       4. remove all sites that are not in reference-conditions
# --------------- #

# Setup -------------------------------------------------------------------
source("R/setup_combined_inv.R")

# Load data ---------------------------------------------------------------
cat =  st_read(file.path(dir$ccm, "2019-06-05_allGRcountires_WGS84.gpkg"))
mzb =  readRDS("data/02_data_close.RDS")       
landcover = readRDS(file.path(dir$crn, "reduced_clc.RDS"))
#observations   <- readRDS(file.path(dir$dt,  "03_data_w_WSO.RDS"))

# add catchment id  -----------------------------------------------------------
mzb  = st_as_sf(mzb)
mzb  = st_transform(mzb, crs = 4326)
cat  = dplyr::select(cat, WSO1_ID)
mzb = st_join(x = mzb,
               y = cat,
               join = st_intersects)


# add landcover data ------------------------------------------------------
setDT(mzb)

# extract unqiue sites from observations, join landcover and add binary
# variables that indicates whether the combined area of landuse and urban areas
# exceeds 20% 
site = 
        mzb %>% 
        unique(by = "gr_sample_id") %>% 
        left_join(landcover, 
                  by = "WSO1_ID") %>% 
        mutate(impact_lc = ifelse(Sum2 + Sum1 >= 20, 0, 1)) %>% 
        dplyr::select(
                      gr_sample_id,
                      impact_lc,
                      WSO1_ID) %>% 
        setDT()


mzb2 = site[mzb, on = "gr_sample_id"]
mzb2[, low_impact := ifelse(is.na(pristine), impact_lc, pristine)]
mzb2_low = mzb2[low_impact == 1]
sites_low <- unique(mzb2_low,by = "gr_sample_id")

# Save to file  -----------------------------------------------------------
saveRDS(file = "data/03_data_low_impact.rds",
        object = mzb2_low)


