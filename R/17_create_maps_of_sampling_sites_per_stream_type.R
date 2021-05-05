# ---------------------------------------------- #
### --- create maps of sampling sites from --- ### 
# ---------------------------------------------- #

# --------------- #
# date:
#       16.04.21
# files in: 
#       
# files out:
#
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#       Plot all instances of a river type together with all available sampling sites 
# --------------- #

# setup -------------------------------------------------------------------
library(sf)
library(lubridate)
library(data.table)
library(dplyr)
library(magrittr)
library(tmap)
setwd(here::here())
tmap_mode("plot")
# load data -------------------------------------------------------------------------
europe <- st_read("D://Arbeit/Data/natural_earth/2020_06_29_europe.gpkg")
brt    <- st_read("D://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp")
GloRiC <- readRDS("D://Arbeit/Data/gloric/gloric_v10.RDS")
Illies <- st_read("D://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
BioGeo <- st_read("D://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
sites  <- readRDS("data/04_invertebrates_w_typologies.rds")

# carpeting --------------------------------------------------------------

## -- remove sites from before 2000 
sites =
        sites[year(date) >= 2000] %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf

## -- combine data in list 
typologies = list(
        brt = brt, 
        gloric = GloRiC,
        illies = Illies,
        biogeo = BioGeo
)

## -- transform typologies 
for (i in seq_along(typologies)){
        if (st_crs(typologies[[i]]) != st_crs(europe)) {
                typologies[[i]] <- st_transform(typologies[[i]], 
                                                crs = 4326)
        }
}


# Map  --------------------------------------------------------------------
## -- brt20
for (i in 1:20) {
        
        # declare variables 
        river     <- paste0("RT", i)
        map_title <- paste("Rivertype", i) 
        save_name <- paste0("figures/sampling_sites_per_river_type/brt20_", river, ".pdf")
        
        loop_sub_rivers <- filter(typologies$brt, m_btype20c  == river)
        loop_sub_sites  <- filter(sites, brt20 == river) 
        
        if (nrow(loop_sub_sites) == 0) next()
        
        loop_map <- tm_shape(europe) + tm_polygons() + 
                tm_shape(loop_sub_rivers) + tm_lines(col = "blue") + 
                tm_shape(loop_sub_sites) + tm_dots(col = "red", size = .1) + 
                tm_layout(title = map_title) + 
                tm_compass(position = c("left", "top")) + 
                tm_scale_bar(position = c("center", "bottom"))
        
        tmap_save(tm = loop_map, 
                  filename = save_name, 
                  )
        print(paste(i))
}
## -- brt12
for (i in 1:12) {
        
        # declare variables 
        river     <- paste0("RT", i)
        map_title <- paste("Rivertype", i) 
        save_name <- paste0("figures/sampling_sites_per_river_type/brt12_", river, ".pdf")
        
        loop_sub_rivers <- filter(typologies$brt, m_btype12 == river)
        loop_sub_sites  <- filter(sites, brt12 == river) 
        
        if (nrow(loop_sub_sites) == 0) next()
        
        loop_map <- tm_shape(europe) + tm_polygons() + 
                tm_shape(loop_sub_rivers) + tm_lines(col = "blue") + 
                tm_shape(loop_sub_sites) + tm_dots(col = "red", size = .1) + 
                tm_layout(title = map_title) + 
                tm_compass(position = c("left", "top")) + 
                tm_scale_bar(position = c("center", "bottom"))
        
        tmap_save(tm = loop_map, 
                  filename = save_name, 
        )
        print(paste(i))
}
## -- gloric 
uniqueN(typologies$gloric$Kmeans_30)
for (i in 1:30) {
        
        print(paste(i))
        # declare variables 
        river     <- paste(i)
        map_title <- paste("Rivertype", i) 
        save_name <- paste0("figures/sampling_sites_per_river_type/gloric_", river, ".pdf")
        
        loop_sub_rivers <- filter(typologies$gloric, Kmeans_30 == river)
        loop_sub_sites  <- filter(sites, gloric == river) 
        
        if (nrow(loop_sub_sites) == 0) next()
        
        loop_map <- tm_shape(europe) + tm_polygons() + 
                tm_shape(loop_sub_rivers) + tm_lines(col = "blue") + 
                tm_shape(loop_sub_sites) + tm_dots(col = "red", size = .1) + 
                tm_layout(title = map_title) + 
                tm_compass(position = c("left", "top")) + 
                tm_scale_bar(position = c("center", "bottom"))
        
        tmap_save(tm = loop_map, 
                  filename = save_name, 
        )
        print(paste(i))
}
## -- Illies
       
