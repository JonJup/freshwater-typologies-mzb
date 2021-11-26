#  — Macroinvertebrate Maps  — # 

# setup -----------------------------------------------------------------------------
pacman::p_load(
        sf,
        tmap,
        data.table,
        fs,
        lubridate,
        stringr,
        dplyr,
        terra,
        maptiles,
        magrittr
)
tmap_mode("view")


# options ---------------------------------------------------------------------------

opt <- list()
#- create static png maps for paper? 
opt$static <- TRUE
#- create dynamic seasonal maps for paper
opt$season <- FALSE


# load data -------------------------------------------------------------------------
most_recent_date <- 
        dir_ls("data/02_combined_data/", regexp = "combined_data.rds") |> 
        str_remove("data/02_combined_data/01_") |> 
        str_remove("_combined_data.rds") |> 
        ymd() |> 
        {\(x) x[which.max(x)]}()

data <- readRDS("data/02_combined_data/02_2021-10-14_core_taxa_data_aggregated.rds")
brt12 <- st_read("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp")
rm(most_recent_date)

#- extract sites from data for each season
sites     <- lapply(seq_along(data), function(x) st_as_sf(unique(data[[x]], by = "gr_sample_id"), crs = 3035))
#- create data set with all three seasons 
sites_all <- rbindlist(sites)
#- define custom color palette
custom.color.palette <- c("#EC6B4F", "#65F78D")
        
#- create sites2. , 
sites_all |> 
        # combine seasons and transfrom to sf object 
        st_as_sf() |> 
        # only least impacted sites that are close to river segments
        filter(least.impacted == TRUE & distance <= 500) |> 
        # Keep only data after 2005 (or from AQEM, STAR)
        filter(year >= 2005 | data.set %in% c("Project STAR","Project AQEM (Sweden)", "Project AQEM (Romania)")) |> 
        #filter(!is.na(season)) |> 
        # #- transform to 
        st_transform(4326) |> 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn"))) -> 
        #mutate(data.set.id = factor(data.set.id))  
        sites2

if (opt$static){
        tmap_mode("plot")
        ## -- load basemap from hard drive or web 
        if ("fig/basemap.tif" %in% fs::dir_ls("fig/")) {
                basemap.tile <- rast("fig/basemap.tif")
        } else {
                basemap.tile <-
                        get_tiles(sites2,
                                  provider = "Esri.OceanBasemap",
                                  zoom = 7,
                                  crop = TRUE)
                writeRaster(basemap.tile, "fig/basemap.tif")
        }
        ## -- create map 
        map.static <-
                tm_shape(basemap.tile) +
                tm_rgb () +
                tm_shape(sites2) +
                tm_symbols(col = "#CD5C5C",
                           #col = "data.set.id",
                           shape = 21,
                           size = .05) +
                tm_facets(by = "season", free.coords = FALSE, nrow = 1)
        ## -- save map to file 
        tmap_save(tm = map.static, 
                  filename = paste0("fig/", Sys.Date(), "_map.png"))
}


#-- map brt12 

brt12.static <-
        tm_shape(basemap.tile) +
        tm_rgb () +
        tm_shape(brt12, legend.show = FALSE) +
        tm_lines(col = "m_btype12")  
tmap_save(tm = brt12.static, 
          filename = paste0("fig/", Sys.Date(), "_brt12.png"))


tmap_mode("view")
spring <- sites2 |> filter(season == "spring") |> tm_shape() + tm_dots(col = "data.set")
summer <- sites2 |> filter(season == "summer")|> tm_shape() + tm_dots(col = "data.set")
autumn <- sites2 |> filter(season == "autumn")|> tm_shape() + tm_dots(col = "data.set")
#winter <- tm_shape(sites[[4]]) + tm_dots(col = "data.set")

tmap_save(tm = spring, filename = "fig/animated_sample_maps/map_samples_spring_by_data_set.html")
tmap_save(tm = summer, filename = "fig/animated_sample_maps/map_samples_summer_by_data_set.html")
tmap_save(tm = autumn, filename = "fig/animated_sample_maps/map_samples_autumn_by_data_set.html")

#- map for paper 
tmap_save(tm = spring, filename = "fig/animated_sample_maps/map_samples_spring_by_data_set.html")
tmap_save(tm = summer, filename = "fig/animated_sample_maps/map_samples_summer_by_data_set.html")
tmap_save(tm = autumn, filename = "fig/animated_sample_maps/map_samples_autumn_by_data_set.html")

# close to BRT 
sites_all_brt <- sites_all[,'brt_bool' := brt_distance <=500]
sites_all_brt <- sites_all[,'close to BRT river segment?' := ifelse(brt_bool, "yes", "no")]
sites_all_brt <- st_as_sf(sites_all_brt)

brt_map <- tm_shape(sites_all_brt) + tm_dots(col = 'close to BRT river segment?', palette = custom.color.palette)
tmap_save(tm = brt_map, filename = "fig/animated_sample_maps/close_to_brt.html")

# far from border 
bgr.distance.obj <- sites_all$bgr_distance
bgr.distance.obj2 <- vector(mode = "numeric", length = length(bgr.distance.obj))
for (i in seq_along(bgr.distance.obj)) bgr.distance.obj2[i] <- ifelse(is.null(bgr.distance.obj[[i]]), 25001, bgr.distance.obj[i])
sites_all[, bgr_distance2 := bgr.distance.obj2]
sites_all_border <- sites_all[,region_bool := bgr_distance2 > 2500 & illies_distance >25000]
sites_all_border[, 'Removed from Region borders?' := ifelse(region_bool, "Yes", "No")]
sites_all_border <- st_as_sf(sites_all_border)
region_map <- tm_shape(sites_all_border) + tm_dots(col = 'Removed from Region borders?', palette = custom.color.palette)
tmap_save(tm = region_map, filename = "fig/animated_sample_maps/removed_from_region.html")

# least impacted 
sites_all[, "Least Impacted?" := ifelse(fec.least.impacted, "Yes", "No")]
sites.all.sf <- st_as_sf(sites_all)
region_map <- tm_shape(sites.all.sf) + tm_dots(col = 'Least Impacted?', palette = custom.color.palette)
tmap_save(tm = region_map, filename = "fig/animated_sample_maps/least_impacted.html")

#- Least impacted & far from region border & close to brt segment 
sites_all[, final.selection := region_bool & brt_distance <=500 & fec.least.impacted]
sites_all[, 'Fulfills all criteria?' := ifelse(final.selection, "Yes", "No")]
sites.all.sf <- st_as_sf(sites_all)
region_map <- tm_shape(sites.all.sf) + tm_dots(col = 'Fulfills all criteria?', palette = custom.color.palette)
tmap_save(tm = region_map, filename = "fig/animated_sample_maps/fulfills_all_criteria.html")
