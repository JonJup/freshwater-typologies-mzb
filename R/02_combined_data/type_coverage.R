### —————————————————————————————————— ###
### ———————— Type coverage ——————————— ### 
### —————————————————————————————————— ###

### ————————————————————
# date created: 11-10-21
# date last modified: 12-10-21
### ————————————————————

# set up  ---------------------------------------------------------------------------
library(data.table)
library(sf)
library(mapview)
library(dplyr)
library(tmap)

# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/01_2021-10-14_combined_data_aggregated.rds")
brt12 <- readRDS("data/all_typologies.rds")
bgr <- st_read("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp") |> st_transform(crs = 3035)
ife <- st_read("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp") |> st_transform(crs = 3035)

basemap <- terra::rast("fig/basemap.tif")
# prepare data -----------------------------------------------------------------------------
sites <- lapply(data, function(x) unique(x, by = "gr_sample_id"))
sites <- lapply(sites, function(x) x[distance < 500])

#- remove rare illies types (see below)
sites[[1]] <- sites[[1]][!ife %in% c("Baltic province", "Fenno-scandian shield", "Italy and Corsica", "Pyrenees", "Hellenic western Balkan", "Eastern Balkan", "Pontic province", "Dinaric western Balkan")]
sites[[2]] <- sites[[2]][!ife %in% c("Baltic province", "Italy and Corsica", "Pontic province", "Hellenic western Balkan", "Dinaric western Balkan", "Hungarian lowlands", "The Carpathiens")]
sites[[3]] <- sites[[3]][!ife %in% c("Hellenic western Balkan", "Tundra", "Italy and Corsica", "Eastern Balkan", "Pontic province")]

#- remove rare bgr types (see below)
sites[[1]] <- sites[[1]][!bgr %in% c("boreal", "pannonian")]
sites[[2]] <- sites[[2]][!bgr %in% c("pannonian")]
sites[[3]] <- sites[[3]][!bgr %in% c("arctic", "pannonian")]

# numbers ---------------------------------------------------------------------------
## -- brt 
sites[[1]]$brt12 |>  table() |>  min()
sites[[2]]$brt12 |>  table() |>  min()
sites[[3]]$brt12 |>  table() |>  min()
sites[[1]][least.impacted == TRUE,brt12] |> table() |> min()
sites[[2]][least.impacted == TRUE,brt12] |> table() |> min()
sites[[3]][least.impacted == TRUE,brt12] |> table() |> min()

## -- illies 
sites[[1]]$ife |>  table() |>  min()
sites[[2]]$ife |>  table() |> min()
sites[[3]]$ife |>  table() |>  min()
sites[[1]][least.impacted == TRUE, ife] |> table()  |> sort()
sites[[2]][least.impacted == TRUE, ife] |> table()  |> sort()
sites[[3]][least.impacted == TRUE, ife] |> table()  |> sort()

sites[[1]] |> st_as_sf() |> mapview(zcol = "ife")
sites[[2]] |> st_as_sf() |> mapview(zcol = "ife")
sites[[3]] |> st_as_sf() |> mapview(zcol = "ife")

## -- bgr 
sites[[1]]$bgr |>  table() |> sort()
sites[[2]]$bgr |>  table() |> sort()
sites[[3]]$bgr |>  table() |> sort()
sites[[1]][least.impacted == TRUE, bgr] |> table() |> sort() 
sites[[2]][least.impacted == TRUE, bgr] |> table() |> sort() 
sites[[3]][least.impacted == TRUE, bgr] |> table() |> sort() 

# distribution ----------------------------------------------------------------------

#- create one spatial "sites" data set 
sites2 <- rbindlist(sites) |> 
        st_as_sf() |> 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn")))
sites3 <- filter(sites2, least.impacted == TRUE)     

folder_name <- "21_10_14_maps"

#- Maps for broad river type 
for (i in 1:uniqueN(sites2$brt12)){
        tp = unique(sites2$brt12)[i]
        to_save <- tm_shape(basemap) + 
                tm_rgb() + 
                tm_shape(filter(brt12, brt12 == tp)) + 
                tm_lines(scale = 0.8, col = "blue") + 
                tm_shape(filter(sites2, brt12 == tp)) + 
                tm_dots(scale = 1.5, col = "red") + 
                tm_facets(by = "season", free.coords = FALSE) + 
                tm_layout(panel.labels = paste0(tp, "-", c("spring", "summer", "autumn"))) 
        tmap_save(to_save, filename = paste0("fig/",folder_name,"/brt_",tp,".png"))
                         
}
for (i in 1:uniqueN(sites3$brt12)){
        tp = unique(sites3$brt12)[i]
        to_save <- tm_shape(basemap) + 
                tm_rgb() + 
                tm_shape(filter(bgr12, bgr12 == tp)) + 
                tm_lines(scale = 0.8, col = "blue") + 
                tm_shape(filter(sites3, brt12 == tp)) + 
                tm_dots(scale = 1.5, col="red") + 
                tm_facets(by = "season", free.coords = FALSE) + 
                tm_layout(panel.labels = paste0(tp, "-", c("spring", "summer", "autumn"))) 
        tmap_save(to_save, filename = paste0("fig/",folder_name,"/brt_",tp,"_least_impacted.png"))
        
}
#- Maps for Illies Freshwater Ecoregions 
for (i in 1:uniqueN(sites2$ife)){
        tp = unique(sites2$ife)[i]
        to_save <- tm_shape(basemap) + 
                tm_rgb() + 
                tm_shape(filter(ife, NAME == tp)) + 
                tm_polygons(alpha = 0.3, col = "blue") + 
                tm_shape(filter(sites2, ife == tp)) + 
                tm_dots(scale = 1.5, col = "red") + 
                tm_facets(by = "season", free.coords = FALSE) + 
                tm_layout(panel.labels = paste0(tp, "-", c("spring", "summer", "autumn"))) 
        tmap_save(to_save, filename = paste0("fig/",folder_name,"/ife_",tp,".png"))
                         
}
for (i in 1:uniqueN(sites3$ife)){
        tp = unique(sites3$ife)[i]
        to_save <- tm_shape(basemap) + 
                tm_rgb() + 
                tm_shape(filter(ife, NAME == tp)) + 
                tm_polygons(alpha = 0.3, col = "blue") + 
                tm_shape(filter(sites3, ife == tp)) + 
                tm_dots(scale = 1.5, col = "red") + 
                tm_facets(by = "season", free.coords = FALSE) + 
                tm_layout(panel.labels = paste0(tp, "-", c("spring", "summer", "autumn"))) 
        tmap_save(to_save, filename = paste0("fig/",folder_name,"/ife",tp,"_least_impacted.png"))
        
}
#- Maps Biogeographic Regions 
for (i in 1:uniqueN(sites2$bgr)){
        tp = unique(sites2$bgr)[i]
        to_save <- tm_shape(basemap) + 
                tm_rgb() + 
                tm_shape(filter(bgr, short_name == tp)) + 
                tm_polygons(alpha = 0.3, col = "blue") + 
                tm_shape(filter(sites2, bgr == tp)) + 
                tm_dots(scale = 1.5, col = "red") + 
                tm_facets(by = "season", free.coords = FALSE) + 
                tm_layout(panel.labels = paste0(tp, "-", c("spring", "summer", "autumn"))) 
        tmap_save(to_save, filename = paste0("fig/",folder_name,"/bgr_",tp,".png"))
                         
}
for (i in 1:uniqueN(sites3$bgr)){
        tp = unique(sites3$bgr)[i]
        to_save <- tm_shape(basemap) + 
                tm_rgb() + 
                tm_shape(filter(bgr, short_name == tp)) + 
                tm_polygons(alpha = 0.3, col = "blue") + 
                tm_shape(filter(sites3, bgr == tp)) + 
                tm_dots(scale = 1.5, col = "red") + 
                tm_facets(by = "season", free.coords = FALSE) + 
                tm_layout(panel.labels = paste0(tp, "-", c("spring", "summer", "autumn"))) 
        tmap_save(to_save, filename = paste0("fig/",folder_name,"/bgr_",tp,"_least_impacted.png"))
}
