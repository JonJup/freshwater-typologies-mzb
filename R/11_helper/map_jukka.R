data <- readRDS("data/02_combined_data/01_2021-10-14_combined_data_aggregated.rds")
BRT  <- st_read("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp")
ne  <- st_read("E://Arbeit/Data/natural_earth/My 10m/ne_10m_admin_0_map_units.shp")

library(sf)
library(data.table)
library(tmap)
library(dplyr)
library(mapview)

data2 <- rbindlist(data)
data3 <- copy(data2)
data3 <- data3[least.impacted == TRUE & brt12 == "RT01"]
data3 <- unique(data3, by = "gr_sample_id")

data4 <- st_as_sf(data3)

rt1 <- filter(BRT, m_btype12 == "RT1")
rt1 <- st_transform(rt1, 3035)

remove <- c("site_00710_date_00178_monitoring_poland", 
            "site_00446_date_00086_monitoring_poland", 
            "site_01535_date_00645_monitoring_poland",
            "site_00820_date_00182_monitoring_poland", 
            "site_00834_date_00196_monitoring_poland",
            "site_01497_date_00257_monitoring_poland",
            "site_00852_date_00171_monitoring_poland",
            "site_00920_date_00175_monitoring_poland",
            "site_00084_date_00389_monitoring_poland",
            "site_00990_date_00165_monitoring_poland",
            "site_00283_date_00077_monitoring_poland",
            "site_01414_date_00272_monitoring_poland",
            "site_16064_date_04849_monitoring_germany",
            "site_16779_date_04845_monitoring_germany",
            "site_16780_date_04845_monitoring_germany",
            "site_16781_date_04845_monitoring_germany",
            "site_00429_date_00129_monitoring_poland",
            "site_00689_date_00167_monitoring_poland",
            "site_01581_date_00262_monitoring_poland",
            "site_00777_date_00139_wiser",
            "site_16379_date_04709_monitoring_germany",
            "site_16378_date_04709_monitoring_germany",
            "site_15150_date_04570_monitoring_germany",
            "site_15076_date_04558_monitoring_germany",
            "site_01873_date_00211_naiades",
            "site_01879_date_00211_naiades",
            "site_00242_date_00003_monitoring_portugual",
            "site_00198_date_00003_monitoring_portugual",
            "site_00224_date_00003_monitoring_portugual",
            "site_00397_date_00003_monitoring_portugual",
            "site_00482_date_00003_monitoring_portugual",
            "site_00657_date_00003_monitoring_portugual",
            "site_00227_date_00137_monitoring_spain",
            "site_17554_date_04924_monitoring_germany",
            "site_03715_date_01223_naiades",
            "site_03713_date_00117_naiades", 
            "site_00671_date_00003_monitoring_portugual", 
            "site_15076_date_05828_monitoring_germany",
            "site_15150_date_05824_monitoring_germany",
            "site_01879_date_00509_naiades",
            "site_16781_date_04731_monitoring_germany", 
            "site_16779_date_04731_monitoring_germany",
            "site_16780_date_05471_monitoring_germany",
            "site_16064_date_05473_monitoring_germany",
            "site_16780_date_04731_monitoring_germany",
            "site_16064_date_04700_monitoring_germany",
            "site_00747_date_00190_monitoring_poland",
            "site_00394_date_00111_monitoring_poland",
            "site_01135_date_00176_monitoring_poland",
            "site_01587_date_00272_monitoring_poland",
            "site_01582_date_00264_monitoring_poland",
            "site_01082_date_00184_monitoring_poland",
            "site_00707_date_00167_monitoring_poland", 
            "site_01535_date_00226_monitoring_poland",
            "site_00084_date_00542_monitoring_poland", 
            "site_00895_date_00190_monitoring_poland",
            "site_01012_date_00211_monitoring_poland",
            "site_00105_date_00264_monitoring_poland",
            "site_00496_date_00077_monitoring_poland",
            "site_13067_date_04261_monitoring_germany",
            "site_14229_date_04452_monitoring_germany",
            "site_04712_date_00061_naiades",
            "site_00046_date_00088_monitoring_spain")

data5 <- filter(data4,! gr_sample_id %in% remove)

mapview(data5) + mapview(rt1)
basemap_tile <- rast("fig/basemap.tif")
ne <- st_transform(ne, 4326)
data4 <- st_transform(data4, 4326)
map_ob <- 
        #tm_shape(basemap_tile) + tm_rgb() + 
        tm_shape(ne) + tm_polygons() + 
       # tm_shape(rt1) + tm_lines(col = "blue") + 
        tm_shape(data4) + tm_dots(col = "red", size = .2) + 
        tm_compass(type = "4star", size = 2, position = c("left", "top"))
tmap_save(tm = map_ob, filename = "fig/map_graphical_abstract.png")
        