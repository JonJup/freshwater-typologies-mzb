# —————————————————————————————————————————— #
# ——— Clean monitoring data from Romania ——— # 
# —————————————————————————————————————————— #


# ———————————————————————————————————
# date created:  02.08.21
# date last modified:  22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data provided by Mirella Cimpean. 
# Temporal aggregation: yes 
# EPSG: 4326
# ————————————————

# SETUP  -----------------------------------------------------------------------------

pacman::p_load(
        here,
        taxize,
        data.table,
        sf,
        dplyr,
        lubridate,
        magrittr,
        mapview,
        purrr,
        readr,
        readxl,
        sp,
        stringr,
        tidyr,
        xlsx
)



# LOAD DATA  ------------------------------------------------------------------------
data1 <- read_xlsx( "data/01_original_data/monitoring_romania/raw_data/Aries_River_Romania_GetReal.xlsx",  col_types = c("text", "text", "date", "text", "text", "text", "numeric"))
data2 <- read_xlsx( "data/01_original_data/monitoring_romania/raw_data/Capra_River_Romania_GetReal -.xlsx") 
data3 <- read.xlsx2("data/01_original_data/monitoring_romania/raw_data/Caras_River_Romania.xls", sheetIndex = 1, colIndex = 1:8, colClasses = c("character", "character", "Date", "character", "character", "character", "numeric")) 
data4 <- read_xlsx( "data/01_original_data/monitoring_romania/raw_data/Dejani_River_Romania_GetReal.xlsx") 
data5 <- read_xlsx( "data/01_original_data/monitoring_romania/raw_data/Sebes_River_Romania_GetReal.xlsx") 
data6 <- read.xlsx2("data/01_original_data/monitoring_romania/raw_data/Tur_River_Romana.xls",sheetIndex = 1, colClasses = c("character", "character", "Date", "character", "character", "character", "numeric"))
most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data/", file = "taxontable.rds")              
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")


# PREPARE DATA   ---------------------------------------------------------------------
setDT(data1)
setDT(data2)
setDT(data3)
setDT(data4)
setDT(data5)
setDT(data6)

#  —————— data set 1 Aries river —————— # 
data1 = data1[-1,]
x = data1[,c("X coordinates", "Y coordinates")]
anyNA(x)
names(x) <- c("lat", "lon")        
chd = "°"
chm = "’"
chs = "”"

x$lat %<>% str_remove("N") %>% str_trim    
x$lon %<>% str_remove("E") %>% str_trim    

cd.lat <- char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.lon <- char2dms(x$lon, chd = chd, chm = chm, chs = chs)

data12 = data1[, list(
        original_site_name = Site_name,
        date = ymd(`sampling date`),
        taxon = `taxon name`,
        x.coord = as.numeric(cd.lon),
        y.coord = as.numeric(cd.lat),
        EPSG = `Coordinate reference system`
)]
data12[, c("year", "season") := list(
        year = year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]

anyNA(data12$x.coord)
anyNA(data12$y.coord)

sites <- data12 |> unique(by = "original_site_name") 
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 4326) 
mapview(sites)

#  —————— data set 2 —————— # 

data21 = data2[, list(
        original_site_name = Code,
        date = data2$`sampling date` %>% dmy(),
        taxon = data2$`taxon name`,
        x.coord = data2$Longitudine,
        y.coord = data2$Latitudine,
        EPSG = data2$`Coordinate reference system`
)]

x = data21[,c("x.coord", "y.coord")]

anyNA(x)

names(x) <- c("long", "lat")        
chd = "°"
chm = "'"
chs = "\\."

x$lat  %<>% substr(start = 1, stop = 9)
x$long %<>% substr(start = 1, stop = 9)

cd.lat  <- char2dms(x$lat,  chd = chd, chm = chm, chs = chs)
cd.long <- char2dms(x$long, chd = chd, chm = chm, chs = chs)

data21 = data21[, c("x.coord", "y.coord", "year", "season") := list(
        as.numeric(cd.long),
        as.numeric(cd.lat),
        year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]

anyNA(data21$x.coord)
anyNA(data21$y.coord)

sites <- data21 |> unique(by = "original_site_name") 
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 4326) 
mapview(sites)

#  —————— data set 3 —————— # 

data3 <- data3[-1,]
x  <- data3[,c("X.coordinates.", "Y.coordinates.")]
anyNA(x)
names(x) <- c("lat", "lon")        
chd = "º"
chm = "'"
chs = "\\."

#- remove North and East letters 
x$lat %<>% str_remove("N") %>% str_trim    
x$lon %<>% str_remove("E") %>% str_trim    
#- drop last two numbers 
x$lat  %<>% substr(start = 1, stop = 9)
x$lon  %<>% substr(start = 1, stop = 9)
#- transfrom string to coordinates (function from sp package)
cd.lat = char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.lon = char2dms(x$lon, chd = chd, chm = chm, chs = chs)
#- update data set 
data32 = data3[, list(
        original_site_name = Site_name.,
        date = ymd(`sampling.date.`),
        taxon = `taxon.name.`,
        x.coord = as.numeric(cd.lon),
        y.coord = as.numeric(cd.lat),
        EPSG = `Coordinate.reference.system.`
)]
#- add year and season variable 
data32[, c("year", "season") := list(
        year = year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]

#- check coordinates on map 

sites <- data32 |> unique(by = "original_site_name") 
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 4326) 
mapview(sites)

#  ————————— data set 4 ———————————— # 
#- reshape data 
data41 = data4[, list(
        original_site_name = Code,
        date = data4$`sampling date` %>% dmy(),
        taxon = data4$`taxon name`,
        x.coord = data4$Longitudine,
        y.coord = data4$Latitudine,
        EPSG = data4$`Coordinate reference system`
)]
#- extract coordinates
x <- data41[,c("x.coord", "y.coord")]
anyNA(x)
names(x) <- c("lon", "lat")        
chd = "°"
chm = "'"
chs = "\\."

x$lat %<>% str_remove_all("N") %>% str_trim
x$lon %<>% str_remove_all("N") %>% str_trim

cd.lat  <- char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.lon <-  char2dms(x$lon, chd = chd, chm = chm, chs = chs)

data41 = data41[, c("x.coord", "y.coord", "year", "season") := list(
        as.numeric(cd.lon),
        as.numeric(cd.lat),
        year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]


#- check coordinates on map 
sites <- data41 |> unique(by = "original_site_name") 
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 4326) 
mapview(sites)

#  ————————— data set 5 ———————————— # 
#- reshape data 
data51 = data5[, list(
        original_site_name = Code,
        date = data5$`sampling date` %>% dmy(),
        taxon = data5$`taxon name`,
        x.coord = data5$Longitudine,
        y.coord = data5$Latitudine,
        EPSG = data5$`Coordinate reference system`
)]

x = data51[,c("x.coord", "y.coord")]

names(x) <- c("lon", "lat")        
chd = "°"
chm = "'"
chs = "\\."

x$lat %<>% str_remove_all("N") %>% str_trim
x$lon %<>% str_remove_all("E") %>% str_trim

#- drop last two numbers 
x$lat  %<>% substr(start = 1, stop = 9)
x$lon  %<>% substr(start = 1, stop = 9)

cd.lat  <- char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.lon <- char2dms(x$lon, chd = chd, chm = chm, chs = chs)

data51 = data51[, c("x.coord", "y.coord", "year", "season") := list(
        as.numeric(cd.lon),
        as.numeric(cd.lat),
        year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]

#- check coordinates on map 
sites <- data51 |> unique(by = "original_site_name") 
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 4326) 
mapview(sites)

#  ————————— data set 6 ———————————— # 

data6 = data6[-1,]
x = data6[,c("X.coordinates.", "Y.coordinates.")]

names(x) <- c("lat", "lon")        
anyNA(x)

chd = "°"
chm = "’"
chs = "’’"

x$lat %<>% str_remove("N") %>% str_trim    
x$lon %<>% str_remove("E") %>% str_trim    

cd.lat <- char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.lon <- char2dms(x$lon, chd = chd, chm = chm, chs = chs)

data62 = data6[, list(
        original_site_name = Site_name.,
        date = ymd(`sampling.date.`),
        taxon = `taxon.name.`,
        x.coord = as.numeric(cd.lon),
        y.coord = as.numeric(cd.lat),
        EPSG = `Coordinate.reference.system.`
)]

data62[, c("year", "season") := list(
        year = year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]


#- check coordinates on map 
sites <- data62 |> unique(by = "original_site_name") 
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = 4326) 
mapview(sites)


## -- combine into one dataset 

data = rbindlist(list(data62, data51, data41, data32, data21, data12))
data2 = data

#- check on map 
data.sp <- 
        data2 |> 
        unique(by = "original_site_name") |> 
        filter(!is.na(x.coord)) |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) 
mapview(data.sp)

data2 = data2[, list(
        original_site_name,
        date = ymd(date),
        year = year(date),
        month = month(date),
        taxon,
        y.coord,
        x.coord,
        abundance = NA,
        EPSG,
        data.set = "mirela_cimpean"
)]

data2[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]

(TU <- sort(unique(data2$taxon)))
new_tu <- which(!TU %in% taxontable$original_name)
TU <- TU[new_tu]

#- sort out larger diatom genera
data2 <- data2[!str_detect(string = taxon, pattern = "Achnanthidium")]
data2 <- data2[!str_detect(string = taxon, pattern = "Amphora")]
data2 <- data2[!str_detect(string = taxon, pattern = "Adlafia")]
data2 <- data2[!str_detect(string = taxon, pattern = "Bacillaria")]
data2 <- data2[!str_detect(string = taxon, pattern = "Cocconeis")]
data2 <- data2[!str_detect(string = taxon, pattern = "Cymbella")]
data2 <- data2[!str_detect(string = taxon, pattern = "Diatoma")]
data2 <- data2[!str_detect(string = taxon, pattern = "Didymosphenia")]
data2 <- data2[!str_detect(string = taxon, pattern = "Diploneis")]
data2 <- data2[!str_detect(string = taxon, pattern = "Encyonema")]
data2 <- data2[!str_detect(string = taxon, pattern = "Eunotia")]
data2 <- data2[!str_detect(string = taxon, pattern = "Fragilaria")]
data2 <- data2[!str_detect(string = taxon, pattern = "Iconella")]
data2 <- data2[!str_detect(string = taxon, pattern = "Cymatopleura")]
data2 <- data2[!str_detect(string = taxon, pattern = "Craticula")]
data2 <- data2[!str_detect(string = taxon, pattern = "Paraplaconeis")]
data2 <- data2[!str_detect(string = taxon, pattern = "Tabellaria")]
data2 <- data2[!str_detect(string = taxon, pattern = "Gomphonema")]
data2 <- data2[!str_detect(string = taxon, pattern = "Gyrosigma")]
data2 <- data2[!str_detect(string = taxon, pattern = "Hannaea arcus")]
data2 <- data2[!str_detect(string = taxon, pattern = "Neidium")]
data2 <- data2[!str_detect(string = taxon, pattern = "Pinnularia")]
data2 <- data2[!str_detect(string = taxon, pattern = "Psammothidium ")]
data2 <- data2[!str_detect(string = taxon, pattern = "Sellaphora")]
data2 <- data2[!str_detect(string = taxon, pattern = "Suriella")]
data2 <- data2[!str_detect(string = taxon, pattern = "Surirella")]
data2 <- data2[!str_detect(string = taxon, pattern = "Navicula")]
data2 <- data2[!str_detect(string = taxon, pattern = "Nitzschia")]
data2 <- data2[!str_detect(string = taxon, pattern = "Ulnaria")]
data2 <- data2[!str_detect(string = taxon, pattern = "Stauroneis")]
data2 <- data2[!str_detect(string = taxon, pattern = "Tryblionella")]
data2 <- data2[!str_detect(string = taxon, pattern = "Tetracyclus")]
data2 <- data2[!str_detect(string = taxon, pattern = "Melosira")]
data2 <- data2[!str_detect(string = taxon, pattern = "Meridion")]
data2 <- data2[!str_detect(string = taxon, pattern = "Fragilariforma")]
data2 <- data2[!str_detect(string = taxon, pattern = "Brebissonia lanceolata")]
data2 <- data2[!str_detect(string = taxon, pattern = "Brachysira vitrea")]
data2 <- data2[!str_detect(string = taxon, pattern = "Cyclotella iris")]
data2 <- data2[!str_detect(string = taxon, pattern = "Gomphonella calcarea")]
data2 <- data2[!str_detect(string = taxon, pattern = "Navigeia decussis")]
data2 <- data2[!str_detect(string = taxon, pattern = "Odontidium")]
data2 <- data2[!str_detect(string = taxon, pattern = "Frustula")]
data2 <- data2[!str_detect(string = taxon, pattern ="Amphipleura pellucida")]      
data2 <- data2[!str_detect(string = taxon, pattern ="Aneumastus tuscula"    )]     
data2 <- data2[!str_detect(string = taxon, pattern ="Aulacoseira granulata")]      
data2 <- data2[!str_detect(string = taxon, pattern ="Caloneis amphisbaena"  )]     
data2 <- data2[!str_detect(string = taxon, pattern ="Caloneis silicula"     )]    
data2 <- data2[!str_detect(string = taxon, pattern ="Cyclotella meneghiniana"   )] 
data2 <- data2[!str_detect(string = taxon, pattern ="Cymbopleura cuspidata"      )]
data2 <- data2[!str_detect(string = taxon, pattern ="Cymbopleura naviculiformis" )]
data2 <- data2[!str_detect(string = taxon, pattern ="Frustulia saxonica"         )]
data2 <- data2[!str_detect(string = taxon, pattern ="Frustulia vulgaris"        )]
data2 <- data2[!str_detect(string = taxon, pattern ="Halamphora veneta"          )]
data2 <- data2[!str_detect(string = taxon, pattern ="Hantzschia amphioxys"       )]
data2 <- data2[!str_detect(string = taxon, pattern ="Hippodonta capitata"        )]
data2 <- data2[!str_detect(string = taxon, pattern ="Lemnicola hungarica"        )]
data2 <- data2[!str_detect(string = taxon, pattern ="Luticola goeppertiana"     )]
data2 <- data2[!str_detect(string = taxon, pattern ="Luticola mutica"            )]
data2 <- data2[!str_detect(string = taxon, pattern ="Luticola nivalis"           )]
data2 <- data2[!str_detect(string = taxon, pattern ="Mayamaea atomus"            )]
data2 <- data2[!str_detect(string = taxon, pattern ="Nitzscha intermedia"        )]
data2 <- data2[!str_detect(string = taxon, pattern ="Orthoseira roeseana"       )]
data2 <- data2[!str_detect(string = taxon, pattern ="Placoneis elginensis"       )]
data2 <- data2[!str_detect(string = taxon, pattern ="Planothidium delicatulum"   )]
data2 <- data2[!str_detect(string = taxon, pattern ="Planothidium lanceolatum"   )]
data2 <- data2[!str_detect(string = taxon, pattern ="Plathelminthes"             )]
data2 <- data2[!str_detect(string = taxon, pattern ="Reimeria sinuata"          )]
data2 <- data2[!str_detect(string = taxon, pattern ="Rhoicosphenia abbreviata"   )]
data2 <- data2[!str_detect(string = taxon, pattern ="Rhopalodia gibba"           )]
data2 <- data2[!str_detect(string = taxon, pattern ="Staurosira construens"      )]
data2 <- data2[!str_detect(string = taxon, pattern ="Staurosirella pinnata"  )]

TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu] |> sort())


# taxonomy --------------------------------------------------------------------------

taxontable <- update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := .GRP, by = "date"]

#- add leading zeros 
data3[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data3[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_romania")]


data3[is.na(kingdom)]

data3[original_name == "Gasteropoda",    c("class", "phylum", "kingdom") := .("Gastropoda", "Mollusca", "Animalia")]
data3[original_name == "Nematoda",       c("phylum", "kingdom") := .("Nematoda", "Animalia")]
data3[original_name == "Plathelminthes", c("phylum", "kingdom") := .("Platyhelminthes", "Animalia")]
data3[original_name == "Polycelis",      c("genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .("Polycelis", "Planariidae", "Tricladida",
                                                                                                                     NA,"Rhabditophora", "Platyhelminthes", "Animalia")]
data3[original_name == "Dugesia",      c("genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .("Dugesia", "Dugesiidae", "Tricladida",
                                                                                                                      NA,"Rhabditophora", "Platyhelminthes", "Animalia")]
data3[original_name == "Stratiomidae",   c("family", "order", "subclass", "class", "phylum", "kingdom") := .("Stratiomydae", "Diptera", NA, "Insecta", "Arthropoda", "Animalia")]

data4 <- data3[, list(
        gr_sample_id,
        original_site_name = original_site_name,
        date,
        year,
        season,
        site_id,
        date_id,
        original_name,
        species,
        genus,
        family,
        order,
        subclass,
        class,
        phylum,
        kingdom,
        abundance,
        x.coord,
        y.coord,
        EPSG = 4326,
        data.set = "monitoring_romania"
)]

data4 <- data4[!is.na(x.coord)]

#- combine entries of same taxon 
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum,kingdom)))))))]

#- check 
data4[is.na(lowest.taxon)]

data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

saveRDS(data4, paste0("data/01_original_data/monitoring_romania/auxilliary/02_",Sys.Date(),"_data_before_taxonomy.rds"))
data4 <- readRDS("data/01_original_data/monitoring_romania/auxilliary/02_2021-09-22_data_before_taxonomy.rds")
# TYPOLOGIES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_romania/",Sys.Date(),"_final_non_aggregated.rds"))
# TEMPORAL AGGREGATION --------------------------------------------------------------

source("R/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/monitoring_romania/",Sys.Date(),"_final_aggregated.rds"))

# SUMMARY STATISTICS ----------------------------------------------------------------
data6[,uniqueN(gr_sample_id)]    
data6[distance <= 500,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE ,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE & distance <= 500,uniqueN(gr_sample_id)]    
