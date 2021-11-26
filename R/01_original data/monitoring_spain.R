# ---------------------------------------------------- #
# -------- Clean Spanish Monitoring data ------------- # 
# ---------------------------------------------------- #


# --------------------------------------------------------------------------------------------------------
# date created:  17.08.21
# date last modified: 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the Spain monitoring
# data provided by Nuria Bonada.  
# Note: In her Email from the 17.08.12, Nuria writes that, for the data on all of Spain, we
# should better use the later years to ensure consistency with the national monitoring standards.  
# For the data on Catalonia, after 2013 is best. 
# Temporal aggregation: yes 
# --------------------------------------------------------------------------------------------------------

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
        stringr,
        tidyr
)

#- functions 


# LOAD DATA  ------------------------------------------------------------------------

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxatable.rds")

dt.ctln1.1 <- read_excel("data/01_original_data/monitoring_spain/raw_data/Catalan data.xlsx", sheet = 1) 
dt.ctln2.1 <- read_excel("data/01_original_data/monitoring_spain/raw_data/Catalan data.xlsx", sheet = 2) 
dt.as.1 <- read_excel("data/01_original_data/monitoring_spain/raw_data/Data long term Spain.xlsx", sheet = 1)

taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA   ---------------------------------------------------------------------

# Nuria wrote: "For the Spanish data, all should be data collected with the official
# sampling method but better to use the latter years" and "For the Catalan data, the
# period 2013-2018 was also sampled using the official method".

# —————— Data Catalan
#- transform to data.table 
setDT(dt.ctln1.1)
setDT(dt.ctln2.1)


#- all site names refer to more than one x.coordiante value hence I create a new unique identifier 
dt.ctln1.2 = data.table(
        taxon              = dt.ctln1.1$Taxa,
        abundance          = dt.ctln1.1$`Abundancia (ind/mostra, fins un màxim de 100)`,
        date               = ymd(dt.ctln1.1$DataRealització),
        x.coord            = dt.ctln1.1$UTMX,
        y.coord            = dt.ctln1.1$UTMY
                       )

dt.ctln2.2 = data.table(
        taxon              = dt.ctln2.1$Tbl_MacInv_Dic.Nombre,
        abundance          = dt.ctln2.1$Cantidad,
        date               = ymd(dt.ctln2.1$Fecha),
        x.coord            = dt.ctln2.1$UTMX,
        y.coord            = dt.ctln2.1$UTMY
                       )

dt.ctln <- rbindlist(list(dt.ctln1.2, dt.ctln2.2))
dt.ctln[, original_site_name := .GRP, by = c("x.coord", "y.coord")]

#- check that each site has only one coordinate 
all(dt.ctln[,uniqueN(x.coord), by = "original_site_name"]$V1 == 1)

#- assign year and EPSG code
dt.ctln[, year := year(date)]
dt.ctln[, EPSG := 25830]
 
summary(dt.ctln$year)

#- drop observations before 2013 or without date 
dt.ctln <- dt.ctln[!is.na(year) & year > 2012]

#- check sites on a map 
sites <-
        dt.ctln |>
        unique(by = "original_site_name") |>
        st_as_sf(coords = c("x.coord", "y.coord"), crs = dt.ctln$EPSG[1])

mapview(sites)

#- transform to WGS 84
dt.ctln %<>% 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = dt.ctln$EPSG[1]) %>%
        st_transform(crs = 4326)
#- add cat_ prefix to site name
dt.ctln %<>% mutate(original_site_name = paste0("cat_", original_site_name))

# —————— Monitoring all of Spain 
dt.as.2 <- copy(dt.as.1)

names(dt.as.2)%<>% str_remove_all("-")
names(dt.as.2)%<>% str_trim()

for (i in seq_along(names(dt.as.2))){
        if (names(dt.as.2)[i] %in% c("Site", "EU_water body", "surfaceWaterBodyName", "longitude", "latitude", "Sampling date")) next()
        x <- names(dt.as.2)[i]
        x1 <- word(x, -1)
        if (
                substring(x1, 1,1) == str_to_upper(substring(x1,1,1))
        ) {
                x2 <- x1
        } else {
                x1b <- word(x, -5)
                x2 <- paste(x1b, x1)
        }
        names(dt.as.2)[i] <- x2        
}
#- fix problems 
names(dt.as.2)[which(names(dt.as.2) == " caurelensis")]  <- "Philopotamus montanus"
names(dt.as.2)[which(names(dt.as.2) ==  " lusitanicus")] <- "Austropotamobius pallipes"
names(dt.as.2)[which(names(dt.as.2) ==  " saturniae")]   <- "Silo mediterraneus"
#- drop emtpy taxon column
dt.as.2 <- dt.as.2[,-which(names(dt.as.2) == "")]
#- reshape into long format 
dt.as.3 <- pivot_longer(data = dt.as.2, 
                        cols = Dytiscidae:Hydrochus,
                        names_to = "taxon", 
                        values_to = "abundance")
#- reshape into common format 
dt.as.4 <- data.table(
        taxon              = dt.as.3$taxon,
        abundance          = dt.as.3$abundance,
        x.coord            = dt.as.3$longitude,
        y.coord            = dt.as.3$latitude,
        EPSG               = 4326,
        date               = ymd(dt.as.3$`Sampling date`)
        )
dt.as.4[, original_site_name := .GRP, by = c("x.coord", "y.coord")]
dt.as.4[, original_site_name := paste0("as_",original_site_name)]
dt.as.4 <- dt.as.4[abundance != 0]
dt.as.4[, year := year(date)]
dt.as.4 %<>% st_as_sf(coords = c("x.coord", "y.coord"))

#- remove early years 
summary(dt.as.4$year)
dt.as.4 %<>% filter(year > 2012)

data <- bind_rows(dt.ctln, dt.as.4)

setDT(data)

data[, season := ifelse(month(date) %in% c(12,1,2), "winter", ifelse(month(date) %in% c(3,4,5), "spring", ifelse(month(date) %in% c(6,7,8), "summer", ifelse(month(date) %in% c(9,10,11), "autumn", "what?"))))]
any(data$season == "what?")
#- some entries (not many) have no date 
data <- data[season != "what?"]

data[taxon == "Cambaridae (P. clarkii)", taxon := "Procambarus clarkii"]
data[taxon == "Cambaridae (P. clarkii)", taxon := "Procambarus clarkii"]
data[taxon == "Cambaridae (Procambarus clarkii)", taxon := "Procambarus clarkii"]
data[taxon == "Cambaridae al·lòcton", taxon := "Cambaridae"]
data[taxon == "Coleopter sp1", taxon := "Coleopter"]
data[taxon == "Coleopter sp2", taxon := "Coleopter"]
data[taxon == "Corbiculidae (Corbicula fluminea)", taxon := "Corbicula fluminea"]
data[taxon == "Corbiculidae al·lòcton", taxon := "Corbiculidae"]
data[taxon == "Dípter no identificat", taxon := "Diptera"]
data[taxon %in% c("Hydrobiidae (P. antipodarum)", "Hydrobiidae (Potamopyrgus antipodarum)"), taxon := "Potamopygrus antipodarum"]
data <- data[taxon != "Sphaerotillus"]   
TU <- sort(unique(data$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

# TAXONOMY --------------------------------------------------------------------------

taxontable <- update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------
data2 <- data
setDT(data2)
names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := .GRP, by = c("date")]

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_spain")]



data4 <- data3[, list(
        gr_sample_id,
        original_site_name,
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
        x.coord = st_coordinates(st_as_sf(data3))[,1],
        y.coord = st_coordinates(st_as_sf(data3))[,2],
        EPSG = 4326,
        data.set = "Monitorig data from Spain"
)]

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

data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/monitoring_spain/auxilliary/01_",Sys.Date(),"_data_before:typologies.rds"))

# SITES -----------------------------------------------------------------------------
data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_spain/",Sys.Date(),"_final_non_aggregated.rds"))
data5 <- readRDS("data/01_original_data/monitoring_spain/2021-09-22_final_non_aggregated.rds")
# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/monitoring_spain/",Sys.Date(),"_final_aggregated.rds"))       

# SUMMARY STATISTICS ------------------------------------------------------------------------
data6[,uniqueN(gr_sample_id)]    
data6[distance <= 500,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE ,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE & distance <= 500,uniqueN(gr_sample_id)] 


