# ——————————————————————————————— #
# ——— Clean Pepe Barquin data ——— # 
# ——————————————————————————————— #


# ———————————————————————————————————
# date created: 15.07.21
# date : 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data provided by Pepe Barquin. 
# Temporal aggregation: yes 
# ————————————————

# setup -----------------------------------------------------------------------------

pacman::p_load(
        biotic,
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

# load data  ------------------------------------------------------------------------

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxatable.rds")

data.cant  <- read_excel("data/01_original_data/pepe_barquin/raw_data/Cantabria_55sites_2005.xls", skip = 2) 
spat.cant  <- st_read   ("data/01_original_data/pepe_barquin/raw_data/Cantabria55.shp")
data.pico  <- read_excel("data/01_original_data/pepe_barquin/raw_data/Picos_13Sites_2015_2017.xlsx", skip = 2) 
spat.pico  <- st_read   ("data/01_original_data/pepe_barquin/raw_data/Picos13_sites.shp")

taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# prepare data  ---------------------------------------------------------------------

#- transform to data table 
setDT(data.cant)
setDT(data.pico)

#- I will first prepare the data from Cantabria and then the samples from the Picos de Europa

# ————————————————— # 
# ——— Cantabria ——— # 
# ————————————————— #

# there are nine empty rows at the end which need to be removed  
data.cant  <- data.cant[!is.na(Site)]
data.cant <- data.cant[, -(175:209)]
#- some abundance values are characters
data.cant <- as.data.frame(data.cant)
for (i in 3:ncol(data.cant)){
        if (class(pull(data.cant,i)) == "character"){
                data.cant[,i] <- as.numeric(data.cant[,i])
                }
}
#- to long format 
data.cant2 <-
        data.cant |>
        pivot_longer(cols = 3:ncol(data.cant)) |> 
        filter(value != 0) |> 
        rename(taxon = name) |> 
        rename(abundance = value) |> 
        mutate(year   = str_split(Date, pattern = "\\ ", simplify = T)[,2]) |> 
        mutate(season = str_split(Date, pattern = "\\ ", simplify = T)[,1]) |> 
        select(!Date) |> 
        mutate(EPSG = 23030, 
               data.set = "cantabria")
# adapt site-id to spatial data 
SiteString2 <- c()
for (i in 1:length(data.cant2$Site)) {
        
        a = data.cant2$Site[i]
        if (is.na(a)) {SiteString2[i] = NA } else {
                
                
                front = str_split(a, regex("[0-9]"))[[1]][1]
                back = str_split(a, regex("[A-Z]"))[[1]][3]
                
                new = paste0(front,0,back)
                SiteString2[i] = new
        }
        rm(i,a,front, back,new)
        
}

data.cant2$Site = SiteString2

data.cant2 <- 
        data.cant2 |> 
        left_join(select(spat.cant, c("ESTACION", "X_COORD", "Y_COORD")),  
                  by = c("Site" = "ESTACION")) |> 
        rename(original_site_name = Site,
               x.coord = X_COORD, 
               y.coord = Y_COORD) |> 
        select(!geometry) |> 
        setDT()

# ——————————————————————— # 
# ——— Picos de Europa ——— # 
# ——————————————————————— #

#- This data set contains two samples per site. 
#- One from a pool and one from a run.
#- I aggregate them.

# head(data.pico)
# View(data.pico)

#- convert to long format
data.pico2 <- 
        data.pico |>
        pivot_longer(cols = 6:ncol(data.pico)) |>
        select(!c("Habitat", "Codigo")) |> 
        filter(value != 0) |> 
        rename(taxon = name,
               abundance = value,
               site = Río,
               year = Año,
               date = FechaMuestra
               ) |> 
        mutate(EPSG = 23030, 
               data.set = "picos_de_europa",
               date = ymd(date),
               season = if_else(month(date) %in% c(1,2,12),  "winter",
                                if_else(month(date) %in% c(3,4,5),   "spring",
                                        if_else(month(date) %in% c(6,7,8),   "summer",
                                                if_else(month(date) %in% c(9,10,11), "autumn", "NA"))))
               )

# clean site names to join with spatial 
data.pico2$site[which(data.pico2$site == "ARENAL")] = "Río Arenal"
data.pico2$site[which(data.pico2$site == "BULNES")] = "Bulnes"
data.pico2$site[which(data.pico2$site == "CARES-VALDEÓN")] = "Cares Valdeón"
data.pico2$site[which(data.pico2$site == "DUJE")] = "Duje"
data.pico2$site[which(data.pico2$site == "CASAÑO")] = "Casaño"
data.pico2$site[which(data.pico2$site == "FARFADA")] = "Manantial Farfada"
data.pico2$site[which(data.pico2$site == "FUENTE DÉ")] = "Fuente Dé"
data.pico2$site[which(data.pico2$site == "MANANTIAL FARFADA")] = "Manantial Farfada"
data.pico2$site[which(data.pico2$site == "MANANTIAL PONGA")] = "Manantial Ponga"
data.pico2$site[which(data.pico2$site == "RÍO PONGA")] = "Ponga"
data.pico2$site[which(data.pico2$site == "SALVORÓN")] = "Salvorón"
data.pico2$site[which(data.pico2$site == "SECO")] = "Seco"
data.pico2$site[which(data.pico2$site == "SELLA")] = "Sella"
data.pico2$site[which(data.pico2$site == "TIELVE")] = "Duje Tielve"
data.pico2$site[which(data.pico2$site == "TIELVE-DUJE")] = "Duje Tielve"
data.pico2$site[which(data.pico2$site == "VALDEÓN")] = "Cares Valdeón" 

spat.pico$site <- as.character(spat.pico$Nom_rio)

data.pico2 <-
        data.pico2 |>
        left_join(select(spat.pico, c("site", "X_Coord", "Y_Coord"))) |>
        select(!geometry) |>
        filter(!taxon %in% c("...157", "...158", "...159", "NA")) |>
        rename(x.coord = X_Coord,
               y.coord = Y_Coord,
               original_site_name = site) |>
        setDT()

data.pico2[, taxon := str_remove_all(taxon, "sp\\.$")]
data.cant2[, taxon := str_remove_all(taxon, "sp\\.$")]
data.pico2[taxon == "Brachycentrus B. montanus", taxon := "Brachycentrus montanus"]

TU <- sort(unique(append(data.pico2$taxon, data.cant2$taxon)))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

# taxonomy --------------------------------------------------------------------------

taxontable <- update_taxonomy(TU)

# COMBINE DATA AND TAXONOMY ---------------------------------------------------------

names(data.cant2)[which(names(data.cant2) == "taxon")] <- "original_name"
names(data.pico2)[which(names(data.pico2) == "taxon")] <- "original_name"

data2 <- rbindlist(list(data.cant2, data.pico2), fill = T)

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_pepe_barquin")]

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
        x.coord = x.coord,
        y.coord = y.coord,
        EPSG,
        data.set
        
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


data4[is.na(lowest.taxon)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/pepe_barquin/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))
data4 <- readRDS("data/01_original_data/pepe_barquin/auxilliary/01_2021-09-22_data_before_typologies.rds")
# Typology -----------------------------------------------------------------------------
data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/pepe_barquin/",Sys.Date(),"_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/pepe_barquin/",Sys.Date(),"_final_aggregated.rds"))


# STATISTICS ------------------------------------------------------------------------
data6[,uniqueN(gr_sample_id)]    
data6[distance <= 500,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE ,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE & distance <= 500,uniqueN(gr_sample_id)] 

