# ———————————————————————————— #
# ——— Clean Koutajoki data ——— # 
# ———————————————————————————— #

# ———————————————————————————————————
# date created:  14-07-21
# date last modified: 05-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data provided from Koutajoki by Kaisa-Leena Huttunen. 
# Temporal aggregation: Yes 
# ———————————————————————————————————

# SETUP -----------------------------------------------------------------------------

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
        stringr
)

# source("R/fill_taxon_table.R")
# source("R/get_last_date.r")
# source("R/temoral_aggregation_function.R")
# source("R/add_typologies.R")
# source("R/update_taxonomy.R")

# LOAD DATA  ------------------------------------------------------------------------

samples    <- read_excel("data/01_original_data/koutajoki/raw_data/MI_data_KLHuttunen.xlsx") 
sites      <- read_excel("data/01_original_data/koutajoki/raw_data/MI_data_KLHuttunen.xlsx", sheet = 2) 
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA  ---------------------------------------------------------------------
setDT(samples)
setDT(sites)

samples <- melt(samples, id.vars = c("Site", "Year"),
             value.name = "abundance", 
             variable.name = "taxon")

samples <-  samples[abundance != 0]

#- This is stated in Huttunen et al 2017: Habitat connectivity and in-stream vegetation control temporal
#- variability of benthic invertebrate communities
samples$season = "autumn"
# In the paper it is also stated that all sites are pristine 

data <- samples[sites, on = "Site"]

data2 <- data[, list(
        original_site_name = Site,
        date = as.Date(NA),
        year = Year,
        season,
        taxon = as.character(taxon),
        abundance,
        x.coord = EUREF_Lon_E,
        y.coord = EUREF_Lat_N,
        EPSG = 4258,
        data.set = "kaisa-leena_huttunen"
)]

#- taxonomy 
data2[, taxon := str_remove_all(taxon, "\\_group$")]
data2[, taxon := str_remove_all(taxon, "\\_other$")]
data2[, taxon := str_remove_all(taxon, "\\ sp\\.$")]
data2[taxon == "Baetis niger/digitatus", taxon := "Baetis"]
data2[taxon == "Hemerodromia/Wiedemannia", taxon := "Empididae"]
data2[taxon == "Leuctra digi/fusc/hipp", taxon := "Leuctra"]
data2[taxon == "Pericomini/Telmatoscopini", taxon := "Psychodidae"]
data2[taxon == "Rhyacophila obliterata/fasciata", taxon := "Rhyacophila"]

TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])


# TAXONOMY --------------------------------------------------------------------------

taxontale <- update_taxonomy(TU)

# COMBINE DATA AND SITES ------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

#- check taxa 
unique(data3$kingdom)
unique(data3$phylum)
unique(data3$class)
unique(data3$subclass)
unique(data3$order)

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := .GRP, by = "year"]

#- add leading zeros 
data3[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
#- add leading zeros 
data3[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_Koutajohi")]

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
        x.coord,
        y.coord,
        EPSG,
        data.set
        
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

saveRDS(data4, paste0("data/01_original_data/koutajoki/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))
data4 <- readRDS("data/01_original_data/koutajoki/auxilliary/01_2021-09-22_data_before_typologies.rds")
# SITES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/koutajoki/",Sys.Date(),"_final_non_aggregated.rds"))
data5 <- readRDS("data/01_original_data/koutajoki/2021-09-22_final_non_aggregated.rds")

# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data6 <- data5[year == 2013]
saveRDS(data6, paste0("data/01_original_data/koutajoki/",Sys.Date(),"_final_aggregated.rds"))

# Statistics ------------------------------------------------------------------------
uniqueN(sites6$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)
sites6 |> filter(brt_distance <= 500) |> count()
data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
sites6 |> filter(fec.least.impacted) |> count()
sites6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()

# aggregated 
uniqueN(data6$gr_sample_id)
data6 |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data6 |> filter(least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data6 |> filter(least.impacted & distance <= 500) |> pull(gr_sample_id) |> uniqueN()

        # SAVE TO FILE  ---------------------------------------------------------------------
saveRDS(data6, paste0("data/01_original_data/koutajoki/",Sys.Date(),"_final_aggregated.rds"))
