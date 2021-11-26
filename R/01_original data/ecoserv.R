# —————————————————————————— #
# ——— Clean EcoSurv data ——— # 
# —————————————————————————— #


# ———————————————————————————————————
# date created: 14-07-21
# date last modified: 22-0-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data provided from EcoSurv by Denes Schmera. 
# Temporal aggregation: No
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
        stringr
)

#- functions 
source("R/fill_taxon_table.R")
source("R/get_last_date.r")
source("R/temoral_aggregation_function.R")
source("R/add_typologies.R")
source("R/update_taxonomy.R")

# LOAD DATA  ------------------------------------------------------------------------
data       <- read_excel("data/original data/ecosurv/raw data/Hungary-data2.xlsx") 
taxontable <- readRDS(paste0("data/original data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")


# PREPARE DATA   ---------------------------------------------------------------------
setDT(data)

#- information on year and season is taken from  Schmera & Bauer (2011): Testing a
#- typology system of running waters for conservation planning in Hungary

data = data[, 
            list(   original_site_name = SiteID,
                    year =  2005,
                    season = "spring",
                    x.coord = EOVY,
                    y.coord = EOVX,
                    taxon = TAXON,
                    EPSG = 23700,
                    date = as.Date(NA),
                    data.set = "ecosurv"
            )
]

TU <- sort(unique(data$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

# TAXONOMY --------------------------------------------------------------------------

taxontable <- update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------

names(data)[which(names(data) == "taxon")] <- "original_name"
data3 <- taxontable[data, on = "original_name"]

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := 00001]

#- add leading zeros 
data3[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_ecosurv")]

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
        abundance = NA,
        x.coord,
        y.coord,
        EPSG,
        data.set = "ecosurv"
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

data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/ecosurv/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))

# SITES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)

saveRDS(data5, paste0("data/original data/ecosurv/",Sys.Date(),"_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)

#- Aggregation not necessary 

# Statistics ------------------------------------------------------------------------
uniqueN(sites2$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)

data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()

sites2 |> filter(brt_distance <= 500) |> count()
sites2 |> filter(fec.least.impacted) |> count()
sites2 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
