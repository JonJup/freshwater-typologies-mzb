# ——————————————————————————————— #
# ——— Clean AQEM ROMANIA data ——— # 
# ——————————————————————————————— #

# ———————————————————————————————————
# date created:  02-08-21
# date last modified:  05-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the AQEM data data from Romania. 
# Temporal aggregation: Yes 
# ————————————————

# SETUP  -----------------------------------------------------------------------------
devtools::install_github("https://github.com/JonJup/jjmisc")
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
        fs,
        geodist
)

# LOAD DATA  ------------------------------------------------------------------------
most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", "taxontable.rds")
data       <- read_excel("data/01_original_data/aqem_romania/raw_data/RO_Species+AQEM_ID.xlsx") 
sites      <- read_excel("data/01_original_data/aqem_romania/raw_data/RO_RiversSections.xlsx") 
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")
rm(most_recent_date)
# PREPARE DATA   ---------------------------------------------------------------------
setDT(data)

#- rename abundance data
names(data)[13] <- "iz"


data2 <- data.table()
data2[, original_site_name := data$`Sampling site code`]
data2[, taxon := data$`Species name`]
data2[, date := mdy_hms(data$`Sampling date`)]
data2[, year := year(date)]
data2[, month := month(date)]
data2[, abundance := data$iz]
data2[, data.set := "aqem_romania"]


sites2 <- data.table()
sites2[, original_site_name := sites$`Sampling site code`]
sites2[, x.coord := sites$Longitude]
sites2[, y.coord := sites$Latitude]

test <- unique(sites2, by = "original_site_name")
test  |>  st_as_sf(coords = c("x.coord", "y.coord"), crs = 4326)
mapview(test)

data2 <- sites2[data2, on = "original_site_name"]

anyNA(data2$x.coord)
anyNA(data2$y.coord)

summary(data2$year)

data2$season <- case_when(
        data2$month %in% c(3, 4, 5)   ~ "spring",
        data2$month %in% c(12, 1, 2)  ~ "winter",
        data2$month %in% c(6, 7, 8)   ~ "summer",
        data2$month %in% c(9, 10, 11) ~ "autumn"
)


TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

# TAXONOMY --------------------------------------------------------------------------

taxontable <- jjmisc::update_taxonomy(TU)

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_aqem_romania")]



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
        EPSG = 4326,
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

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/aqem_romania/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))
data4 <- readRDS("data/01_original_data/aqem_romania/auxilliary/01_2021-10-05_data_before_typologies.rds")
# TYPOLOGIES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)

#- save to or load from file 
saveRDS(data5, paste0("data/01_original_data/aqem_romania/", Sys.Date(), "_final_non_aggregated.rds"))
data5 <- readRDS("data/01_original_data/aqem_romania/2021-10-05_final_non_aggregated.rds")
# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/aqem_romania/", Sys.Date(), "_final_aggregated.rds"))

# STATISTICS ------------------------------------------------------------------------
uniqueN(data5$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)
sites |> filter(distance <= 500) |> count()
data5 |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()
sites |> filter(least.impacted) |> filter(distance <= 500) |> count()
data5 |> filter(least.impacted) |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()
# - aggregated 
uniqueN(data6$gr_sample_id)
data6 |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data6 |> filter(least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data6 |> filter(least.impacted) |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()



