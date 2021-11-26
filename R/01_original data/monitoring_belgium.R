# ---------------------------------------------------- #
# -------- Clean Belgium Monitoring data ------------- # 
# ---------------------------------------------------- #


# --------------------------------------------------------------------------------------------------------
# date created: 03.09.21
# date last modified: 05-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the Belgium monitoring data 
# # Temporal aggregation: Yes 
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


# load data -------------------------------------------------------------------------

taxa  <- fread("data/01_original_data/monitoring_belgium/raw/occurrence.txt") 
sites <- fread("data/01_original_data/monitoring_belgium/raw/event.txt") 
most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data/", 
                                             file   = "taxontable.rds")
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------
names(taxa)
names(sites)
names(sites)[1] <- "eventID"
all(taxa$eventID %in% sites$eventID)
data <- sites[taxa, on = "eventID"]
names(data)
sort(table(data$samplingProtocol))
#- keep only handnet (=kicknet) samples 
data <- data[samplingProtocol == "Handnet"]

sort(unique(data$habitat))

data <- data[habitat %in% c("RtNt:River;Water body", "BkK:Small stream Kempen;Water body", "Bk:Small stream;Water body", "Bg:Large stream;Water body", "BgK:Large stream Kempen;Water body",
                            "Rk:Small river;Water body", "Rg:Large river;Water body", "No type:ditch;Water body", "Rzg:Very large river;Water body")]

data2 <- data.table(
        original_site_name = data$locationID,
        date               = ymd(data$eventDate),
        taxon              = data$scientificName,
        x.coord = data$verbatimLongitude,
        y.coord  = data$verbatimLatitude,
        EPSG = 31370,
        data.set = "monitoring_belgium",
        abundance = 1,
        countryCode = data$countryCode
)

data2 |> 
        unique(by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) -> 
        sites 
mapview(sites)


data2[, `:=` 
     (year = year(date), 
             season = ifelse(month(date) %in% c(12,1,2), "winter", 
                             ifelse(month(date) %in% c(3,4,5), "spring", 
                                    ifelse(month(date) %in% c(6,7,8), "summer", "autumn")
                             )
             )
     )
]
#- check that each site only has set of coordinates 
all(data2[,uniqueN(x.coord), by = "original_site_name"]$V1 == 1)
all(data2[,uniqueN(y.coord), by = "original_site_name"]$V1 == 1)


TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

taxontable = update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_belgium")]



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

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/monitoring_belgium/auxilliary/02_",Sys.Date(),"_data_before_typology.rds"))
data4 <- readRDS("data/01_original_data/monitoring_belgium/auxilliary/02_2021-09-22_data_before_typology.rds")

# SITES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_belgium/", Sys.Date(),"_final_non_aggregated.rds"))
data5 <- readRDS("data/01_original_data/monitoring_belgium/2021-10-05_final_non_aggregated.rds")

data5 |> 
        distinct_at("site_id", .keep_all = TRUE) |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1]) |> 
        mapview(zcol = "least.impacted")


# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data5 <- data5[year >=2004]
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/monitoring_belgium/",Sys.Date(),"_final_aggregated.rds")) 

# SUMMARY STATISTICS ------------------------------------------------------------------------
uniqueN(sites6$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)

# samples 
# one factor 
data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(year >= 2004)  |> pull(gr_sample_id) |> uniqueN()
# two factor 
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(year >= 2004) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(year >= 2004) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
# three factors
data5 |> filter(year > 2004) |> filter(brt_distance <= 500) |> filter(fec.least.impacted)|> pull(gr_sample_id) |> uniqueN()

sites6 |> filter(brt_distance <= 500) |> count()
sites6 |> filter(fec.least.impacted) |> count()
data5  |> filter(year >=2004) |> pull(site_id) |> uniqueN()
sites6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
data5  |> filter(year >=2004 & brt_distance <= 500) |> pull(site_id) |> uniqueN()
data5  |> filter(year >=2004 & brt_distance <= 500 & fec.least.impacted) |> pull(site_id) |> uniqueN()   

#- aggregated 
data6[, uniqueN(gr_sample_id)]
data6[distance <= 500, uniqueN(gr_sample_id)]
data6[least.impacted == TRUE, uniqueN(gr_sample_id)]
data6[fec.least.impacted == TRUE & brt_distance <= 500, uniqueN(gr_sample_id)]

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data6, paste0("data/original data/monitoring_belgium/",Sys.Date(),"_final_aggregated.rds")) 
