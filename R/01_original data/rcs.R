# ——————————————————————————————————— #
# ——— Clean data from RCS Network ——— # 
# ——————————————————————————————————— #

# ———————————————————————————————————
# date created: 16.07.21
# date last modified: 23-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data 
# provided by Phillipe Usseglio Polatera. 
# Temporal aggregation: No 
# ————————————————

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

# LOAD DATA  ------------------------------------------------------------------------

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxatable.rds")

data       <- read_excel("data/01_original_data/rcs/raw_data/data_invertebrate_GETREAL project-final version.xlsx", skip = 4)
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")


# PREPARE DATA  ---------------------------------------------------------------------

setDT(data)

# ——— SAMPLES 1 ——— #

data[, date := ymd(`Date (D/M/Y)`)]
data[, `:=` 
     (year = year(date), 
       season = ifelse(month(date) %in% c(12,1,2), "winter", 
                       ifelse(month(date) %in% c(3,4,5), "spring", 
                              ifelse(month(date) %in% c(6,7,8), "summer", "autumn")
                       )
       )
     )
]

samples <- data[,.SD, .SDcols = c(1,33:381)]
site    <- data[,.SD, .SDcols = c(1,5,6,382:384)]

samples2 <- melt(samples, 
                id.vars = "List No", 
                variable.name = "taxon")

samples2 = samples2[value != 0]

data2 = samples2[site, on = "List No"]
data2$taxon = as.character(data2$taxon)
data2[,value := NULL]

data2[, c("data.set", "EPSG",  "abundance") := .("phillipe_ussseglio-polatera", 27572, NA)]

data2 %<>% rename(x.coord = "Coordinate (xl2)", 
                  y.coord = "Coordinate (yl2)",
                  original_site_name = "List No")

data2[,taxon := str_remove(taxon, "\\.\\.\\.[0-9]+")]
data2[,taxon := str_replace(taxon, "\\_", "\\ ")]     
data2[taxon %in% c("Triaenodes/Ylodes"), taxon := "Leptoceridae"]

#- clean up 
rm(data, samples, samples2, site)
gc()

TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

data2 <- data2[taxon != "Nemathelmintha"]

# TAXONOMY --------------------------------------------------------------------------
taxontable <- update_taxonomy(TU)

# prepare data 4 --------------------------------------------------------------------
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
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_rcs")]

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
        data.set = "rcs"
        
)]

data4 <- data4[!is.na(x.coord)]

#- combine entries of same taxon 
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]

data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))



#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/rcs/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))

# TYPOLOGIES -----------------------------------------------------------------------------
data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/rcs/",Sys.Date(),"_final_non_aggregated.rds")) 

# TEMPORAL AGGREGATION --------------------------------------------------------------

#- no temporal aggregation necessary. 
#- minimal richnesses are ok. 

# SUMMARY STATISTIC -----------------------------------------------------------------
summary(data5$year)
sites |> nrow()
sites |> filter (brt_distance <= 500) |> nrow()
sites |> filter (fec.least.impacted) |> nrow()
sites |> filter (fec.least.impacted) |> filter (brt_distance <= 500) |> nrow()

data5[, uniqueN(gr_sample_id)]

