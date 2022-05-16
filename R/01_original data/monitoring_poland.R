# ————————————————————————————————————————— #
# ——— Clean monitoring data from Poland ——— # 
# ————————————————————————————————————————— #


# ———————————————————————————————————
# date created:  02-08-21
# date last modified: 05-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data provided by Poland 
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
        stringr,
        tidyr
)

#- functions 
#- functions 
# LOAD DATA  ------------------------------------------------------------------------
taxontable <- readRDS("data/01_original_data/2021-12-10_taxontable.rds")
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA   ---------------------------------------------------------------------

#- have to source directly from the script. Open with F2
source("R/01_original data/monitoring_poland_collect_data.R")

# TAXONOMY --------------------------------------------------------------------------

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_poland")]

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
        data.set = "monitoring_poland"
        
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
data4 <- data4[abundance != "0"]
data4 <- data4[,abundance := 1]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- same name but separate sites (caught in temporal aggregation)
data4[original_site_name == "PL01S1601_3670" & date == "2016-06-21", c("original_site_name", "site_id") := .("PL01S1601_36702", "01924")]
data4[gr_sample_id == "site_00243_date_00268_monitoring_poland",     c("original_site_name", "site_id") := .("PL01S1501_17492", "01925")]
data4[gr_sample_id == "site_00244_date_00056_monitoring_poland",     c("original_site_name", "site_id") := .("PL01S1501_17652", "01926")]


#- same name and same location but one coordinate combination deviates slightly. 
data4[original_site_name == "PL01S0701_1218" & date == "2016-09-13", c("x.coord", "y.coord") := .(
        unique(data4[original_site_name == "PL01S0701_1218" & date == "2019-09-09", x.coord]),
        unique(data4[original_site_name == "PL01S0701_1218" & date == "2019-09-09", y.coord])
)]

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/monitoring_poland/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))

# SITES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_poland/", Sys.Date(), "_final_non_aggregated.rds"))
data5 <- readRDS("data/01_original_data/monitoring_poland/2021-09-22_final_non_aggregated.rds")

# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/monitoring_poland/",Sys.Date(),"_final_aggregated.rds"))

# SUMMARY STATISTICS ----------------------------------------------------------------

#- aggregated 
data6[, uniqueN(gr_sample_id)]
data6[distance <= 500, uniqueN(gr_sample_id)]
data6[least.impacted == TRUE, uniqueN(gr_sample_id)]
data6[distance <= 500 & least.impacted == TRUE , uniqueN(gr_sample_id)]

