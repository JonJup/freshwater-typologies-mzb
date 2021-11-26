# ----------------------------------------------- #
# -------- Clean UK Monitoring data ------------- # 
# ----------------------------------------------- #


# --------------------------------------------------------------------------------------------------------
# date created: 03.09.21
# date last modified: 22-09-21
# Project:Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the UK monitoring 
# data provided by https://environment.data.gov.uk/ecology/explorer/
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


# load data -------------------------------------------------------------------------

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxatable.rds")

taxa  <- fread("data/01_original_data/monitoring_uk/raw/INV_OPEN_DATA_TAXA_2021-09-03.csv") 
sites <- fread("data/01_original_data/monitoring_uk/raw/INV_OPEN_DATA_SITE_2021-09-03.csv") 

taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------

head(taxa)
unique(taxa$TAXON_TYPE)

taxa <- taxa[TAXON_TYPE == "Other Macroinvertebrates"]

unique(taxa$SAMPLE_METHOD_DESCRIPTION)
table(taxa$SAMPLE_METHOD_DESCRIPTION)

taxa <- taxa[SAMPLE_METHOD_DESCRIPTION == "3-MIN POND NET (BT001): 3-min active sampling, 1-min hand search as per BT001"]

taxa2 <- taxa[,c("SITE_ID", "SAMPLE_DATE", "TAXON_NAME", "TOTAL_ABUNDANCE")]
names(taxa2) <- c("original_site_name", "date", "taxon", "abundance")
taxa2[, date := ymd(date)]

head(sites)
all(taxa2$original_site_name %in% sites$SITE_ID)
sites <- sites[SITE_ID %in% taxa2$original_site_name]
sites <- sites[,c("SITE_ID", "FULL_EASTING", "FULL_NORTHING")]
sites_test <- st_as_sf(sites, coords = c("FULL_EASTING", "FULL_NORTHING"), crs = 27700)
mapview(sites_test)
names(sites) <- c("original_site_name", "x.coord", "y.coord")
data <- sites[taxa2, on = "original_site_name"]

data[, EPSG := 27700]
data[, data.set := "monitoring_uk"]
data[, `:=` 
     (year = year(date), 
             season = ifelse(month(date) %in% c(12,1,2), "winter", 
                             ifelse(month(date) %in% c(3,4,5), "spring", 
                                    ifelse(month(date) %in% c(6,7,8), "summer", "autumn")
                             )
             )
     )
]
#- check that each site only has set of coordinates 
all(data[,uniqueN(x.coord), by = "original_site_name"]$V1 == 1)
all(data[,uniqueN(y.coord), by = "original_site_name"]$V1 == 1)


TU <- sort(unique(data$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

#- clean up taxon names 
#- remove all brackets 
data[str_detect(taxon, "\\("), taxon := str_remove(taxon, "\\(.*\\)")]
data[, taxon := str_remove(taxon, "Larval\\ Species\\ C")]
data[str_detect(taxon, "\\/"),  taxon := word(taxon, 1)]
data[, taxon := str_trim(taxon)]
data <- data[taxon != "Nemathelmintha"]

# taxonomy --------------------------------------------------------------------------
taxontable <- update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------

names(data)[which(names(data) == "taxon")] <- "original_name"
data3 <- taxontable[data, on = "original_name"]

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_uk")]



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
saveRDS(data4, paste0("data/01_original_data/monitoring_uk/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))
data4 <- readRDS("data/01_original_data/monitoring_uk/auxilliary/01_2021-09-22_data_before_typologies.rds")

# TYPOLOGY -----------------------------------------------------------------------------
data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_uk/",Sys.Date(),"_final_non_aggregated.rds")) 
data5 <- readRDS("data/01_original_data/monitoring_uk/2021-10-05_final_non_aggregated.rds")
# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/monitoring_uk/",Sys.Date(),"_final_aggregated.rds")) 

# SUMMARY STATISTICS ------------------------------------------------------------------------
data6[,uniqueN(gr_sample_id)]
data6[distance <= 500,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE ,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE & distance <= 500,uniqueN(gr_sample_id)] 

