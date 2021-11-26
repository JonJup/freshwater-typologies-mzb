# ——————————————————————————————————— #
# ——— Clean Dutch Monitoring Data ——— # 
# ——————————————————————————————————— #

# ———————————————————————————————————
# date created:  14.07.21
# date last modified: 05-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data provided by Edwin Peters. 
# EPSG based on projfinder: 28992
# Temporal aggregation: yes 
# ————————————————

# SETUP -------------------------------------------------------------------

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
        stringr
)


# LOAD DATA -------------------------------------------------------------------------
sites      <- read_csv2("data/original data/monitoring_dutch/raw data/MaFa_Description_Locations_Getreal.csv")
samples    <- read_csv2("data/original data/monitoring_dutch/raw data/MaFa_Getreal_Genus.csv")
taxontable <- readRDS(paste0("data/original data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA --------------------------------------------------------

#- transform objects to data.table 
setDT(sites)
setDT(samples)

#- drop sites that are not from running waters 
#- this reduces the number of sites from 14812 to 4237
#- dropping sites with either x or y coordinates missing further reduces to 4037
sites <- sites[Watertype %in% c("BRN","KRK" ,"OUD" ,"RIV", "STR") & !is.na(X_coord) & !is.na(Y_coord)]

#- join sample and site data 
data <- samples[sites, on = "Location"]
#- add a season variable 
data[, season := ifelse(month(Date) %in% c(12,1,2), "winter",
                        ifelse(month(Date) %in% c(3,4,5), "spring",
                        ifelse(month(Date) %in% c(6,7,8), "summer", "autumn")))]
#- there are some rows with empty GenusName
data2 = data[,list(
        original_site_name = Location,
        date = dmy_hms(Date),
        season,
        taxon = GenusName,
        abundance = Counts,
        x.coord = X_coord,
        y.coord = Y_coord,
        EPSG = 28992,
        data.set = "monitoring data from the Netherlands"
)]

#- create year variable  
data2[, year := year(date)]
#- drop rows without taxa denomination 
data2 <- data2[!is.na(taxon)]
#- taxonomy 
TU <- sort(unique(data2$taxon))
#- some family and order names are always followed by "indet". 
#- Remove that. 
data2[, taxon := str_remove(taxon, "_indet$")]
data2[, taxon := str_remove(taxon, "_indete$")]
data2[, taxon := str_remove(taxon, "_INDET$")]
#- Agabus and Iybius are genera in Dytiscidae
data2[taxon == "Agabus / Ilybius indet", taxon := "Dytiscidae"]
#- remove some taxa 
data2 <- data2[! taxon %in% c("Amphichaete", "Aschelminthes", "Giardia", "Salctula")]

#- extract unique taxon names 
TU <- sort(unique(data2$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])



# TAXONOMY  --------------------------------------------------

taxontable <- update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

#- check 
sort(unique(data3$kingdom))
sort(unique(data3$phylum))
sort(unique(data3$class))
sort(unique(data3$subclass))
sort(unique(data3$order))

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_dutch_monitoring")]

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
        x.coord = x.coord / 100,
        y.coord = y.coord / 100,
        EPSG = EPSG,
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
data4[is.na(lowest.taxon), unique(original_name)]

data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- Some sites are at a wrong position. I will remove them. 
site_to_remove <-  c("00459", "00454", "00512", "00560", 
                     "00495", "00539", "00554", "00538", 
                     "00567", "00469", "00531", "00484", 
                     "00490", "00529", "00505")
data4 <- data4[! site_id %in% site_to_remove]
rm(site_to_remove)

#- save to or load from file 
saveRDS(data4, paste0("data/original data/monitoring_dutch//auxilliary/02_",Sys.Date(),"_data_before_typologies.rds"))
data4 <- readRDS("data/01_original_data/monitoring_dutch/auxilliary/02_2021-09-22_data_before_typologies.rds")
# SITES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_dutch", Sys.Date(),"_final_non_aggregated.rds"))


# TEMPORAL AGGREGATION  -------------------------------------------------------------
source("R/newest_sample.R")
data5 <- data5[year > 2004]
data6 <- newest_sample(data5) 
saveRDS(data6, paste0("data/01_original_data/monitoring_dutch/",Sys.Date(),"_final_aggregated.rds"))
# Statistics ------------------------------------------------------------------------
uniqueN(sites6$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)

# samples 
# one factor 
data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(year > 2004)  |> pull(gr_sample_id) |> uniqueN()
# two factor 
data5 |> filter(year > 2004) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(year > 2004) |> pull(gr_sample_id) |> uniqueN()
# three factors
data5 |> filter(year > 2004) |> filter(brt_distance <= 500) |> filter(fec.least.impacted)|> pull(gr_sample_id) |> uniqueN()

sites6 |> filter(brt_distance <= 500) |> count()
sites6 |> filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(brt_distance <= 500) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
sites6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()

#- aggregated 
uniqueN(data6$gr_sample_id)
data6[distance <= 500, uniqueN(gr_sample_id) ]
data6[least.impacted == TRUE , uniqueN(gr_sample_id) ]
data6[least.impacted ==TRUE & distance <= 500, uniqueN(gr_sample_id)]




