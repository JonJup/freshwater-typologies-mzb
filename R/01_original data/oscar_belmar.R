# ——————————————————————————————— #
# ——— Clean Oscar Belmar data ——— # 
# ——————————————————————————————— #


# ———————————————————————————————————
# date created: 02.08.21
# date last modified: 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data 
# provided by Oscar Belmar. 
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
        stringr,
        tidyr
)


# LOAD DATA  ------------------------------------------------------------------------

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxatable.rds")

data       <-  fread("data/01_original_data/oscar_belmar/raw_data/Taxa_sp.csv", header = T)
sites      <-  read_excel("data/01_original_data/oscar_belmar/raw_data/Sites.xlsx")
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA   ---------------------------------------------------------------------

data2 <- pivot_longer(data, cols = !TAXA)
data2 %<>% filter(!is.na(value))
sites$Site = as.character(sites$Site)

data = left_join(x = data2,
                 y = sites,
                 by = c("name" = "Site")) %>% setDT

names(data)[which(names(data) == "Coor UTM X")] <- "x.coord"
names(data)[which(names(data) == "Coor UTM Y")] <- "y.coord"

data2 = data[, list(
        site = Name,
        date = ymd(Date),
        year = year(Date),
        month = month(Date),
        taxon = TAXA,
        y.coord,
        x.coord,
        abundance = value,
        EPSG = 23030,
        data.set = "oscar_belmar"
)]

data2[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]

TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
TU <- TU[new_tu]

# TAXONOMY --------------------------------------------------------------------------

taxontable <- update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

#- add site and date ids 
data3[, site_id := .GRP, by = "site"]
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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_oscar_belmar")]

data4 <- data3[, list(
        gr_sample_id,
        original_site_name = site,
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
        EPSG = 23030,
        data.set = "segura_basin"
        
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

data4 <- data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

saveRDS(data4, paste0("data/01_original_data/oscar_belmar/auxilliary/01_", Sys.Date(), "_data_before_typologies.rds"))

# TYPOLOGY -----------------------------------------------------------------------------
data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/oscar_belmar/",Sys.Date(),"_final_non_aggregated.rds")) 

# TEMPORAL AGGREGATION --------------------------------------------------------------
#--> Aggregation not necessary 

# SUMMARY STATISTICS ----------------------------------------------------------------
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

