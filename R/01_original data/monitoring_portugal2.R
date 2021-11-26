# ———————————————————————————————————————— #
# ——— Clean Portuguese Monitoring data ——— # 
# ——— 2019 Data                        ——— #
# ———————————————————————————————————————— #

# ———————————————————————————————————
# date created:  16-08-21
# date last modified:  24-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the 2019er Portuguese  
# monitoring data provided by Teresa Ferreira. 
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
source("R/fill_taxon_table.R")
source("R/get_last_date.r")
source("R/temoral_aggregation_function.R")
source("R/add_typologies.R")
source("R/update_taxonomy.R")


# LOAD DATA  ------------------------------------------------------------------------

data <- read_excel("data/01_original_data/monitoring_portugual2/raw_data/Macroinvertebrados_2019 .xlsx", sheet = 3)
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA   ---------------------------------------------------------------------

site_name <- names(data)[-c(1:2)]
original_site_names <- 
        data[3,-c(1:2)] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[1,-c(1:2)] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[2,-c(1:2)] |> 
        unlist() |> 
        unname() |> 
        as.numeric()

data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = as.Date(NA), year = 2019, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
data2$season <- "summer"

taxa <- data[-c(1:3), -1]
taxa %<>% 
        rename(taxon = "...2") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       season = NA,
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "protugal19"
                      )

data <- data[taxon != "SOMA"]
data[, taxon := str_remove(taxon, "\\(Ad\\)")]
data[, taxon := str_remove(taxon, "\\(Lv\\)")]
data[, taxon := str_trim(taxon)]

TU     <- sort(unique(data$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
TU     <- TU[new_tu]

# TAXONOMY --------------------------------------------------------------------------
taxontable <- update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------
data2 <- data
setDT(data2)
names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3$year = 2019
data3[, date_id := .GRP, by = c("year")]


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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_portugual")]



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
data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/monitoring_portugual2/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))

# TYPOLOGIES -----------------------------------------------------------------------------
data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_portugual2/", Sys.Date(), "_final_non_aggregated.rds"))

