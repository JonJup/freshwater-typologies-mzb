# ———————————————————————————————————————— #
# ——— Clean Portuguese Monitoring data ——— # 
# ———————————————————————————————————————— #


# ———————————————————————————————————
# date created: 13.08.21
# date last modified: 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the Portuguese monitoring data provided by Teresa Ferreira 
# temporal aggregation: no 
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

source("R/01_original data/monitoring_portugal_collect_data.R")
taxontable <- readRDS(paste0("data/original data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA   ---------------------------------------------------------------------

TU <- sort(unique(data$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

data[taxon %in% c("[Kl:Oligochaeta]","[Kl:oligochaeta]"), taxon := "Oligochaeta"]
data <- data[!taxon %in% c("Iptis_final", "N/A", "Nemathelmintha")]
data[, taxon := str_remove(taxon, "\\.\\.\\.")]
data[, taxon := str_remove_all(taxon, "\\d")]
data[, taxon := str_remove_all(taxon, "\\(Ad\\.\\)")]
data[, taxon := str_remove_all(taxon, "\\(Lv\\.\\)")]
data[, taxon := str_to_title(taxon)]
data[, taxon := str_trim(taxon)]
data[taxon == "Limaoniidae", taxon := "Limoniidae"]
data[taxon == "Hidracarina", taxon := "Hydrachnidae"]
data[taxon == "Limanephilidae", taxon := "Limnephilidae"]
data[taxon == "Polycentropididae", taxon := "Polycentropodidae"]


# TAXONOMY --------------------------------------------------------------------------

taxontable2 <- update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------
data2 <- data
setDT(data2)
names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]



#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := .GRP, by = c("year", "season")]


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
saveRDS(data4, paste0("data/original data/monitoring_portugual/auxilliary/02_",Sys.Date(),"_data_before_typologies.rds"))

# SITES -----------------------------------------------------------------------------
data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/original data/monitoring_portugual/", Sys.Date(), "_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)

#--> TEMOPORAL AGGREGATION not necessary 


# SUMMARY STATISTICS ----------------------------------------------------------------
summary(data5$year)
sites6 |> nrow()
sites6 |> filter(brt_distance <= 500) |> nrow()
sites6 |> filter(fec.least.impacted)  |> nrow()
sites6 |> filter(brt_distance <= 500 & fec.least.impacted) |> nrow()

data5[, uniqueN(gr_sample_id)]
data5[brt_distance <= 500, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE, uniqueN(gr_sample_id)]
data5[year >= 2004, uniqueN(gr_sample_id)]
data5[brt_distance <= 500 & year >= 2004, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE & year >= 2004, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE & brt_distance <= 500, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE & brt_distance <= 500 & year >= 2004, uniqueN(gr_sample_id)]
   
