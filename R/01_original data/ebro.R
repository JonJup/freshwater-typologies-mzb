# —————————————————————————————————————————————————— #
# ——— Clean Ebro Hydrographic Confederation data ——— # 
# —————————————————————————————————————————————————— #

# ———————————————————————————————————
# date created: 15.07.21
# date last modified: 05-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data provided from Ebro Hydrographic Confederation by Miguel Canedo-Argüelles. 
# Temporal aggregation: Yes 
# ————————————————

# setup -----------------------------------------------------------------------------

pacman::p_load(
        here,
        taxize,
        data.table,
        sf,
        dplyr,
        ggplot2,
        lubridate,
        magrittr,
        mapview,
        purrr,
        readr,
        readxl,
        stringr,
        tidyr
)

# load data  ------------------------------------------------------------------------
samples1   <- read_excel("data/01_original_data/ebro/raw_data/macroinvertebrates_2000-2009.xlsx") 
samples2   <- read_excel("data/01_original_data/ebro/raw_data/macroinvertebrates_2010-2020.xlsx")
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# prepare data  ---------------------------------------------------------------------
setDT(samples1)
setDT(samples2)

samples <- rbindlist(list(samples1, samples2))    
unique(samples$Parámetro)
unique(samples$Unidad)

#- keep only abundance records 
samples <- samples[Parámetro == "Abundancia macroinvertebrados"]

#- separate indices for pristine sites
names(samples)[c(1, 13, 14)] = c("site_id", "parameter", "taxon")
#parameters <- unique(samples$parameter)

#- In the data set are two parameters that can be used to judge water quality  
#- The Iberian Bio-Monitoring Working Party (IBMWP) goes back to Alba Tercedor (1988) who adopted the
#- British index (BMWP). Greater values = higher quality. 
#- Iberian Average Score per Taxon (IASPT) is the IBMWP divided by the number identified taxa.

# IASPT <- samples[parameter == "Indice IASPT"]

## ——————————————— ##
## ——— IASPT ————— ##
## ——————————————— ## 

# IASPT <- IASPT[, .SD, .SDcols = c(3,11,16)]
# names(IASPT) <- c("site", "date", "IASPT")
# IASPT$date <- dmy(IASPT$date)
# IASPT$IASPT <- as.numeric(IASPT$IASPT)
# IASPT[, IASPT_mean := mean(IASPT, na.rm = T),  site]
# # IASPT[, IASPT_max := max(IASPT, na.rm = T),  site]
# # IASPT[, IASPT_min := min(IASPT, na.rm = T),  site]
# # IASPT[, IASPT_range := IASPT_max- IASPT_min]
# 
# IASPT %<>% unique(by = "site")
# IASPT[,least.impact := ifelse(IASPT_mean >= 4.5, 1,0) ]
# IASPT <- IASPT[, c("site", "least.impact")]


## —————————————— ##
## ——— data ————— ##
## —————————————— ## 

#- check that each sites ID only has one X and Y coordinate 
table(samples[, uniqueN(ETRS89_X30), by = "site_id"]$V1)
table(samples[, uniqueN(ETRS89_Y30), by = "site_id"]$V1)



data <- samples[,
               list(
                       date = dmy(Fecha),
                       original_site_name = site_id,
                       taxon =  taxon,
                       x.coord = ETRS89_X30,
                       y.coord = ETRS89_Y30 ,
                       EPSG = 25830,
                       data.set = "ebro_hydrographic_confederation",
                       abundance = Valor
               )]

data[,  c("year", "month") := list(year(date), month(date))]
data[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]

data <- data[!(taxon %in% c("--"))]

#- taxonomy
data[, taxon := str_remove_all(taxon, "^Fam.\\ ")]
data[, taxon := str_remove_all(taxon, "^Clase\\ ")]
data[, taxon := str_remove_all(taxon, "^Filo\\  ")]
data[, taxon := str_remove_all(taxon, "\\ sp\\.$")]
data[, taxon := str_remove_all(taxon, "^Orden\\ ")]
data[taxon == "Polyarthra vulgaris-dolichoptera", taxon := "Polyarthra"]

data2 <- data

TU <- sort(unique(data2$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
TU <- TU[new_tu]


#- clean up
rm(samples1, samples2, samples, data)
gc()

# taxonomy --------------------------------------------------------------------------

taxontable <- 
        jjmisc::update_taxonomy(TU)
#- identify elements that need to be entered manually 
taxontable[clean == FALSE]
# COMBINE DATA AND TAXONOMY ---------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]


unique(data3$kingdom)
unique(data3$phylum)
unique(data3$class)
unique(data3$subclass)
unique(data3$order)


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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_ebro")]

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
        x.coord = x.coord,
        y.coord = y.coord,
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
                                                                  ifelse(!is.na(phylum), phylum,0)))))))]

data4 <- data4[abundance != "+"]
data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("lowest.taxon", "gr_sample_id")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/ebro/auxilliary/01_",Sys.Date(),"_data_before_typology.rds"))
data4 <- readRDS("data/01_original_data/ebro/auxilliary/01_2021-09-21_data_before_typology.rds")
 # SITES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)

#- save to or load from file 
saveRDS(data5, paste0("data/01_original_data/ebro/", Sys.Date(), "_final_non_aggregated.rds"))
data5 <- readRDS("data/01_original_data/ebro/2021-09-21_final_non_aggregated.rds")

# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/ebro/",Sys.Date(),"_final_aggregated.rds"))

# STATISTICS ------------------------------------------------------------------------

uniqueN(data5$gr_sample_id)
summary(data5$year)

# samples 
# one factor 
data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
# two factor 
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()


uniqueN(data6$gr_sample_id)
data6 |> filter(distance <= 500) |> distinct(gr_sample_id) |> count()
data6 |> filter(least.impacted)  |> distinct(gr_sample_id) |> count()
data6 |> filter(least.impacted & distance <= 500)  |> distinct(gr_sample_id) |> count()

