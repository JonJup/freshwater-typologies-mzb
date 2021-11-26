# ———————————————————————————— #
# ——— Clean WISER MZB data ——— # 
# ———————————————————————————— #


# ———————————————————————————————————
# date created: 14.07.21
# date last modified: 22-09-21
# Project:  Evaluating European Broad River Types with Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data 
# provided from WISER by Christian Feld. 
# Temporal aggregation: No 
# ————————————————


# 01. Setup -------------------------------------------------------------------

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

# load data -------------------------------------------------------------------------

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxatable.rds")

samples <- read_excel("data/01_original_data/wiser/raw_data/WISER_Invertebrate_taxa.xlsx") 
sites   <- read_excel("data/01_original_data/wiser/raw_data/WISER_Metadata_Abiotics.xls") 
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------
setDT(samples)
setDT(sites)

samples <- samples[, list(
        original_site_name = StationCode, 
        date = ymd(InvSampleDate),
        year = year(InvSampleDate),
        season = ifelse(
                month(InvSampleDate) %in% c(12,1,2), "winter", 
                ifelse(month(InvSampleDate) %in% c(3,4,5), "spring", 
                       ifelse(month(InvSampleDate) %in% c(6,7,8), "summer", "autumn"))),
        taxon = InvTaxonName_ad,
        abundance = InvAbundance
)]


sites <- sites[, list(
        original_site_name = StationCode, 
        x.coord = Longitude, 
        y.coord = Latitude,
        EPSG = 4326,
        data.set = "wiser"
)]

#- join sites and sample data 
data <- sites[samples, on = "original_site_name"]

#- taxomomy 
data[, taxon := str_remove_all(taxon, "\\-Gr\\.")                 ] 
data[, taxon := str_remove_all(taxon, "\\ ssp\\.$")               ]   
data[, taxon := str_remove_all(taxon, "\\ sp\\.$")                ] 
data[, taxon := str_remove_all(taxon, "\\ Gen\\.$")               ] 
data[, taxon := str_remove_all(taxon, "\\ Ad\\.$")                ] 
data[, taxon := str_remove_all(taxon, "\\ Lv\\.$")                ] 
data[, taxon := str_remove_all(taxon, "\\-Agg\\.$")               ] 
data[, taxon := str_remove_all(taxon, "\\-Agg\\.$")               ] 
data[, taxon := str_remove_all(taxon, "\\ \\(Hyperrhyacophila\\)")] 
data[, taxon := str_remove_all(taxon, "\\ \\(Rhyacophila\\)")     ] 
data[, taxon := str_remove_all(taxon, "\\ \\(Holotanypus\\)")     ] 
data[, taxon := str_remove_all(taxon, "\\ \\(Psilotanypus\\)")    ] 
data[, taxon := str_remove_all(taxon, "\\ sp\\.$")                ] 
data[, taxon := str_remove_all(taxon, "\\ \\(Glyptotendipes\\)")  ] 
data[, taxon := str_remove_all(taxon, "\\ \\-Gr\\.")              ] 
data[, taxon := str_remove_all(taxon, "\\-Gr\\.")                 ] 
data[, taxon := str_remove_all(taxon, "\\ ssp\\.$")               ]
data[, taxon := str_replace_all(taxon, pattern = "Nemoura/Nemurella", replacement = "Nemouridae")]
data[, taxon := str_replace_all(taxon, pattern = "Jungiella/Psychoda/Tinearia", replacement = "Psychodidae")]
data[, taxon := str_replace_all(taxon, pattern = "Anisus leucostoma/spirorbis", replacement = "Anisus")]
data[, taxon := str_replace_all(taxon, pattern = "Conchapelopia/Arctopelopia", replacement = "Chironomidae")]
data[, taxon := str_replace_all(taxon, pattern = "Haliplus \\(Haliplus\\)", replacement = "Haliplus")]
data[, taxon := str_replace_all(taxon, pattern = "Haliplus \\(Haliplus\\)", replacement = "Haliplus")]
data[, taxon := str_replace_all(taxon, pattern = "Helophorus aequalis/aquaticus", replacement = "Helophorus aequalis")]
data[, taxon := str_replace_all(taxon, pattern = "Hydraena assimilis/riparia", replacement = "Hydraena")]
data[, taxon := str_replace_all(taxon, pattern = "Sigara distincta/falleni/iactans/longipalis", replacement = "Sigara")] 

TU <- sort(unique(data$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])
# taxonomy --------------------------------------------------------------------------
taxontable <- update_taxonomy(TU)

# COMBINE DATA AND TAXONOMY ---------------------------------------------------------
names(data)[which(names(data) == "taxon")] <- "original_name"
data3 <- taxontable[data, on = "original_name"]

sort(unique(data3$kingdom))
sort(unique(data3$phylum))
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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_wiser")]

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
data4[is.na(lowest.taxon)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save sites to file 
saveRDS (data4, paste0("data/01_original_data/wiser/auxilliary/01_",Sys.Date(),"_data_before_typology.rds"))

# TYPOLOGY -----------------------------------------------------------------------------
data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/wiser/",Sys.Date(),"_final_non_aggregated.rds"))

# SUMMARY STATISTICS ------------------------------------------------------------------------
uniqueN(sites$site_id)
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

sites |> filter(brt_distance <= 500) |> count()
sites |> filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(brt_distance <= 500) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
sites |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()

