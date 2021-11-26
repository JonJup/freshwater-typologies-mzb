# —————————————————————————————— #
# ——— Clean AQEM SWEDEN data ——— # 
# —————————————————————————————— #

# ———————————————————————————————————
# date created: 14-07-21
# date last modified: 05-10-21
# Project: Evaluating European Broad River Types with Macroinvertebrates
# Purpose: In this script, I create a harmonized spatial data set from the raw data AQEM Sweden provided from  by Leonard Sandin. 
# Temporal aggregation: No 
# ————————————————

# setup -----------------------------------------------------------------------------

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

# — — — FUNCTIONS
source("R/fill_taxon_table.R")
source("R/get_last_date.r")

# load data  ------------------------------------------------------------------------
samples    <- read_excel("data/original data/aqem_sweden/raw_data/CompleteTaxalist_AQEM_Site_info.xls", skip = 2) 
sites      <- read_excel("data/original data/aqem_sweden/raw_data/CompleteTaxalist_AQEM_Site_info.xls", sheet = 2)
taxontable <- readRDS(paste0("data/original data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")
rm(most_recent_date)

# prepare data  ---------------------------------------------------------------------

#- transform to data table 
setDT(samples)
setDT(sites)

#- rename 
names(samples)[c(1,2)] <- c("taxon", "taxon_supp")

#- Some rows are only summaries of the rows below them, aggregated to a Family
#- level. They need to be removed. In these rows the second column is empty. Some
#- data are only collected at family level though. I want to maintain them but
#- their second column is also empty. 
samples <-
        samples[!(
                str_detect(samples$taxon, "[A-Z]{2,}") |
                        taxon %in% c(
                                "[Kl:Turbellaria]",
                                "Turbellaria",
                                "Nematoda",
                                "[Kl:Nematoda]",
                                "[Kl:Oligochaeta]",
                                "Oligochaeta",
                                "[Ord:Lepidoptera]"
                        )
        )]

samples[taxon_supp %in% c("Gen. sp.", "sp.", "Gen. sp. Lv.", "sp. Lv.", "sp. Ad."), 
         taxon_supp := ""]

samples[, taxon_clean := str_trim(paste(taxon,taxon_supp))]

samples <- samples[,.SD, .SDcols = 7:157]
samples <- samples[-1]

site_names <- colnames(samples)[-151]
samples = data.table::melt.data.table(samples, id.vars = c("taxon_clean"))
samples = samples[value != 0]

sites = data.table(
        site = rep(sites$`sampling site`,2),
        x.coord = as.numeric(rep(sites$`X_RAK (Xnew)`,2)),
        y.coord = as.numeric(rep(sites$`Y_RAK (Xnew)`,2)),
        id = append(sites$`Sample Number Spring`,sites$`Sample Number Autumn`),
        date = append(sites$`1st sampling date`, sites$`2nd sampling date`)
        
)
sites[,date := dmy(date)]
sites[,c("year","season") := 
               list(
                       year(date),
                       ifelse(month(date) %in% c(3,4,5), "spring", 
                              ifelse(month(date) %in% c(6,7,8), "summer", 
                                     ifelse(month(date) %in% c(9,10,11), "autumn", "winter"
                                     )))
               )]
samples$id = as.character(samples$variable)


data <- sites[samples, on = "id"]
data2 <- data[, list(
        original_site_name = site,
        date = ymd(date),
        year,
        season,
        taxon = taxon_clean,
        abundance = value,
        x.coord ,
        y.coord,
        EPSG = 2400,
        data.set = "AQEM_sweden"
)]


data2[, taxon := str_remove_all(taxon, "-Gr\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Lv\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Ad\\.$")]

data2[taxon == "Amphinemura standfussi/sulcicollis", taxon := "Amphinemura"]
data2[taxon == "Baetis macani/bundaye", taxon := "Baetis"]
data2[taxon == "Gyraulus acronicus/albus/laevis", taxon := "Gyraulus"]
data2[taxon == "Leuctra fusca/digitata/hippopus", taxon := "Leuctra"]
data2[taxon == "Oulimnius troglodytes/tuberculatus", taxon := "Oulimnius"]
data2[taxon == "Radix peregra/ovata", taxon := "Radix"]
data2[taxon == "Rhyacophila obliterata/nubila", taxon := "Rhyacophila"]
data2[taxon == "Chaetopteryx/Anitella", taxon := "Limnephilidae"]
data2[taxon == "Mystacides longicornis/nigra", taxon := "Mystacides"]

TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

# taxonomy --------------------------------------------------------------------------
taxontable <- jjmisc::update_taxonomy(TU)

# COMBINE DATA AND TAXONOMY ------------------------------------------------------------

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_aqem_Sweden")]

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
        x.coord = y.coord,
        y.coord = x.coord,
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
data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/aqem_sweden/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))
data4 <- readRDS("data/01_original_data/aqem_sweden/auxilliary/01_2021-09-21_data_before_typologies.rds")
# TYPOLOGIES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)

#- save to or load from file 
saveRDS(data5, paste0("data/original data/aqem_sweden/", Sys.Date(), "_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)
#- always sampled in two different seasons 
data5[, uniqueN(season), by = "site_id"]

#- Aggregation not necessary 


# STATISTICS ------------------------------------------------------------------------
uniqueN(sites$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)

# samples 
# one factor 
data5 |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(year > 2004)  |> pull(gr_sample_id) |> uniqueN()
# two factor 
data5 |> filter(year > 2004) |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(year > 2004) |> pull(gr_sample_id) |> uniqueN()
# three factors
data5 |> filter(year > 2004) |> filter(distance <= 500) |> filter(fec.least.impacted)|> pull(gr_sample_id) |> uniqueN()

sites |> filter(distance <= 500) |> count()
sites |> filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(distance <= 500) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |> filter(fec.least.impacted) |> filter(distance <= 500) |> count()
sites |> filter(fec.least.impacted) |> filter(distance <= 500) |> count()



