# ---------------------------------------------------- #
# -------- Clean Finnish Monitoring data ------------- # 
# ---------------------------------------------------- #


# --------------------------------------------------------------------------------------------------------
# date created: 13.09.21
# date last modified: 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the Finnish monitoring data provided by Jukka Aroviita
# Temporal aggregation: No 
# EPSG: 3067 TM35FIN(E,N) -- Finland
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
#- functions 
source("R/fill_taxon_table.R")
source("R/get_last_date.r")
source("R/temoral_aggregation_function.R")
source("R/add_typologies.R")
source("R/update_taxonomy.R")



# load data -------------------------------------------------------------------------

data  <- read_excel("data/original data/monitoring_finnland/raw/Finland_Stream_Macroinvert_Data_N410_ToJupke.xlsx") 
taxontable <- readRDS(paste0("data/original data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")


# prepare data ----------------------------------------------------------------------

#- Transform data to data.table format. 
setDT(data)


# X and Y coordinates are crossed on purpose.
sites <- data.table(
        original_site_name = data$SITENAME,
        date               = ymd_hms(data$SAMPLINGDATE),
        x.coord = data$`P_ETRS-TM35FIN`,
        y.coord  = data$`I_ETRS-TM35FIN`,
        EPSG = 3067, # TM35FIN(E,N) -- Finland
        data.set = "monitoring_finnland"
)

# Is there only one X coordinate per sampling site?
all(sites[,uniqueN(x.coord), by = original_site_name]$V1 == 1)
#- The same for the Y coordinate. 
all(sites[,uniqueN(y.coord), by = original_site_name]$V1 == 1)

sites.plot <- 
        sites |> 
        unique(by="original_site_name") |> 
        st_as_sf(coords = c("y.coord", "x.coord"), crs = sites$EPSG[1])

mapview(sites.plot)

#- biological data 

#- at which column do taxa abundances begin? 
bio.col <- 23

bio <- data[,bio.col:ncol(data)]
bio[, original_site_name := data$SITENAME]
bio %<>% 
        pivot_longer(cols = !original_site_name, names_to = "taxon", values_to = "abundance") %>% 
        filter(!is.na(abundance))

data2 <- left_join(bio, sites)
setDT(data2)


data2[, `:=` 
      (year = year(date), 
              season = ifelse(month(date) %in% c(12,1,2), "winter", 
                              ifelse(month(date) %in% c(3,4,5), "spring", 
                                     ifelse(month(date) %in% c(6,7,8), "summer", "autumn")
                              )
              )
      )
]
)



#- clean up taxon names 
data2[, taxon := str_remove(taxon, "\\ group$")]
data2[, taxon := str_remove(taxon, "\\.\\.\\..*")]
data2[, taxon := str_trim(taxon)]

TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

taxontable <- update_taxonomy(TU)


# COMBINE DATA SETS -----------------------------------------------------------------
#- join taxon information with data 
names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

#- check high taxonomic levels for inconsistencies)
unique(data3$kingdom)
unique(data3$phylum)

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_finnland")]


#- Coordinates are crossed on purpose 
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

data4[, abundance := sum(abundance), by = "lowest.taxon"]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/monitoring_finnland/auxilliary/02_",Sys.Date(),"_data_before_taxonomy.rds"))

# SITES -----------------------------------------------------------------------------

data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/original data/monitoring_finnland/",Sys.Date(),"_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------
data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)

#---> No aggregation necessary 

# SUMMARY STATISTICS ------------------------------------------------------------------------
uniqueN(sites6$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)

# samples 
# one factor 
data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(year >= 2004)  |> pull(gr_sample_id) |> uniqueN()
# two factor 
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(year >= 2004) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(year >= 2004) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
# three factors
data5 |> filter(year > 2004) |> filter(brt_distance <= 500) |> filter(fec.least.impacted)|> pull(gr_sample_id) |> uniqueN()

sites6 |> filter(brt_distance <= 500) |> count()
sites6 |> filter(fec.least.impacted) |> count()
data5  |> filter(year >=2004) |> pull(site_id) |> uniqueN()
sites6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
data5  |> filter(year >=2004 & brt_distance <= 500) |> pull(site_id) |> uniqueN()
data5  |> filter(year >=2004 & fec.least.impacted) |> pull(site_id) |> uniqueN()
data5  |> filter(year >=2004 & brt_distance <= 500 & fec.least.impacted) |> pull(site_id) |> uniqueN()
