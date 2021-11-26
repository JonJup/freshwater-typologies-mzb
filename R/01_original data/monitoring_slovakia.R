#  ——————————————————————————————————————————  # 
#  ————— Clean Monitoring Slovakia data —————  # 
#  ——————————————————————————————————————————  #

#  ——————————————————————————————————————————  #
# date created: 25.08.21
# date last modified: 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the monitoring data from Slovakia. 
# See Straka et al (2021) Ecol. Ind. for more details. 
# Temporal aggregation: yes 
#  ——————————————————————————————————————————  #

# SETUP  -----------------------------------------------------------------------------

pacman::p_load(
        here,
        taxize,
        data.table,
        sf,
        sp,
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

data       <- read_excel("data/01_original_data/monitoring_slovakia/raw_data/Data source CZ for Jonathan_River Typology_corrected2.xlsx", sheet = 1) 

taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA   ---------------------------------------------------------------------
#  — — — extract site data 
data1        <- data[1:7, ]
data1.site   <- data1[2,-1] |> unlist() |> unname()
data1.sample <- data1[3,-1] |> unlist() |> unname()
data1.season <- data1[6,-1] |> unlist() |> unname()
data1.year   <- data1[7,-1] |> unlist() |> unname() |> as.numeric()
data1.coords <- data1[4,-1] |> unlist() |> unname()
data1.coords <- data.table(original = data1.coords)
data1.coords[, x.original := str_extract(string = original, pattern = ".*N")]
data1.coords[, y.original := str_extract(string = original, pattern = "N, .*")   |>  str_remove(pattern = "N,")                               |>  str_trim()]
data1.coords[, x.degree   := str_extract(string = x.original, pattern = ".*°")   |>  str_remove(pattern = "°")                                |>  str_trim()]
data1.coords[, y.degree   := str_extract(string = y.original, pattern = ".*°")   |>  str_remove(pattern = "°")                                |>  str_trim()]
data1.coords[, x.minutes  := str_extract(string = x.original, pattern = "°.*'")  |>  str_remove(pattern = "°") |> str_remove(pattern = "'") |>  str_trim()]
data1.coords[, y.minutes  := str_extract(string = y.original, pattern = "°.*'")  |>  str_remove(pattern = "°") |> str_remove(pattern = "'") |>  str_trim()]
data1.coords[, x.seconds  := str_extract(string = x.original, pattern = "'.*\"") |>  str_remove(pattern = "'") |> str_remove(pattern = "\"") |>  str_trim()]
data1.coords[, y.seconds  := str_extract(string = y.original, pattern = "'.*\"") |>  str_remove(pattern = "'") |> str_remove(pattern = "\"") |>  str_trim()]
#- there is a few problematic entry 
unique(data1.coords$x.degree)
unique(data1.coords$y.degree)
unique(data1.coords$x.minute)
unique(data1.coords$y.minute)
unique(data1.coords$x.second)
unique(data1.coords$y.second)
#- put together new coordinates 
data1.coords[, x.new := paste0(x.degree, "°", x.minutes, "!", x.seconds, "§", "E")]
data1.coords[, y.new := paste0(y.degree, "°", y.minutes, "!", y.seconds, "§", "N")]
data1.coords[, x.new := as.numeric(char2dms(data1.coords$x.new, chd = "°", chm = "!", chs = "§"))]
data1.coords[, y.new := as.numeric(char2dms(data1.coords$y.new, chd = "°", chm = "!", chs = "§"))]

#- check on map 
site <-
        data1.coords |> 
        st_as_sf(coords = c("y.new", "x.new"), crs = 4326) 
mapview(site)

data1 <- data.table(site = data1.site, 
                    sample = data1.sample,
                    date = as.Date(NA),
                    year = data1.year,
                    season = data1.season,
                    x.coord = data1.coords$y.new,
                    y.coord = data1.coords$x.new,
                    EPSG = 4326, 
                    data.set = "Monitoring data from Slovakia")


data2 <- data[8:nrow(data), ]
names(data2) <- append("taxon", data1$sample)
data2 <- 
        data2 |> 
        pivot_longer(cols = !taxon,
                     names_to = "sample", 
                     values_to = "abundance"
        ) |> 
        filter(!is.na(abundance))


data3 <- left_join(x = data2, 
                   y = data1,
                   by = "sample")

data3$taxon %<>% str_remove("\\ sp\\.")
data3$taxon %<>% str_remove("\\ Gen\\.")
data3$taxon %<>% str_remove("\\ Ad\\.")
data3$taxon %<>% str_remove("\\ Lv\\.")
data3$taxon %<>% str_remove("\\ L\\.")
data3$taxon %<>% str_remove("\\ s\\.\\ lat\\.")
data3$taxon %<>% str_remove("-Gr\\.")
data3$taxon %<>% str_remove("\\ Gr\\.")
data3$taxon %<>% str_remove("\\ agg\\.")

setDT(data3)

data3[taxon == "Tvetenia bavarica/calvescens", taxon := "Tvetenia"]
data3[taxon == "Thienemanniella vittata/clavicornis", taxon := "Thienemanniella"]
data3[taxon == "Heterotrissocladius grimshawi/scutellatus", taxon := "Heterotrissocladius"]
data3[taxon == "Rhithrogena iridina/picteti", taxon := "Rhithrogena"]
data3[taxon == "Orthocladius obumbratus/oblidens", taxon := "Orthocladius"]


data3 <- data3[!is.na(taxon)]

TU <- sort(unique(data3$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

data2 <- data3

# TAXONOMY --------------------------------------------------------------------------
taxontable <- update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------
setDT(data2)
names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

unique(data3$kingdom)
unique(data3$phylum)   |> sort()
unique(data3$class)    |> sort()
unique(data3$subclass) |> sort()
unique(data3$order) |> sort()
unique(data3$family) |> sort()

#- add site and date ids 
data3[, site_id := .GRP, by = "site"]
data3[, date_id := .GRP, by = "year"]

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_slovakia")]



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
        abundance = as.numeric(abundance),
        x.coord,
        y.coord,
        EPSG ,
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
saveRDS(data4, paste0("data/01_original_data/monitoring_slovakia/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))

# TYPOLOGIES -----------------------------------------------------------------------------

data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_slovakia/",Sys.Date(),"_final_non_aggregated.rds"))
data5 <- readRDS("data/01_original_data/monitoring_slovakia/2021-09-22_final_non_aggregated.rds")
# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/monitoring_slovakia/",Sys.Date(),"_final_aggregated.rds"))  

# Statistics ------------------------------------------------------------------------
data6[,uniqueN(gr_sample_id)]    
data6[distance <= 500,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE ,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE & distance <= 500,uniqueN(gr_sample_id)] 
