# ---------------------------------------------------- #
# -------- Clean czech Monitoring data ------------- # 
# ---------------------------------------------------- #


# --------------------------------------------------------------------------------------------------------
# date created: 03-09-21
# date last modified: 04-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the Czech Republic monitoring data provided by Michal Straka and Marek Polášek
# Note: Marek Polášek has send an updated version on the 01.10.21. The new file is called:  CZECH_state_monitoring_updated.csv
# --------------------------------------------------------------------------------------------------------

# SETUP  -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/JonJup/jjmisc")
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
# source("R/fill_taxon_table.R")
# source("R/get_last_date.r")
# source("R/temoral_aggregation_function.R")
# source("R/add_typologies.R")
# source("R/update_taxonomy.R")


# load data -------------------------------------------------------------------------

data  <- fread("data/01_original_data/monitoring_czech/raw/CZECH_state_monitoring_updated.csv") 
most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", "taxontable.rds")
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")


# prepare data ----------------------------------------------------------------------

# X and Y coordinates are crossed on purpose.
data2 <- data.table(
        original_site_name = data$site_id,
        site_name = data$`site name`,
        date               = dmy(data$date),
        taxon              = data$`taxon name`,
        x.coord = data$JTSK_Y * -1,
        y.coord  = data$JTSK_X * -1,
        EPSG = 2065, # S-JTSK (Ferro) / Krovak
        data.set = "monitoring_czech2",
        abundance = 1
)

data2 <- data2[!is.na(x.coord)]
data2 <- data2[x.coord != 0]

data2 |> 
        unique(by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) -> 
        sites 

mapview(sites)

#- some site are outside of Czech Republic 
#- get shapefile for Czech Republic   
#- here I use the naturalearth data set because geodata::gadm() and geodata::world()returned an error 
#- (cannot open URL 'https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_adm0_r5_pk.rds': HTTP status was '403 Forbidden') 
#- apparently something is awry on the gadm website (gadm.org) )(date:04.10.21 @ 7:00 AM)
#- Result: for the outside-czech data; x and y coordinates are swapped. 
czech <- st_read("E://Arbeit/Data/natural_earth/Europe/Europe.shp")
czech %<>% filter(SOV_A3 == "CZE") %>% st_transform(crs = st_crs(sites))

data.in.cz <- st_within(x = sites, 
                        y = st_buffer(czech, dist = units::as_units(15, "km")))
outside_id <- which(lengths(data.in.cz) == 0)

outside_id <- sites[outside_id,"original_site_name"]

# out.sites <- filter(sites, !original_site_name %in% outside_id$original_site_name)
# mapview(out.sites)

data2[original_site_name %in% outside_id$original_site_name, c("y.new", "x.new") := .(x.coord, y.coord)]

data2 |> unique(by="original_site_name") |> filter(!is.na(y.new)) |> st_as_sf(coords = c("y.coord", "x.coord"), crs = data2$EPSG[1]) |> mapview()

data2[!is.na(y.new), c("y.coord", "x.coord") := .(y.new, x.new)]

data2[, c("y.new", "x.new") := NULL]

data2 |> 
  unique(by = "original_site_name") |> 
  st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) |> 
  mapview()

#- fix and save original data set 
# data[site_id %in% outside_id$original_site_name, c("y.new", "x.new") := .(JTSK_X, JTSK_Y)]
# data[!is.na(y.new), c("JTSK_X", "JTSK_Y") := .(x.new, y.new)]
# data[,c("x.new", "y.new") := NULL]
# 
# outside_id%<>%rename(site_id = original_site_name)
# 
# fwrite(data, "data/auxilliary/czech_monitoring_jonathan_fixed_coordinates.csv")
# fwrite(st_drop_geometry(outside_id), "data/01_original_data/monitoring_czech/auxilliary/switched_coordinate_sites.csv")

data2[, `:=` 
      (year = year(date), 
              season = ifelse(month(date) %in% c(12,1,2), "winter", 
                              ifelse(month(date) %in% c(3,4,5), "spring", 
                                     ifelse(month(date) %in% c(6,7,8), "summer", "autumn")
                              )
              )
      )
]
#- check that each site only has set of coordinates 
all(data2[,uniqueN(x.coord), by = "original_site_name"]$V1 == 1)
all(data2[,uniqueN(y.coord), by = "original_site_name"]$V1 == 1)
# table(data2[,uniqueN(x.coord), by = "original_site_name"]$V1 == 1)
# multicoord.x <- data2[,uniqueN(x.coord), by = "original_site_name"][V1 == 2, original_site_name]
# multicoord.y <- data2[,uniqueN(y.coord), by = "original_site_name"][V1 == 2, original_site_name]
# multicoord.x == multicoord.y
# data2[original_site_name %in% multicoord.x] |> 
#  # unique(by = "date") |> 
#   st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) |> 
#   mapview(zcol = "original_site_name")

#- clean up taxon names 
data2[, taxon := str_remove(taxon, "\\ sp\\.")]
data2[, taxon := str_remove(taxon, "\\ Ad\\.")]
data2[, taxon := str_remove(taxon, "\\ Lv\\.")]
data2[, taxon := str_remove(taxon, "\\ Gen\\.")]
data2[, taxon := str_remove(taxon, "-Gr\\.")]
data2[, taxon := str_remove(taxon, "\\ agg\\.")]
data2[taxon == "Clinocera/Wiedemannia", taxon := "Empididae"]
data2[, taxon := str_trim(taxon)]
data2[str_detect(taxon, "\\/"), taxon := word(taxon, 1)]

TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

taxontable <- update_taxonomy(TU)
saveRDS(taxontable, paste0("data/", Sys.Date(),"_taxontable.rds"))

# COMBINE DATA SETS -----------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

unique(data3$kingdom)
data3 <- data3[!kingdom %in% c("Bacteria", "Plantae", "Fungi", "Chromista", "Protozoa")]
unique(data3$phylum)
data[phylum == "Chordata"]
data3 <- data3[phylum != "Chordata"]


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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_czech2")]

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

data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/monitoring_czech/auxilliary/02_",Sys.Date(),"_data_before_typologies.rds"))
data4 <- readRDS("data/01_original_data/monitoring_czech/auxilliary/02_2021-10-04_data_before_typologies.rds")

# SITES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_czech/", Sys.Date(),"_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------

source("R/newest_sample.R")
data5 <- data5[year >=2004]
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/monitoring_czech/", Sys.Date(),"_final_aggregated.rds"))
# SUMMARY STATISTICS ------------------------------------------------------------------------
data6[,uniqueN(gr_sample_id)]
data6[distance <= 500,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE ,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE & distance <= 500,uniqueN(gr_sample_id)]  
