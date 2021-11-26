# ——————————————————————————————— #
# ——— Clean data from Naiades ——— # 
# ——————————————————————————————— #

# ———————————————————————————————————
# date first written: 16.07.21
# date last modified: 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data provided 
# from Naiades. 
# Temporal aggregation: yes 
# ————————————————

# SETUP -----------------------------------------------------------------------------

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


# LOAD DATA  ------------------------------------------------------------------------

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxatable.rds")

fauna_table <- fread("data/01_original_data/naiades/raw_data/fauneflore.csv")
geo_table   <- fread("data/01_original_data/naiades/raw_data/stations.csv")

taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA  ---------------------------------------------------------------------

#- The variable LbSupport from fauna table tells us what kind of observation is in the row.
#- There are diatoms, fishes, macroinvertebrates, macrophytes and phytoplancton.
unique(fauna_table$LbSupport)
#- We subset fauna_table to macroinvertebrate observations. 
fauna_table <- fauna_table[LbSupport == unique(fauna_table$LbSupport)[3] & MnTypTaxRep == "NbrTax"]
#- check results 
unique(fauna_table$LbSupport)
#- join macroinvertebrate data with station data in geo_table. The latter contains station coordiantes.  
fauna_table <- geo_table[fauna_table, on = "CdStationMesureEauxSurface"]
data        <- fauna_table[,
             list(
                     "original_site_name" = CdStationMesureEauxSurface,
                     "date"               = ymd(DateDebutOperationPrelBio),
                     "taxon"              = NomLatinAppelTaxon,
                     "x.coord"            = CoordXStationMesureEauxSurface,
                     "y.coord"            = CoordYStationMesureEauxSurface,
                     "EPSG"               = LibelleProjection,
                     "abundance"          = RsTaxRep
             )
]

data[,c("year", "season") := .(year(date), case_when(month(date) %in% c(12,1,2) ~ "winter",
                                                     month(date) %in% c(3,4,5) ~ "spring",
                                                     month(date) %in% c(6,7,8) ~ "summer",
                                                     month(date) %in% c(9,10,11) ~ "autumn"))]

#- The data set contains data in four different coordinate reference systems. 
#- Most data are RGF93 / Lambert 93 (EPSG: 2154). These are the only data we will keep. 
#- The remaining data are in French Guyana (CRS: RGFG95 / UTM 22; EPSG: 2972), 
#- Fort-de-France (a Caribbean island; CRS: RRAF 91 (WGS84) / UTM 20; EPSG: 2989) or are lacking 
#- spatial coordinates and hence also a reference system. 

data <- data[EPSG == "RGF93 / Lambert 93"]
data[, c("EPSG", "data.set") := .(2154, "naiades")]
data[, EPSG := as.numeric(EPSG)]
data2 <- data

data2[taxon == "Triaenodes/Ylodes", taxon := "Leptoceridae"]
data2[taxon == "Chaetopteryx villosa/fusca", taxon := "Chaetopteryx"]

#- some taxa seem to have received a wrong tag. These are not invertebrates 
data2 <- data2[taxon != "Appellation de Taxon inconnue"]
data2 <- data2[taxon != "Marsilea quadrifolia"]
data2 <- data2[taxon != "Elodes palustris"]
data2 <- data2[taxon != "Merluccius merluccius"]
data2 <- data2[taxon != "Code gelÃ© 1999 (nematomorphes)"]
data2 <- data2[taxon != "Hantzschia amphioxys var. vivax"]
data2 <- data2[taxon != "Nemathelmintha"]

#- check sites on map 
# data2 |> 
#         unique(by = "original_site_name") |> 
#         st_as_sf(coords = c("x.coord", "y.coord"), crs = 2154) |> 
#         mapview()


#- first look at taxa 
TU <- sort(unique(data2$taxon))
#- which ones are not yet in taxa table
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

#- clean up 
rm(data, fauna_table, geo_table, new_tu)
gc()

# TAXONOMY --------------------------------------------------------------------------

taxontable <- update_taxonomy(TU)
# COMBINE DATA AND TAXONOMY --------------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

sort(unique(data3$kingdom))
sort(unique(data3$phylum))
sort(unique(data3$class))
sort(unique(data3$subclass))
sort(unique(data3$order))

data3 <- data3[kingdom != "Chromista"]


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
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_naiades")]

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

data4 <- data4[!is.na(x.coord)]

#- combine entries of same taxon 
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]

data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/naiades/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))
data4 <- readRDS("data/01_original_data/naiades/auxilliary/01_2021-09-22_data_before_typologies.rds")
# SITES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/naiades/",Sys.Date(),"_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data5 <- data5[year>=2009]
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/naiades/",Sys.Date(),"_final_aggregated.rds"))
# SUMMARY STATISTICS ----------------------------------------------------------------
data6[,uniqueN(gr_sample_id)]    
data6[distance <= 500,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE ,uniqueN(gr_sample_id)]    
data6[least.impacted == TRUE & distance <= 500,uniqueN(gr_sample_id)] 

