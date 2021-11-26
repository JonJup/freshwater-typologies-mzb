# ------------------------------------------------------ #
# -------- Clean Norwegian Monitoring data ------------- # 
# ------------------------------------------------------ #

# --------------------------------------------------------------------------------------------------------
# date created:  01-10-21
# date last modified: 01-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the Norwegian monitoring
# data provided by Leonard Sandin.  
# temoporal aggregation: no
# EPSG: 4326
# --------------------------------------------------------------------------------------------------------
devtools::install_github("https://github.com/JonJup/jjmisc", force = TRUE)

pacman::p_load(
        here,
        taxize,
        data.table,
        dplyr,
        jjmisc,
        lubridate,
        magrittr,
        mapview,
        purrr,
        readr,
        readxl,
        stringr,
        sf,
        tidyr
)

# LOAD DATA  ------------------------------------------------------------------------
data  <- read_excel("data/01_original_data/monitoring_norway/raw_data/NO_data_inverts_2017_2018_pres_abs.xlsx")
most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", "taxontable.rds")
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA   ---------------------------------------------------------------------

setDT(data)
data[, sample_id := 1:.N]
sites <- data[, c("year","station","date",                        
                  "season","latitude","longitude", "sample_id")]
#- check sites 
# sites |> 
#         st_as_sf(coords = c("longitude", "latitude"), crs=4326) |> 
#         mapview()

bio <- data[, Acari:sample_id]
bio %<>% pivot_longer(cols = !sample_id, names_to = "taxon", values_to = "abundance")
setDT(bio)
bio[str_detect(taxon, "\\ Ad\\."), taxon := str_remove(taxon, "\\ Ad\\.")]
bio[str_detect(taxon, "\\ ad\\."), taxon := str_remove(taxon, "\\ ad\\.")]
bio[str_detect(taxon, "\\ Lv\\."), taxon := str_remove(taxon, "\\ Lv\\.")]
bio[str_detect(taxon, "\\ lv\\."), taxon := str_remove(taxon, "\\ lv\\.")]
bio[taxon == "Baetis scambus/fuscatus", taxon := "Baetis"]
bio[taxon == "Capniidae/Leuctridae", taxon := "Plecoptera"]
bio[taxon == "Limoniidae/Pediciidae", taxon := "Diptera"]
bio[taxon == "Radix labiata/balthica", taxon := "Radix"]


bio <- bio[abundance != 0]
bio[, abundance := sum(abundance), by = c("sample_id", "taxon")]
bio <- unique(bio, by = c("sample_id", "taxon"))

data <- sites[bio, on = "sample_id"]
data[date == "43027", date := "19.10.2017"]
data[date == "43034", date := "26.10.2017"]
data[date == "43035", date := "27.10.2017"]
data[, date2 := dmy(date)]
data <- data[, date := NULL]

data%<>%rename(original_site_name = station,
               date = date2,
               x.coord = longitude,
               y.coord = latitude)

data[, c("data.set", "EPSG") := .("monitoring_norway", 4326)]

TU <- sort(unique(data$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

# TAXONOMY --------------------------------------------------------------------------

fill_new_table <- character(length(TU))

taxontable_new <-
        data.table::data.table(
                original_name = TU,
                species  = fill_new_table,
                genus    = fill_new_table,
                family   = fill_new_table,
                order    = fill_new_table,
                subclass = fill_new_table,
                class    = fill_new_table,
                phylum   = fill_new_table,
                kingdom  = fill_new_table,
                clean    = FALSE
        )

taxontable <- data.table::rbindlist(list(taxontable, taxontable_new))

for (i in seq_along(TU)){
        
        #- skip this iteration of the loop if the focal taxon has already been evaluated.
        if (taxontable[original_name == TU[i], clean]) next()
        
        i.co <-
                taxize::classification(TU[i], db = "gbif") |>
                {\(x) x[[1]]}()
        
        # skip this iteration of the taxon is not found
        if (is.na(i.co)) next()
        #- assign taxon levels
        taxontable[original_name == TU[i], species  := ifelse("species"  %in% i.co$rank, i.co$name[which(i.co$rank == "species")], NA)]
        taxontable[original_name == TU[i], genus    := ifelse("genus"    %in% i.co$rank, i.co$name[which(i.co$rank == "genus")], NA)]
        taxontable[original_name == TU[i], family   := ifelse("family"   %in% i.co$rank, i.co$name[which(i.co$rank == "family")], NA)]
        taxontable[original_name == TU[i], order    := ifelse("order"    %in% i.co$rank, i.co$name[which(i.co$rank == "order")], NA)]
        taxontable[original_name == TU[i], subclass := ifelse("subclass" %in% i.co$rank, i.co$name[which(i.co$rank == "subclass")], NA)]
        taxontable[original_name == TU[i], class    := ifelse("class"    %in% i.co$rank, i.co$name[which(i.co$rank == "class")], NA)]
        taxontable[original_name == TU[i], phylum   := ifelse("phylum"   %in% i.co$rank, i.co$name[which(i.co$rank == "phylum")], NA)]
        taxontable[original_name == TU[i], kingdom  := ifelse("kingdom"  %in% i.co$rank, i.co$name[which(i.co$rank == "kingdom")], NA)]
        taxontable[original_name == TU[i], clean := TRUE]
        rm(i.co)
        
}

taxontable[clean == FALSE]

# COMBINE DATA SETS -----------------------------------------------------------------
names(data)[which(names(data) == "taxon")] <- "original_name"
data3 <- taxontable[data, on = "original_name"]

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_norway")]

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

data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/monitoring_norway/auxilliary/01_",Sys.Date(),"_data_before:typologies.rds"))

# SITES -----------------------------------------------------------------------------
data5 <- jjmisc::add_typologies(data4)
data5[,least.impacted := T]
saveRDS(data5, paste0("data/01_original_data/monitoring_norway/",Sys.Date(),"_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------
data5[,uniqueN(gr_sample_id), by = c("original_site_name")]
        

# SUMMARY STATISTICS ------------------------------------------------------------------------
uniqueN(data5$gr_sample_id)
summary(data5$year)

# samples 
# one factor 
data5 |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(least.impacted)  |> pull(gr_sample_id) |> uniqueN()
# two factor 
data5 |> filter(least.impacted) |> filter(distance <= 500) |> pull(gr_sample_id) |> uniqueN()



