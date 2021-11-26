# ——————————————————————————————————— #
# ——— Clean Greek Monitoring data ——— # 
# ——————————————————————————————————— #


# ———————————————————————————————————
# date created: 10.08.21
# date last modified: 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the Greek monitoring data provided by Maria Lazaridou. 
# temporal aggregation: no 
# EPSG: 4326 - WGS89
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

data       <- read_excel("data/original data/monitoring_greece/raw_data/Database_Macroinvertebrate_GR_08.09.2021.xlsx") 
taxontable <- readRDS(paste0("data/original data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA   ---------------------------------------------------------------------

#- transform to data.table 
setDT(data)

#- the first two columns can be discarded 
data <- data[, -c(1,2)]

#- the first two rows can be discarded 
data <- data[-c(1,2),]

#- there are two site columns ('Station' and 'Site Name'). For most entries they are the same. 
data[`Site Name` != Station, c("Site Name", "Station")]
uniqueN(data$`Site Name`) - uniqueN(data$Station)
#- Site Name has 38 more unique entries so I stick to this ID. 

#- season and year 
data[, season := str_to_lower(str_remove(str_extract(Season, ".*_"), "_"))]
data[, year  := as.numeric(str_remove(str_extract(Season, "_.*"), "_"))]
#- coordinates 
data[,x.coord := data$`Coordinates (wgs84)`]
data[,y.coord := data$`...6`]
#- drop variables 
data %<>% select(c(1,9:ncol(data)))

#- create taxon column in long format 
data %<>% pivot_longer(cols = !c("Site Name", "year", "season", "y.coord", "x.coord"), names_to = "taxon", values_to = "abundance")

#- drop absence records 
data %<>% filter(abundance != 0)
#- rename site variable 
data %<>% rename(original_site_name = 'Site Name')
#- add NA date variable 
data %<>% mutate(date = as.Date(NA))
#- add data set variable 
data %<>% mutate(data.set = "monitoring_greece")

#- verify that there is only one x and y coordinate per site name 
setDT(data)
data[, uniqueN(x.coord), by = "original_site_name"] |> pull(V1) |> table()
data[, uniqueN(y.coord), by = "original_site_name"] |> pull(V1) |> table()

#- except for one all are fine. The one special case needs to be fixed. 
data[, uniqueN(x.coord), by = "original_site_name"][V1 == 3]
data[original_site_name == "SYMVOLI" & year == 2010, original_site_name := "SYMVOLI_1"]
data[original_site_name == "SYMVOLI" & year == 2009, original_site_name := "SYMVOLI_2"]
data[original_site_name == "SYMVOLI" & year == 2008, original_site_name := "SYMVOLI_3"]
#- test spatial coordinates 
sites <- 
        unique(data, by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = 4326)

mapview(sites)
#- one site is far removed from all other sites towards the eastern border of Turkey. I will remove it. 
data %<>% filter(original_site_name != "DW_STRATOPEDO")


TU <- sort(unique(data$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

# TAXONOMY --------------------------------------------------------------------------

taxontable <- update_taxonomy(TU)

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_greece")]



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

data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/monitoring_greece/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))
# SITES -----------------------------------------------------------------------------

data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/original data/monitoring_greece/",Sys.Date(),"_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)

#-> no temporal aggregation necessary

# SUMMARY STATISTCS -----------------------------------------------------------------
# - number of sites 
sites2 |> nrow()
sites2[brt_distance <= 500, .N]
sites2[fec.least.impacted == TRUE, .N]
sites2[fec.least.impacted == TRUE & brt_distance <= 500, .N]

data5[, uniqueN(gr_sample_id)]

