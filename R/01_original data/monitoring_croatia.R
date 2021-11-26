# ----------------------------------------------------- #
# -------- Clean Croatian Monitoring data ------------- # 
# ----------------------------------------------------- #


# --------------------------------------------------------------------------------------------------------
# date created      : 13-10-21
# date last modified: 14-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the Croatian monitoring data provided by Marko Milisa
# Temporal aggregation:  not necessary
# EPSG: 3765 (HTRS96 / Croatia TM)
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
#source("R/fill_taxon_table.R")
source("R/get_last_date.r")
source("R/temoral_aggregation_function.R")
source("R/add_typologies.R")
source("R/update_taxonomy.R")



# load data -------------------------------------------------------------------------

bio  <- read_excel("data/01_original_data/monitoring_croatia/raw_data/Copy of Baza MZB MED GIG.xlsx") 
sites  <- read_excel("data/01_original_data/monitoring_croatia/raw_data/MED gig geo.xlsx") 
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")


# prepare data ----------------------------------------------------------------------


bio2 <- 
  bio |> 
  select(-ID_ART) |>
  pivot_longer(cols = 2:52) |> 
  filter(value != 0) |> 
  mutate(value = 1) |> 
  mutate(name = str_remove_all(name, "^[:digit:]+")) |> 
  mutate(name = str_remove_all(name, "^\\.\\ ")) |> 
  setDT()
  

sites2 <- 
  sites |>
  select(c("MJERNA POSTAJA", "X HTRS", "Y HTRS")) |>
  rename(original_site_name = "MJERNA POSTAJA",
         X = "X HTRS",
         Y = "Y HTRS") |>
  mutate(EPSG = 3765) |>
  filter (!is.na(X) & !is.na(Y)) 

#sites3 <- st_as_sf(sites2, coords = c(2:3), crs = 3765)
sites3 = sites2
setDT(sites3)

dates <- 
  sites |> 
  select(c("...17", "...18")) |> 
  rename(original_site_name = "...17", 
         date               = "...18") |> 
  filter(!is.na(date)) |> 
    mutate(date = ifelse(date == "42933", "17.07.2017", date)) |> 
    mutate(date = ifelse(date == "42935", "19.07.2017", date)) |> 
  slice(2:n()) |> 
  mutate(date = lubridate::dmy(date)) |> 
  setDT()


setorderv(sites3, "original_site_name")

sites3[,site_id := 1:.N]
dat.u <- dates$original_site_name |> unique()
dates[original_site_name == dat.u[1], site_id := 4]
dates[original_site_name == dat.u[2], site_id := 19]
dates[original_site_name == dat.u[3], site_id := 12]
dates[original_site_name == dat.u[4], site_id := 6]
dates[original_site_name == dat.u[5], site_id := 11]
dates[original_site_name == dat.u[6], site_id := 18]
dates[original_site_name == dat.u[7], site_id := 5]
dates[original_site_name == dat.u[8], site_id := 22]
dates[original_site_name == dat.u[9], site_id := 1]
dates[original_site_name == dat.u[10], site_id := 20]
dates[original_site_name == dat.u[11], site_id := 14]
dates[original_site_name == dat.u[12], site_id := 7]
dates[original_site_name == dat.u[13], site_id := 8]
dates[original_site_name == dat.u[14], site_id := 10]
dates[original_site_name == dat.u[15], site_id := 21]
dates[original_site_name == dat.u[16], site_id := 23]
dates[original_site_name == dat.u[17], site_id := 13]
dates[original_site_name == dat.u[18], site_id := 24]
dates[original_site_name == dat.u[19], site_id := 9]
dates[original_site_name == dat.u[20], site_id := 15]
dates[original_site_name == dat.u[21], site_id := 16]
dates[original_site_name == dat.u[22], site_id := 66]
dates[original_site_name == dat.u[23], site_id := 25]
dates[original_site_name == dat.u[24], site_id := 69]
dates[original_site_name == dat.u[25], site_id := 62]
dates[original_site_name == dat.u[26], site_id := 49]
dates[original_site_name == dat.u[27], site_id := 65]
dates[original_site_name == dat.u[28], site_id := 51]
dates[original_site_name == dat.u[29], site_id := 45]
dates[original_site_name == dat.u[30], site_id := 33]
dates[original_site_name == dat.u[31], site_id := 60]
dates[original_site_name == dat.u[32], site_id := 58]
dates[original_site_name == dat.u[33], site_id := 55]
dates[original_site_name == dat.u[34], site_id := 56]
dates[original_site_name == dat.u[35], site_id := 28]
dates[original_site_name == dat.u[36], site_id := 57]
dates[original_site_name == dat.u[37], site_id := 63]
dates[original_site_name == dat.u[38], site_id := 64]
dates[original_site_name == dat.u[39], site_id := 27]
dates[original_site_name == dat.u[40], site_id := 26]
dates[original_site_name == dat.u[41], site_id := 48]
dates[original_site_name == dat.u[42], site_id := 29]
dates[original_site_name == dat.u[43], site_id := 46]
dates[original_site_name == dat.u[44], site_id := 30]
dates[original_site_name == dat.u[45], site_id := 41]
dates[original_site_name == dat.u[46], site_id := 39]
dates[original_site_name == dat.u[47], site_id := 42]
dates[original_site_name == dat.u[48], site_id := 35]
dates[original_site_name == dat.u[49], site_id := 44]
dates[original_site_name == dat.u[50], site_id := 38]
dates[original_site_name == dat.u[51], site_id := 32]
dat.u[42]
sites3[!site_id %in% dates$site_id]


sites4 <- sites3 |> 
  left_join(select(dates, - original_site_name), by = "site_id")


sites5 <- filter(sites4, !is.na(date))

bu <- unique(bio2$name)

bio2[name == bu[1], site_id := 19]
bio2[name == bu[2], site_id := 66]
bio2[name == bu[3], site_id := 51]
bio2[name == bu[4], site_id := 65]
bio2[name == bu[5], site_id := 46]
bio2[name == bu[6], site_id := 1]
bio2[name == bu[7], site_id := 69]
bio2[name == bu[8], site_id := 45]
bio2[name == bu[9], site_id := 58]
bio2[name == bu[10], site_id := 29]
bio2[name == bu[11], site_id := 44]
bio2[name == bu[12], site_id := 32]
bio2[name == bu[13], site_id := 62]
bio2[name == bu[14], site_id := 60]
bio2[name == bu[15], site_id := 64]
bio2[name == bu[16], site_id := 38]
bio2[name == bu[17], site_id := 11]
bio2[name == bu[18], site_id := 5]
bio2[name == bu[19], site_id := 14]
bio2[name == bu[20], site_id := 8]
bio2[name == bu[21], site_id := 55]
bio2[name == bu[22], site_id := 56]
bio2[name == bu[23], site_id := 28]
bio2[name == bu[24], site_id := 57]
bio2[name == bu[25], site_id := 26]
bio2[name == bu[26], site_id := 12]
bio2[name == bu[27], site_id := 18]
bio2[name == bu[28], site_id := 20]
bio2[name == bu[29], site_id := 10]
bio2[name == bu[30], site_id := 21]
bio2[name == bu[31], site_id := 23]
bio2[name == bu[32], site_id := 24]
bio2[name == bu[33], site_id := 9]
bio2[name == bu[34], site_id := 15]
bio2[name == bu[35], site_id := 16]
bio2[name == bu[36], site_id := 25]
bio2[name == bu[37], site_id := 49]
bio2[name == bu[38], site_id := 63]
bio2[name == bu[39], site_id := 27]
bio2[name == bu[40], site_id := 3]
bio2[name == bu[41], site_id := 39]
bio2[name == bu[42], site_id := 4]
bio2[name == bu[43], site_id := 22]
bio2[name == bu[44], site_id := 41]
bio2[name == bu[45], site_id := 35]
bio2[name == bu[46], site_id := 7]
bio2[name == bu[47], site_id := 6]
bio2[name == bu[48], site_id := 33]
bio2[name == bu[49], site_id := 48]
bio2[name == bu[50], site_id := 42]

sites5 |> filter(!site_id %in% bio2$site_id)

#- Combine sites with macroinvertebrate samples 
data <- left_join(bio2, 
                  sites5,
                  by = "site_id")
setDT(data)
# The number of rows in data should be the same as in bio2 
nrow(bio2) == nrow(data)

#- Drop columns that are no longer required: name and site_id
data[, c("name", "site_id") := NULL]
data[, data.set := "Monitoring data from Croatia"]

#- add year and season 
data[, `:=` 
      (year = year(date), 
              season = ifelse(month(date) %in% c(12,1,2), "winter", 
                              ifelse(month(date) %in% c(3,4,5), "spring", 
                                     ifelse(month(date) %in% c(6,7,8), "summer", "autumn")
                              )
              )
      )
]
)
data[, taxon := Taxonname]
data[, Taxonnoame := NULL]
data[, taxon := str_remove(taxon, "\\ ssp\\.")]
data[, taxon := str_remove(taxon, "\\ sp\\.")]
data[, taxon := str_remove(taxon, "-Gr\\.")]
data[, taxon := str_remove(taxon, "\\ Gen\\.")]
data[, taxon := str_remove(taxon, "\\ \\(karstic type\\)")]
data[taxon == "Philopotamus variegatus variegatus", taxon := "Philopotamus variegatus"]
data[taxon == "Polycentropus flavomaculatus flavomaculatus", taxon := "Polycentropus flavomaculatus"]
data[taxon == "Tubificidae juv with setae", taxon := "Tubificidae"]

TU <- sort(unique(data$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

taxontable <- update_taxonomy(TU)
saveRDS(taxontable, paste0("data/01_original_data/",Sys.Date(),"_taxontable.rds"))
# COMBINE DATA SETS -----------------------------------------------------------------
#- join taxon information with data 
names(data)[which(names(data) == "taxon")] <- "original_name"
data3 <- taxontable[data, on = "original_name"]

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_croatia")]


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
        abundance = value,
        x.coord = X,
        y.coord = Y,
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
data4 <- data4[!is.na(x.coord)]
#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/monitoring_croatia/auxilliary/02_",Sys.Date(),"_data_before_taxonomy.rds"))

# SITES -----------------------------------------------------------------------------
data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_croatia/",Sys.Date(),"_final_non_aggregated.rds"))

# TEMPORAL AGGREGATION --------------------------------------------------------------
data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)

#---> No aggregation necessary 

# SUMMARY STATISTICS ------------------------------------------------------------------------

summary(data5$year)

#- sites 
uniqueN(data5$site_id)
data5[distance <= 500, uniqueN(site_id)]
data5[least.impacted == TRUE, uniqueN(site_id)]
data5[least.impacted == TRUE & distance <=500, uniqueN(site_id)]

uniqueN(data5$gr_sample_id)


