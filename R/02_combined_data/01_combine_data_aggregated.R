# ------------------------------------------- #
# --- Combine macroinvertebrate data sets --- # 
# ------------------------------------------- #

#       date created: 29.04.22
# date last modified: 04.05.22
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: Combine temporally aggregated (where necessary) data sets
# Notes: 
#       The raw data are available under:
#       Biogeographical Regions: https://www.eea.europa.eu/data-and-maps/data/biogeographical-regions-europe-3
#       Illies' Freshwater Ecoregions: https://www.eea.europa.eu/data-and-maps/data/ecoregions-for-rivers-and-lakes
#       Base Version of Lemm et al 2021 data: https://zenodo.org/record/4322819

# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, fs, magrittr, sf, stringr, tmap, rstudioapi)

x<-getActiveDocumentContext()
sink(file = paste0("R/02_combined_data/log_files//01","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# LOAD DATA -------------------------------------------------------------------------

illies <- st_read("~/../Downloads/wfd_shp_ecoregions/Ecoregions.shp") |> 
        st_transform(crs = 3035)
bgr    <- st_read("~/../Downloads/BiogeoRegions2016_shapefile/BiogeoRegions2016.shp") |> 
        st_transform(crs = 3035)
lemm   <- st_read("data/lemm_least_impacted.gpkg") |> 
        st_transform(crs = "EPSG:3035")

#- list of all data sets 
data.sets <- dir_ls("data/01_original_data", type = "directory", regexp = "pre_", invert = TRUE)
#- At this point several data sets are omitted from the analysis. 
#- Belgium monitoring: no least impacted sites 
data.sets <- data.sets[- which(str_detect(data.sets, "belgium"))]
#- Ecosurv: no dipterans
data.sets <- data.sets[- which(str_detect(data.sets, "ecosurv"))]
#- missing 6 orders, noticeably it is the only data set missing "Pulmonata" and "Isopoda"
data.sets <- data.sets[- which(str_detect(data.sets, "oscar_belmar"))]
#- Monitoring Romania: No Snails 
data.sets <- data.sets[-which(str_detect(data.sets, "monitoring_romania"))]

data      <- list()

#- loop over all (currently 23) data sets to load them as elements of the list 
#- "data"
for (i in seq_along(data.sets)){
        i.ds <- data.sets[i]
        print(paste("LOADING", str_remove(i.ds, "data/01_original_data/")))
        i.files <- dir_ls(i.ds, regexp = "final_aggregated")
        if(length(i.files) == 0) 
                i.files <- dir_ls(i.ds, regexp = "final_non_aggregated")
        i.x     <- readRDS(i.files)
        data[[i]] <- i.x 
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
};rm(i)

# PREPARE DATA ----------------------------------------------------------------------
#- In these next steps, I apply several functions to all elements of the list "data", 
#- i.e. data sets to ensure that they are harmonized. 

#- Make sure all date variables are formatted as such:
data2    <- lapply(data, function(x) x[, date := as.Date(date)])
#- Remove the newest_date column that was created in an old version of the 
#- newest_sample() function. Not all data sets have this column so warning: 
#- "Column 'newest_date' does not exist to remove" is thrown (currently 
#- for 11 data sets).
data2    <- lapply(data2, function(x) x[, newest_date := NULL])
#- Make data spatial (sf) and transform to common coordinate reference system (LAEA Europe). 
data.st <- lapply(data2, function(x) st_as_sf(x, coords = c("x.coord", "y.coord"), crs = x$EPSG[1]))
data.st <- lapply(data.st, function(x) st_transform(x, crs = 3035))
#- Turn back into data.table to bind rows of list elements 
data2   <- lapply(data.st, setDT)
data2   <- rbindlist(data2, fill = TRUE, use.names = TRUE)
#- Remove unnecessary columns
# add both bgr and ife again 
data2[, c("sampling.events", "bgr", "ife", "least.impacted") := NULL]

# - remove winter samples 
data2[, month := month(date)]
data2 <- data2[!month %in% c(12,1,2)]
# - remaining sites and samples
uniqueN(data2, "gr_sample_id")
uniqueN(data2, c("site_id", "data.set"))
coord <- data2 |> st_as_sf() |> st_coordinates() |>data.frame() |> setDT()
nrow(unique(coord, by = c("X", "Y")))
rm(coord)
#- remove sites with no segment in 500 vicinity

#- Remove data from catchments that are missing in the data from Lemm et al. 2021
data2 <- data2[distance < 500]
uniqueN(data2, "gr_sample_id")
uniqueN(data2, c("site_id", "data.set"))
# coord <- data2 |> st_as_sf() |> st_coordinates() |>data.frame() |> setDT()
# nrow(unique(coord, by = c("X", "Y")))
# rm(coord)

#- add Illies freshwater ecoregion 
sites <- unique(data2, by = c("gr_sample_id")) |> st_as_sf()
illies %<>% select(NAME)
bgr    %<>% select(short_name)

sites_illies <- st_join(sites, illies)
data3 <-
  sites_illies |>
  select(gr_sample_id, NAME) |>
  st_drop_geometry() |>
  rename(ife = NAME) |>
  right_join(data2, by = "gr_sample_id") |>
  setDT()  
sites_bgr <- st_join(sites, bgr)
data4 <-
  sites_bgr |>
  select(gr_sample_id, short_name) |>
  st_drop_geometry() |>
  rename(bgr = short_name) |>
  right_join(data3, by = "gr_sample_id") |>
  setDT()  
sites_lemm <- st_join(sites, lemm)
data4 <-
        sites_lemm |>
        select(gr_sample_id, least.impacted) |>
        st_drop_geometry() |>
        right_join(data4, by = "gr_sample_id") |>
        setDT()  
rm(sites_illies, sites, sites_bgr, sites_lemm, bgr, lemm, illies)


# - remove impaired sites

#- Norway is outside the data set of Lemm et al. 21. According to an email from 
#- Leonard Sandin, who provided the data, on the 01-10-21 all sites are in reference 
#- condition. 
data4[data.set == "monitoring_norway", least.impacted := T]

data4 <- data4[least.impacted == TRUE]
# - remaining sites and samples
uniqueN(data4, "gr_sample_id")
uniqueN(data4, c("site_id", "data.set"))
# coord <- data4 |> st_as_sf() |> st_coordinates() |>data.frame() |> setDT()
# nrow(unique(coord, by = c("X", "Y")))

#- Neither IFE nor brt12 having missing values ... 
data4[is.na(ife)]
data4[is.na(brt12)]
data4[is.na(bgr)]

# --- Harmonize Taxonomy --- # 

#- Here I want to make sure that the taxonomy is harmonized. The taxontable is constantly 
#- evolving so potentially errors can occur if data sets are combined with different
#- versions of the taxontable. To avoid this, I join the data with the most recent version
#- of the taxontable here again. 

#- Load taxontable and drop "clean" variable 
taxontable <- readRDS("data/01_original_data/2022-04-29_taxontable.rds")
taxontable[, clean := NULL]

#- Drop taxon variables except "original_name"
data4 %<>% select( - (species:kingdom))
#- Join data and taxontable
data4 <- taxontable[data4, on = "original_name"]

# --- Fix abundance column --- # 
#- Abundance values are strings, mixed absolute and relative abundances and NAs. 
# -> transform all to PA. Mixed 
data4[, abundance2 := as.numeric(abundance)]
data4[is.na(abundance2), abundance2 := 1]
#- Check that no missing or negative values remain  
if (anyNA(data4$abundance2)) print("missing abundance values")
if (nrow(data4[abundance2 < 0]) != 0) print("negative abundance values")
#- Are there any zero abundance observations left? - Drop them. 
data4[abundance2 == 0]
data4 <- data4[abundance2 != 0]

# --- Last taxonomic fixes --- # 
data4 <- data4[lowest.taxon != "Mollusca"]
data4[species == "", species := NA]
data4[genus == "", genus := NA]
data4[family == "", family := NA]
data4[order == "", order := NA]

# --- adjust data.set names --- #
data4[, sort(unique(data.set))]
data4[data.set == "aqem_romania", data.set := "Project AQEM (Romania)"]
data4[data.set == "AQEM_sweden" , data.set := "Project AQEM (Sweden)"]
data4[data.set == "biodrought"  , data.set := "Project Biodrought"]
data4[data.set == "cantabria", data.set := "Cantabria"]
data4[data.set == "ebro_hydrographic_confederation", data.set := "Ebro Hydrographic Confederation"]
data4[data.set == "ecosurv", data.set := "Project Ecosurv"]
data4[data.set == "kaisa-leena_huttunen", data.set := "Koutajoki"]
data4[data.set == "monitoring data from the Netherlands", data.set := "Monitoring data from the Netherlands"]
#data4[data.set == "monitoring_belgium", data.set := "Monitoring data from Belgium"]
data4[data.set == "monitoring_czech2", data.set := "Monitoring data from the Czech Republic"]
data4[data.set == "salamander", data.set := "Monitoring data from the Czech Republic"]
data4[data.set == "monitoring_finnland", data.set := "Monitoring data from Finland"]
data4[data.set == "monitoring_greece", data.set := "Monitoring data from Greece"]
data4[data.set == "monitoring_poland", data.set := "Monitoring data from Poland"]
data4[data.set == "monitoring_protugal", data.set := "Monitoring data from Portugal"]
data4[data.set == "protugal19", data.set := "Monitoring data from Portugal"]
data4[data.set == "monitoring_romania", data.set := "Monitoring data from Romania"]
data4[data.set == "monitoring_uk", data.set := "Monitoring data from the UK"]
data4[data.set == "naiades", data.set := "Monitoring France (Naiades)"]
data4[data.set == "rcs", data.set := "Monitoring France (RCS Network)"]
data4[data.set == "picos_de_europa", data.set := "Picos de Europa"]
data4[data.set == "segura_basin", data.set := "Segura Basin"]
data4[data.set == "star", data.set := "Project STAR"]
data4[data.set == "wiser", data.set := "Project WISER"]
data4[data.set == "monitoring_norway", data.set := "Monitoring data from Norway"]
unique(data4$data.set)

# --- Seasons --- # 
data4 <- list(spring = data4[season == "spring"], 
              summer = data4[season == "summer"], 
              autumn = data4[season == "autumn"])

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data4, paste0("data/02_combined_data/01_",Sys.Date(),"_combined_data_aggregated.rds"))

