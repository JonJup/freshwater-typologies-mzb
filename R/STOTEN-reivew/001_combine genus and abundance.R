# ------------------------- #
# --- Combine data sets for genus level evaluation --- # 
# ------------------------- #

# - # - # - #
# date created: 27.04.22
# date last modified: 27.04.22
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: Combine data sets for genus level abundance data
# - # - # - #

# SETUP -----------------------------------------------------------------------------
library(pacman)

p_load(data.table, dplyr, fs, ggplot2, ggrepel, magrittr, sf, stringr, tmap, rstudioapi)

x<-getActiveDocumentContext()
sink(file = paste0("R/STOTEN-reivew/001","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# LOAD DATA -------------------------------------------------------------------------
#- list of all data sets 
data.sets <- dir_ls("data/01_original_data", type = "directory", regexp = "pre_", invert = TRUE)
# - least impacted 
li <- st_read("data/lemm_least_impacted.gpkg")

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
}


# remove PA data sets ---------------------------------------------------------------
data <- data[-6]
data <- data[-6]
data <- data[-9]
data <- data[-9]
data <- data[-9]
data <- data[-16]

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
data2[, c("sampling.events", "bgr", "ife", "richness", "brtXife", "brtXbgr") := NULL]
data2[data.set == "monitoring_norway", least.impacted := T]
#- Remove data from catchments that are missing in the data from Lemm et al. 2021
data2 <- data2[!is.na(least.impacted)]
data2 <- data2[distance <= 500]

# add new least impacted ------------------------------------------------------------
data2[, least.impacted.old := least.impacted]
data2[, least.impacted := NULL]

li %<>% st_transform(crs = "EPSG:3035")
sites <- data2 |>  
        unique(by = "gr_sample_id") |> 
        st_as_sf()

sites2 <- st_join(sites, 
        li)

# - saved for visual checking in QGIS - was ok. 
#st_write(sites2, "~/../Desktop/test.gpkg")
sites2 %<>% select(gr_sample_id, least.impacted)
sites2 %<>% st_drop_geometry()
setDT(sites2)

data2 <- sites2[data2, on  = "gr_sample_id"]

# sum(sites2$least.impacted, na.rm = T)
# sum(sites2$least.impacted.old, na.rm = T)
# table(sites2$least.impacted, sites2$least.impacted.old)

data2 <- data2[least.impacted == TRUE]

# set to core taxa ------------------------------------------------------------------
focal.phyla <- c("Arthropoda", "Mollusca")
focal.class <- c("Insecta", "Gastropoda", "Bivalvia", "Malacostraca", "Arachnida", NA)
focal.order <- c(
        "Isopoda",
        "Littorinimorpha",
        "Odonata",
        "Sphaeriida",
        "Hemiptera",
        "Amphipoda",
        "Coleoptera",
        "Trichoptera",
        "Diptera",
        "Ephemeroptera",
        NA,
        "Plecoptera"
)
terrestrial.families <- c("Achilidae", "Piersigiidae", "Teutoniidae",
                          "Therevidae", "Rhagionidae", "Anthomyiidae", 
                          "Curculionidae", "Cylindrotomidae")
data2 <- data2[phylum %in% focal.phyla]
data2 <- data2[class %in% focal.class]
data2 <- data2[order %in% focal.order]
data2 <- data2[!family %in% terrestrial.families]



data2[,row_per_data_set := .N, by = "data.set"]
data2[,perc_spe := sum(is.na(species)), by = "data.set"]
data2[,perc_gen := sum(is.na(genus)), by = "data.set"]
data2[,perc_fam := sum(is.na(family)), by = "data.set"]
data2[,perc_spe := perc_spe/row_per_data_set]
data2[,perc_gen := perc_gen/row_per_data_set]
data2[,perc_fam := perc_fam/row_per_data_set]

data2[data.set == "aqem_romania", data.set := "Project AQEM (Romania)"]
data2[data.set == "AQEM_sweden" , data.set := "Project AQEM (Sweden)"]
data2[data.set == "biodrought"  , data.set := "Project Biodrought"]
data2[data.set == "cantabria", data.set := "Cantabria"]
data2[data.set == "ebro_hydrographic_confederation", data.set := "Ebro Hydrographic Confederation"]
data2[data.set == "ecosurv", data.set := "Project Ecosurv"]
data2[data.set == "kaisa-leena_huttunen", data.set := "Koutajoki"]
data2[data.set == "monitoring data from the Netherlands", data.set := "Monitoring data from the Netherlands"]
data2[data.set == "monitoring_czech2", data.set := "Monitoring data from the Czech Republic"]
data2[data.set == "salamander", data.set := "Monitoring data from the Czech Republic"]
data2[data.set == "monitoring_finnland", data.set := "Monitoring data from Finland"]
data2[data.set == "monitoring_greece", data.set := "Monitoring data from Greece"]
data2[data.set == "monitoring_poland", data.set := "Monitoring data from Poland"]
data2[data.set == "monitoring_protugal", data.set := "Monitoring data from Portugal"]
data2[data.set == "protugal19", data.set := "Monitoring data from Portugal"]
data2[data.set == "monitoring_romania", data.set := "Monitoring data from Romania"]
data2[data.set == "monitoring_uk", data.set := "Monitoring data from the UK"]
data2[data.set == "naiades", data.set := "Monitoring France (Naiades)"]
data2[data.set == "rcs", data.set := "Monitoring France (RCS Network)"]
data2[data.set == "picos_de_europa", data.set := "Picos de Europa"]
data2[data.set == "segura_basin", data.set := "Segura Basin"]
data2[data.set == "star", data.set := "Project STAR"]
data2[data.set == "wiser", data.set := "Project WISER"]
data2[data.set == "monitoring_norway", data.set := "Monitoring data from Norway"]

## Species are missing from 28 (biodrought, Slovakia, Germany) to 100 % percent of samples
data2[, c("data.set", "perc_spe", "perc_gen", "perc_fam")] |> 
        unique(by = "data.set") |> 
        tidyr::pivot_longer(cols = !data.set) |> 
        ggplot(aes(y = value, x = name, group = data.set, col = data.set)) + 
        geom_line()

## missing genus mean value 
data2[, c("data.set", "perc_spe", "perc_gen", "perc_fam")] |> 
        filter(!data.set %in% c("Monitorig data from Spain", "Ebro Hydrographic Confederation", "Monitoring data from Portugal")) |> 
        unique(by = "data.set") |> 
        {\(x) mean(x$perc_gen)}()

# data.sets with very high loss
data2 <- data2[!data.set %in% c("Monitorig data from Spain", "Monitoring data from Portugal", "Ebro Hydrographic Confederation")]

data2 <- data2[!is.na(genus)]

#- add Illies freshwater ecoregion 
sites <- unique(data2, by = "gr_sample_id") |> st_as_sf()
sites <- unique(data2, by = c("data.set", "site_id")) |> st_as_sf()
mapview::mapview(sites)



# --- Harmonize Taxonomy --- # 

#- Here I want to make sure that the taxonomy is harmonized. The taxontable is constantly 
#- evolving so potentially errors can occur if data sets are combined with different
#- versions of the taxontable. To avoid this, I join the data with the most recent version
#- of the taxontable here again. 

#- Load taxontable and drop "clean" variable 
taxontable <- readRDS("data/01_original_data/2021-10-14_taxontable.rds")
taxontable[, clean := NULL]

#- Drop taxon variables except "original_name"
data2 %<>% select( - (species:kingdom))
#- Join data and taxontable
data2 <- taxontable[data2, on = "original_name"]



# set to genus level ----------------------------------------------------------------
data2[, abundance := sum(abundance), by = c("genus", "gr_sample_id")]
data2 <- unique(data2, by = c("genus", "gr_sample_id"))
data3 <- copy(data2)
data3 %<>% select(-c(1:2))
data3 %<>% select(-c(family:kingdom))
data3 %<>% select(-lowest.taxon)

# abundance to relative abundances --------------------------------------------------
data3[, total_site_abundance := sum(abundance), by = "gr_sample_id"]
data3[, relative_abundance   := abundance/total_site_abundance]


# --- Seasons --- # 
data4 <- list(spring = data3[season == "spring"], 
              summer = data3[season == "summer"], 
              autumn = data3[season == "autumn"])

# remove rare genera ----------------------------------------------------------------

#- often does which taxon occur? 
genus.table.sp <- data4[[1]] |> pull(genus) |> table() |> sort()
genus.table.su <- data4[[2]] |> pull(genus) |> table() |> sort()
genus.table.au <- data4[[3]] |> pull(genus) |> table() |> sort()

#- cutoff is one percent of samples 
cutoff.sp <- round(data4[[1]][, uniqueN(gr_sample_id)/100])
cutoff.su <- round(data4[[2]][, uniqueN(gr_sample_id)/100])
cutoff.au <- round(data4[[3]][, uniqueN(gr_sample_id)/100])

rare.genus.sp <- names(which(genus.table.sp < cutoff.sp))
rare.genus.su <- names(which(genus.table.su < cutoff.su))
rare.genus.au <- names(which(genus.table.au < cutoff.au))

#- remove rare taxa 
data4[[1]] <- data4[[1]][! genus %in% rare.genus.sp]
data4[[2]] <- data4[[2]][! genus %in% rare.genus.su]
data4[[3]] <- data4[[3]][! genus %in% rare.genus.au]

#- how many genera remain?
uniqueN(data4[[1]]$genus)
uniqueN(data4[[2]]$genus)
uniqueN(data4[[3]]$genus)


# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data4, paste0("data/review/01_",Sys.Date(),"_combined_data_genus_realtive_abundance.rds"))

