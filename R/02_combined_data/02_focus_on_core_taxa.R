# ——————————————————————————— #
# ——— Focus on core taxa  ——— # 
# ——————————————————————————— #

# ————————————————
#  date created: 26-07-21
# last modified: 14-10-21
#       Project: Evaluating European Broad River Types for Macroinvertebrates
#       Purpose: Subset the combined data to focal phyla, classes and orders. 
# ————————————————


# SETUP -----------------------------------------------------------------------------
library(data.table)
library(fs)
library(stringr)
library(lubridate)
library(dplyr)
library(sf)


# LOAD DATA  -------------------------------------------
data <- readRDS("data/02_combined_data/01_2021-10-14_combined_data_aggregated.rds")

#- select focal taxa
focal.phyla <- c("Arthropoda", "Mollusca")
focal.class <- c("Insecta", "Gastropoda", "Bivalvia", "Malacostraca", "Arachnida", NA)
focal.order <- c(#"Trombidiformes",
                 #"Megaloptera",
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

data <- lapply(seq_along(data), function(x) data[[x]][phylum %in% focal.phyla])
data <- lapply(seq_along(data), function(x) data[[x]][class %in% focal.class])
data <- lapply(seq_along(data), function(x) data[[x]][order %in% focal.order])
data <- lapply(seq_along(data), function(x) data[[x]][!family %in% terrestrial.families])

#- also drop observation with less than family-level resolution 
data <- lapply(seq_along(data), function(x) data[[x]][!is.na(family)])

# unique(c(data[[1]][order == "Therevidae", sort(unique(family))],
#          data[[2]][order == "Therevidae", sort(unique(family))],
#          data[[3]][order == "Therevidae", sort(unique(family))]))



# data[[2]][family == "Cylindrotomidae", unique(original_name)]
# data[[3]][family == "Cylindrotomidae"] |> unique(by = "gr_sample_id") |> st_as_sf(crs = 3035) |> mapview::mapview()
# data[[1]][order == "Megaloptera"] |> unique(by = "gr_sample_id") |> st_as_sf(crs = 3035) |> mapview::mapview()
# data[[1]][family == "Scytinopteridae"]

rm(list = setdiff(ls(), c("data", "opt")))


# RARE TAXA -------------------------------------------------------------------------
#  ———
#- often does which taxon occur? 
family.table.sp <- data[[1]] |> pull(family) |> table() |> sort()
family.table.su <- data[[2]] |> pull(family) |> table() |> sort()
family.table.au <- data[[3]] |> pull(family) |> table() |> sort()

#- cutoff is one percent of samples 
cutoff.sp <- data[[1]][, uniqueN(gr_sample_id)/100]
cutoff.su <- data[[2]][, uniqueN(gr_sample_id)/100]
cutoff.au <- data[[3]][, uniqueN(gr_sample_id)/100]

rare.family.sp <- names(which(family.table.sp < cutoff.sp))
rare.family.su <- names(which(family.table.su < cutoff.su))
rare.family.au <- names(which(family.table.au < cutoff.au))

#- remove rare taxa 
data[[1]] <- data[[1]][! family %in% rare.family.sp]
data[[2]] <- data[[2]][! family %in% rare.family.su]
data[[3]] <- data[[3]][! family %in% rare.family.au]

#- how many families remain?
uniqueN(data[[1]]$family)
uniqueN(data[[2]]$family)
uniqueN(data[[3]]$family)

#- clean up 
rm(list = ls()[grepl(pattern = "family", x = ls())])
rm(list = ls()[grepl(pattern = "phyla", x = ls())])
rm(list = ls()[grepl(pattern = "class", x = ls())])
rm(list = ls()[grepl(pattern = "order", x = ls())])
rm(list = ls()[grepl(pattern = "cutoff", x = ls())])


# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data, paste0("data/02_combined_data/02_",Sys.Date(),"_core_taxa_data_aggregated.rds"))
        
