### ---------------------------------- ###
### -------- Type coverage ----------- ### 
### ---------------------------------- ###

# ____________________________
# date created: 27.04.22
# date last modified: 29.04.22
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: Count number of samples per type
#___________________________

# set up  ---------------------------------------------------------------------------
library(pacman)
p_load(
        data.table,
        sf,
        mapview,
        dplyr,
        tmap,
        rstudioapi,
        magrittr
)

x<-getActiveDocumentContext()
sink(file = paste0("R/STOTEN-reivew/004","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
data <- readRDS("data/review/02_2022-04-28_genus_w_added_types.rds")

# prepare data -----------------------------------------------------------------------------
sites <- lapply(data, function(x) unique(x, by = "gr_sample_id"))

sites%<>%lapply(function(x) rename(x, ife = illies))

# numbers ---------------------------------------------------------------------------
## -- Broad River Types 
sites[[1]]$brt12 |>  table()
sites[[2]]$brt12 |>  table()
sites[[3]]$brt12 |>  table()

sites[[1]]%<>%filter(!brt12 %in% c("RT11", "RT12"))
sites[[3]]%<>%filter(!brt12 %in% c("RT12"))

## -- Illies 
sites[[1]]$ife |>  table()
sites[[2]]$ife |>  table() |> min()
sites[[3]]$ife |>  table() |> min()

sites[[1]]%<>%filter(!ife %in% c("Hungarian lowlands", "Pontic province", "Pyrenees", "The Carpathiens", "Fenno-scandian shield"))
sites[[2]]%<>%filter(!ife %in% c("Borealic uplands", "Pontic province", "Pyrenees", "The Carpathiens", "Hungarian lowlands"))
sites[[3]]%<>%filter(!ife %in% c("Pontic province", "The Carpathiens", "Hungarian lowlands", "Italy and Corsica"))

## -- bgr 
sites[[1]]$bgr |>  table() |> sort()
sites[[2]]$bgr |>  table() |> sort()
sites[[3]]$bgr |>  table() |> sort()

sites[[1]]%<>%filter(!ife %in% c("mediterranean", "boreal"))

data[[1]] <- data[[1]][gr_sample_id %in% sites[[1]]$gr_sample_id]
data[[2]] <- data[[2]][gr_sample_id %in% sites[[2]]$gr_sample_id]
data[[3]] <- data[[3]][gr_sample_id %in% sites[[3]]$gr_sample_id]


# save to file ----------------------------------------------------------------------
saveRDS(data, paste0("data/review/03_", Sys.Date(), "_genus_only_common_types.rds"))

