# ------------------------------------------------- #
### --- 4.2 Create temporally aggregated data --- ### 
# ------------------------------------------------- #

# --------------- #
# date:
#       29.06.21
# files in: 
#       <-
# files out:
#       ->
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Temporally aggregate data from sites with repeated sampling. 
# --------------- #

# TEST 
# Dont Repeat Yourself  
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# Design by Contract 
# Fail fast/ defensive programming 

# setup -----------------------------------------------
pacman::p_load(data.table, dplyr, lubridate, magrittr, stringr)

# load data -------------------------------------------
data  <-  readRDS("data/04_invertebrates_w_typologies.rds")

# prepare data ----------------------------------------------------------------------
data2 <- copy(data)

#- drop pre 2000 
data2 <- data2[year(date) >= 2000 | is.na(date)]

#- create site.id 
data2[, site.id := str_remove(gr_sample_id, "date_.*_")]
#- manual fixes for the two data sets from Pepe Barquin which are mixed under the current naming scheme 
data2[data.set == "Cantabria_Pepe", site.id := paste0(site.id, "_cantabria")]
data2[data.set == "Picos_Pepe"    , site.id := paste0(site.id, "_picos")]


data2[, n := uniqueN(gr_sample_id), by = "site.id"]

#- create taxon column 
data2[, taxon := ifelse(!is.na(species), species, ifelse(!is.na(genus), genus, ifelse(!is.na(family), family, ifelse(!is.na(order), order, ifelse(!is.na(subclass), subclass, ifelse(!is.na(class), class, ifelse(!is.na(phylum), phylum, "0")))))))]
#- there are some rows without taxonomic information 
data2 <- data2[taxon != "0"]

sts <- unique(data2, by = "site.id")

sts %<>% select(!geometry)

sts |>  
        group_by(data.set) |> 
        #- data sets with only one number of visits per site 
        filter(!data.set %in% c("Cantabria_Pepe", "kaisa-leena_Huttunen", "leonard_sandin", 
                                "mzb_Ecosurv"   , "mzb_STARS"           , "mzb_WISER"     ,
                                "phillipe_usseglio_polaterra", "Picos_Pepe")) |> 
        ggplot(aes(x = n))  + 
        geom_histogram(aes(fill = data.set)) + 
        facet_wrap(.~data.set, scales = "free")

#- Rivpacs has three samples despite not having a date column because of the seasons. 

sts   |> filter(data.set == "Cantabria_Pepe")
data2 |> select(!geometry) |> filter(site.id == "site_00001_Pepe") |> pull(gr_sample_id) |> unique()
data2 |> select(!geometry) |> filter(data.set == "Cantabria_Pepe") |> pull(date) |> unique()
data2 |> select(!geometry) |> filter(data.set == "rivpacs") |> pull(gr_sample_id) |> unique() |> sort()
data2 |> select(!geometry) |> filter(data.set == "rivpacs")
# aggregate  -------------------------------------------------

for (i in 1:nrow(sts)){
        if (i == 1) 
                new.lst <- list()
        if (i %% 250 == 0) 
                print(i)
        if (sts$n[i] == 1) {
                new.lst[[i]] <- data2[site.id == sts$site.id[i]]
                next()
        }
        lp.sub  <- data2[site.id == sts$site.id[i]]
        lp.n    <- sts$n[i]
        lp.tab  <- table(lp.sub$taxon)
        lp.tab2 <- lp.tab/lp.n
        lp.sub2 <- lp.sub[taxon %in%  names(lp.tab2)[lp.tab2 >= 0.5]]
        lp.sub2[, gr_sample_id := paste0(site.id, "_combined")]
        lp.sub2[, date := NA]
        lp.sub2 <- unique(lp.sub2, by = "taxon")
        new.lst[[i]] <- lp.sub2
        rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
        rm(i)
        gc()
}

data3 <- rbindlist(new.lst)
data3[, c("taxon", "site.id", "n") := NULL]


# save to file ----------------------------------------------------------------------
saveRDS(data3, file = "data/04_invertebrates_w_typologies_temporal_aggergated.rds")

