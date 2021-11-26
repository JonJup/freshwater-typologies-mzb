# ———————————————————————————————————————— #
# ——— PREPARE LANDAU DATA FOR PERLODES ——— # 
# ———————————————————————————————————————— #


# ———————————————————————————————————
# date: 
#       21.07.21
# files in: 
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Prepare German monitoring data available on Landau group server for evaluation with Perlodes. 
# ————————————————


# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, magrittr,lubridate, tidyr)

# LOAD DATA -------------------------------------------------------------------------
samples <- samples <- fread("data/original data/landau/raw_data/mzb_samples.csv")
sites   <- sites   <- fread("data/original data/landau/raw_data/mzb_sites.csv")
sites.close <- readRDS("data/original data/landau/auxilliary/07_2021-07-21_data5.rds")

# PREPARE DATA ----------------------------------------------------------------------
sites.close <- sites.close[,c("german_type","original_site_name")]
names(sites.close)[2] <- "site_id"
sites.close <- unique(sites.close, by = "site_id")
samples2    <- samples[site_id %in% sites.close$site_id]

#- 7383 sites with 21888 samples 
uniqueN(samples2$site_id)
uniqueN(samples2$sample_id)

#- only after 2004 (harmonization of European national sampling schemes)
samples2 %<>% 
        mutate(year = year(date)) %>%
        filter(year > 2004 & ind_qm > 0)

#- 7424 sites with 13993 samples 
uniqueN(samples2$site_id)
uniqueN(samples2$sample_id)

#- join with sites.close for german types 
samples2 <- sites.close[samples2, on = 'site_id']

#- some types are not supported by PELODES 
sites2.xlsx <- 
        samples2 |> 
                filter(!german_type %in% c("22.1", "22.2", "NULL", "77"))

sites2.xlsx <-
        sites2.xlsx |>
        select(sample_id, german_type) |>
        rename(Typ = german_type) |>
        mutate(
                Typ =
                        case_when(
                                Typ == "1.1"   ~ "Typ 01.1",
                                Typ == "1.2"   ~ "Typ 01.2",
                                Typ == "2.1"   ~ "Typ 02.1",
                                Typ == "2.2"   ~ "Typ 02.2",
                                Typ == "3.1"   ~ "Typ 03.1",
                                Typ == "3.2"   ~ "Typ 03.2",
                                Typ == "4"   ~ "Typ 04",
                                Typ == "5"   ~ "Typ 05",
                                Typ == "5.1" ~ "Typ 05.1",
                                Typ == "6"   ~ "Typ 06",
                                Typ == "6_K"   ~ "Typ 06_K",
                                Typ == "7"   ~ "Typ 07",
                                Typ == "9"   ~ "Typ 09",
                                Typ == "9.1" ~ "Typ 09.1",
                                Typ == "9.1_K" ~ "Typ 09.1_K",
                                Typ == "9.2" ~ "Typ 09.2",
                                Typ == "10"  ~ "Typ 10",
                                Typ == "11"  ~ "Typ 11",
                                Typ == "12"  ~ "Typ 12",
                                Typ == "14"  ~ "Typ 14",
                                Typ == "15"  ~ "Typ 15",
                                Typ == "15_G"  ~ "Typ 15g",
                                Typ == "16"  ~ "Typ 16",
                                Typ == "17"  ~ "Typ 17",
                                Typ == "18"  ~ "Typ 18",
                                Typ == "19"  ~ "Typ 19",
                                Typ == "20"  ~ "Typ 20",
                                Typ == "21"  ~ "Typ 21",
                                Typ == "21_S"  ~ "Typ 21_S",
                                Typ == "21_N"  ~ "Typ 21_N",
                                Typ == "22.2"  ~ "Typ 22.2",
                                Typ == "23"  ~ "Typ 23",
                        )
        ) |>
        unique(by = "sample_id") 


#- we have 13883 sites 
uniqueN(sites2.xlsx$sample_id)
site.group.beg <- seq(from = 1, to = 13883, by = 500)
site.group.end <- site.group.beg - 1
site.group.end <- site.group.end[-1]
site.group.end[length(site.group.end) + 1]  <- 13883

sites.list   <- lapply(seq_along(site.group.beg), function(x) sites2.xlsx[site.group.beg[x]:site.group.end[x]])
samples3     <- select(samples2, sample_id, aqem_id, taxon_aqem, ind_qm)  
samples.list <- lapply(seq_along(site.group.beg), function(x) samples3[sample_id %in% sites.list[[x]]$sample_id] )
samples.list <- lapply(samples.list, pivot_wider, names_from = "sample_id", values_fill = 0, values_from = "ind_qm", values_fn = sum)
#- drop taxa that do not occur in subset
samples.list <- lapply(seq_along(samples.list), function(x) samples.list[[x]][which(rowSums(samples.list[[x]][,-c(1,2)]) > 0), ])

saveRDS(samples.list, "data/temp/samples_list.rds")
saveRDS(sites.list, "data/temp/sites_list.rds")
   
samples.list <- readRDS("data/temp/samples_list.rds")
sites.list   <- readRDS("data/temp/sites_list.rds") 

for (i in seq_along(samples.list)){
        if(i %in% c(1:25)) next()
        print(i)
        xlsx::write.xlsx(samples.list[[i]],    paste0("data/original data/landau/auxilliary/perlodes/11_",i, "_", Sys.Date(),"perlodes_taxa1.xlsx"))
        xlsx::write.xlsx(sites.list[[i]],      paste0("data/original data/landau/auxilliary/perlodes/11_",i, "_", Sys.Date(),"perlodes_taxa2.xlsx"))
}

# SAVE TO FILE ----------------------------------------------------------------------

# fwrite(samples3,    paste0("data/original data/landau/auxilliary/perlodes/11_",Sys.Date(),"perlodes_taxa1.csv"))
# fwrite(sites2.xlsx, paste0("data/original data/landau/auxilliary/perlodes/11_",Sys.Date(),"perlodes_taxa2.csv"))

# FORMAT RESULTS --------------------------------------------------------------------
library(readxl)
library(data.table)
library(dplyr)
res.file <- fs::dir_ls("data/original data/landau/auxilliary/perlodes/result")

res.file <- lapply(seq_along(res.file), function(x) read_excel(res.file[x], sheet = 3))
res.file <- lapply(res.file, function(x) select(x, Probe, ÖZK))
res.file <- rbindlist(res.file)

saveRDS(res.file, "data/original data/landau/auxilliary/perlodes_results.rds")
