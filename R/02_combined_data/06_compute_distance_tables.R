### —————————————————————————————————————————————— ###
### ———————— Compute distance matrices ——————————— ### 
### —————————————————————————————————————————————— ###

### ————————————————————
# date created: 09-10-21
# date last modified: 14-10-21
### ————————————————————


# setup -----------------------------------------------------------------------------
library(parallelDist)
library(data.table)
library(tidyr)
library(dplyr)
# load data -------------------------------------------------------------------------
data.all <- readRDS("data/02_combined_data/02_2021-10-14_core_taxa_data_aggregated.rds")

# prepare data ----------------------------------------------------------------------
#- extract seasons into indivudal objects from list "data.all" 
data.spring <- setDT(data.all[[1]])
data.summer <- setDT(data.all[[2]])
data.autumn <- setDT(data.all[[3]])


#- remove rare Illies types
data.spring <- data.spring[!ife %in% c("Baltic province", "Fenno-scandian shield", "Italy and Corsica", "Pyrenees", "Hellenic western Balkan", "Eastern Balkan", "Pontic province", "Dinaric western Balkan")]
data.summer <- data.summer[!ife %in% c("Baltic province", "Italy and Corsica", "Pontic province", "Hellenic western Balkan", "Hungarian lowlands", "The Carpathiens")]
data.autumn <- data.autumn[!ife %in% c("Hellenic western Balkan", "Tundra", "Italy and Corsica", "Eastern Balkan", "Pontic province")]

#- remove rare bgr types (see below)
data.spring <- data.spring[!bgr %in% c("boreal", "pannonian")]
data.summer <- data.summer[!bgr %in% c("pannonian")]
data.autumn <- data.autumn[!bgr %in% c("arctic", "pannonian")]

#- remove entries that are further than 500 meters removed from the next segment. 
data.spring <- data.spring[distance < 500]
data.summer <- data.summer[distance < 500]
data.autumn <- data.autumn[distance < 500]

data.spring <- data.spring[, c("brt12", "bgr", "brtXife", "brtXbgr", "ife", "gr_sample_id", "family", "abundance", "least.impacted")]
data.summer <- data.summer[, c("brt12", "bgr", "brtXife", "brtXbgr", "ife", "gr_sample_id", "family", "abundance", "least.impacted")]
data.autumn <- data.autumn[, c("brt12", "bgr", "brtXife", "brtXbgr", "ife", "gr_sample_id", "family", "abundance", "least.impacted")]

data.spring[, abundance := as.integer(abundance)];data.spring[, abundance := 1]
data.summer[, abundance := as.integer(abundance)];data.summer[, abundance := 1]
data.autumn[, abundance := as.integer(abundance)];data.autumn[, abundance := 1]

data.spring <- unique(data.spring, by = c("gr_sample_id", "family"))
data.summer <- unique(data.summer, by = c("gr_sample_id", "family"))
data.autumn <- unique(data.autumn, by = c("gr_sample_id", "family"))

sites.spring <- unique(data.spring, by = "gr_sample_id") 
sites.summer <- unique(data.summer, by = "gr_sample_id") 
sites.autumn <- unique(data.autumn, by = "gr_sample_id") 

data.spring.li <- data.spring[least.impacted == TRUE]
data.summer.li <- data.summer[least.impacted == TRUE]
data.autumn.li <- data.autumn[least.impacted == TRUE]

data.spring2 <-  pivot_wider(data.spring, id_cols = "gr_sample_id", names_from = "family", values_from = "abundance", values_fill = 0)
data.summer2 <-  pivot_wider(data.summer, id_cols = "gr_sample_id", names_from = "family", values_from = "abundance", values_fill = 0)
data.autumn2 <-  pivot_wider(data.autumn, id_cols = "gr_sample_id", names_from = "family", values_from = "abundance", values_fill = 0)

data.spring2.li <- pivot_wider(data.spring.li, id_cols = "gr_sample_id", names_from = "family", values_from = "abundance", values_fill = 0)
data.summer2.li <- pivot_wider(data.summer.li, id_cols = "gr_sample_id", names_from = "family", values_from = "abundance", values_fill = 0)
data.autumn2.li <- pivot_wider(data.autumn.li, id_cols = "gr_sample_id", names_from = "family", values_from = "abundance", values_fill = 0)


id1 <- data.spring2    |> select("gr_sample_id") |> left_join(sites.spring) |> select(brt12, bgr, ife, brtXbgr, brtXife) |> rename(brt = brt12, bxi = brtXife, bxb = brtXbgr)
id2 <- data.summer2    |> select("gr_sample_id") |> left_join(sites.summer) |> select(brt12, bgr, ife, brtXbgr, brtXife) |> rename(brt = brt12, bxi = brtXife, bxb = brtXbgr)
id3 <- data.autumn2    |> select("gr_sample_id") |> left_join(sites.autumn) |> select(brt12, bgr, ife, brtXbgr, brtXife) |> rename(brt = brt12, bxi = brtXife, bxb = brtXbgr)
id4 <- data.spring2.li |> select("gr_sample_id") |> left_join(sites.spring) |> select(brt12, bgr, ife, brtXbgr, brtXife) |> rename(brt = brt12, bxi = brtXife, bxb = brtXbgr)
id5 <- data.summer2.li |> select("gr_sample_id") |> left_join(sites.summer) |> select(brt12, bgr, ife, brtXbgr, brtXife) |> rename(brt = brt12, bxi = brtXife, bxb = brtXbgr)
id6 <- data.autumn2.li |> select("gr_sample_id") |> left_join(sites.autumn) |> select(brt12, bgr, ife, brtXbgr, brtXife) |> rename(brt = brt12, bxi = brtXife, bxb = brtXbgr)

ids <- list(sp.i = id1, 
            su.i = id2, 
            au.i = id3,
            sp.l = id4,
            su.l = id5,
            au.l = id6)

# compute distance matrices ---------------------------------------------------------
spring.dist <- parallelDist(as.matrix(data.spring2[,-1]), method = "binary", threads = 8)
summer.dist <- parallelDist(as.matrix(data.summer2[,-1]), method = "binary", threads = 8)
autumn.dist <- parallelDist(as.matrix(data.autumn2[,-1]), method = "binary", threads = 8)
spring.dist.li <- parallelDist(as.matrix(data.spring2.li[,-1]), method = "binary", threads = 8)
summer.dist.li <- parallelDist(as.matrix(data.summer2.li[,-1]), method = "binary", threads = 8)
autumn.dist.li <- parallelDist(as.matrix(data.autumn2.li[,-1]), method = "binary", threads = 8)

# save to file ----------------------------------------------------------------------
#- community data 
saveRDS(data.spring2, paste0("data/02_combined_data/",Sys.Date(), "_community_data_spring_impacted.rds"))
saveRDS(data.summer2, paste0("data/02_combined_data/",Sys.Date(), "_community_data_summer_impacted.rds"))
saveRDS(data.autumn2, paste0("data/02_combined_data/",Sys.Date(), "_community_data_autumn_impacted.rds"))
saveRDS(data.spring2.li, paste0("data/02_combined_data/",Sys.Date(), "_community_data_spring_least_impacted.rds"))
saveRDS(data.summer2.li, paste0("data/02_combined_data/",Sys.Date(), "_community_data_summer_least_impacted.rds"))
saveRDS(data.autumn2.li, paste0("data/02_combined_data/",Sys.Date(), "_community_data_autumn_least_impacted.rds"))
#- distance matrices 
saveRDS(spring.dist, paste0("data/02_combined_data/", Sys.Date(), "_distance_spring.rds"))
saveRDS(summer.dist, paste0("data/02_combined_data/", Sys.Date(), "_distance_summer.rds"))
saveRDS(autumn.dist, paste0("data/02_combined_data/", Sys.Date(), "_distance_autumn.rds"))
saveRDS(spring.dist.li, paste0("data/02_combined_data/", Sys.Date(), "_distance_spring_li.rds"))
saveRDS(summer.dist.li, paste0("data/02_combined_data/", Sys.Date(), "_distance_summer_li.rds"))
saveRDS(autumn.dist.li, paste0("data/02_combined_data/", Sys.Date(), "_distance_autumn_li.rds"))
#- ids 
saveRDS(ids, paste0("data/02_combined_data/",Sys.Date(),"_distance_ids.rds")) 
 