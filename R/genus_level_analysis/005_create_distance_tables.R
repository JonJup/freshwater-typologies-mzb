### ---------------------------------------------- ###
### -------- Compute distance matrices ----------- ### 
### ---------------------------------------------- ###

# ____________________________
# date created: 29.04.22
# date last modified: 29.04.22
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: Compute distance tables
#___________________________


# setup -----------------------------------------------------------------------------
library(pacman)
p_load(
        parallelDist,
        data.table,
        tidyr,
        dplyr,
        rstudioapi
)

x<-getActiveDocumentContext()
sink(file = paste0("R/STOTEN-reivew/005","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
data.all <- readRDS("data/review/03_2022-04-29_genus_only_common_types.rds")

# prepare data ----------------------------------------------------------------------
#- extract seasons into indivudal objects from list "data.all" 
data.spring <- setDT(data.all[[1]]) %>% rename(ife = illies)
data.summer <- setDT(data.all[[2]]) %>% rename(ife = illies)
data.autumn <- setDT(data.all[[3]]) %>% rename(ife = illies)

data.spring <- data.spring[, c("brt12", "bgr", "ife", "gr_sample_id", "genus", "relative_abundance", "least.impacted")]
data.summer <- data.summer[, c("brt12", "bgr", "ife", "gr_sample_id", "genus", "relative_abundance", "least.impacted")]
data.autumn <- data.autumn[, c("brt12", "bgr", "ife", "gr_sample_id", "genus", "relative_abundance", "least.impacted")]

data.spring <- unique(data.spring, by = c("gr_sample_id", "genus"))
data.summer <- unique(data.summer, by = c("gr_sample_id", "genus"))
data.autumn <- unique(data.autumn, by = c("gr_sample_id", "genus"))

sites.spring <- unique(data.spring, by = "gr_sample_id") 
sites.summer <- unique(data.summer, by = "gr_sample_id") 
sites.autumn <- unique(data.autumn, by = "gr_sample_id") 

data.spring2 <-  pivot_wider(data.spring, id_cols = "gr_sample_id", names_from = "genus", values_from = "relative_abundance", values_fill = 0)
data.summer2 <-  pivot_wider(data.summer, id_cols = "gr_sample_id", names_from = "genus", values_from = "relative_abundance", values_fill = 0)
data.autumn2 <-  pivot_wider(data.autumn, id_cols = "gr_sample_id", names_from = "genus", values_from = "relative_abundance", values_fill = 0)

id1 <- data.spring2    |> select("gr_sample_id") |> left_join(sites.spring) |> select(brt12, bgr, ife) |> rename(brt = brt12)
id2 <- data.summer2    |> select("gr_sample_id") |> left_join(sites.summer) |> select(brt12, bgr, ife) |> rename(brt = brt12)
id3 <- data.autumn2    |> select("gr_sample_id") |> left_join(sites.autumn) |> select(brt12, bgr, ife) |> rename(brt = brt12)

ids <- list(sp.i = id1, 
            su.i = id2, 
            au.i = id3
            )

# compute distance matrices ---------------------------------------------------------
spring.dist <- parallelDist(as.matrix(data.spring2[,-1]), method = "bray", threads = 8)
summer.dist <- parallelDist(as.matrix(data.summer2[,-1]), method = "bray", threads = 8)
autumn.dist <- parallelDist(as.matrix(data.autumn2[,-1]), method = "bray", threads = 8)

# save to file ----------------------------------------------------------------------
#- community data 
saveRDS(data.spring2, paste0("data/review/",Sys.Date(), "_community_data_spring.rds"))
saveRDS(data.summer2, paste0("data/review/",Sys.Date(), "_community_data_summer.rds"))
saveRDS(data.autumn2, paste0("data/review/",Sys.Date(), "_community_data_autumn.rds"))

#- distance matrices 
saveRDS(spring.dist, paste0("data/review/", Sys.Date(), "_distance_spring.rds"))
saveRDS(summer.dist, paste0("data/review/", Sys.Date(), "_distance_summer.rds"))
saveRDS(autumn.dist, paste0("data/review/", Sys.Date(), "_distance_autumn.rds"))
#- ids 
saveRDS(ids, paste0("data/review/",Sys.Date(),"_distance_ids.rds")) 
