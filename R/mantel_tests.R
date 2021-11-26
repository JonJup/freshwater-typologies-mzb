#- Mantel Tests -# 

## This script runs a mantel test on the whole data set. 
## This doesn't make sense. 

# setup -----------------------------------------------------------------------------
#- libraries
pacman::p_load(fs, stringr, lubridate,data.table, tidyr, vegan, dplyr, parallelDist)


# load data -------------------------------------------------------------------------
most.recent.data <- 
        dir_ls("data/combined_data/", regexp = "_core_taxa_data.rds") |> 
        str_remove_all(paste(c("data/combined_data/03_", "_core_taxa_data.rds"), collapse = "|")) |> 
        ymd() |> 
        {\(x) x[which.max(x)]}()

#- read in and combine elements 
data <- readRDS(file = paste0("data/combined_data/03_",most.recent.data,"_core_taxa_data.rds"))
data <- rbindlist(data)

#- extract species and genus level data 
sp  <- data[!is.na(species)]
gn  <- data[!is.na(genus)]


# prepare data ----------------------------------------------------------------------
# ———— PA ———— #  
sp.pa <- copy(sp)
gn.pa <- copy(gn)

sp.pa[, abundance := 1]
gn.pa[, abundance := 1]

sp.pa.s <- 
        sp.pa[, c("species", "gr_sample_id", "abundance")] |> 
        unique(by = c("species", "gr_sample_id")) |> 
        pivot_wider(id_cols = gr_sample_id, names_from = "species", values_from = abundance, values_fill = 0) |> 
        select(!gr_sample_id) |> 
        as.matrix()
#- Compute Jaccard similarity (a/a+b+c)
#- run time: 4:58AM to 
sp.pa.s <- parallelDist(sp.pa.s, method = "binary", cpu = 6)


sp.pa.g <- 
        sp.pa[, c("genus", "gr_sample_id", "abundance")] |> 
        unique(by = c("genus", "gr_sample_id")) |> 
        pivot_wider(id_cols = gr_sample_id, names_from = "genus", values_from = abundance, values_fill = 0) |> 
        select(!gr_sample_id) |> 
        as.matrix()
sp.pa.f <- 
        sp.pa[, c("family", "gr_sample_id", "abundance")] |> 
        unique(by = c("family", "gr_sample_id")) |> 
        pivot_wider(id_cols = gr_sample_id, names_from = "family", values_from = abundance, values_fill = 0) |> 
        select(!gr_sample_id) |> 
        as.matrix()

sp.pas.d <- vegdist(sp.pa.s, method = "jaccard")
sp.pag.d <- vegdist(sp.pa.g, method = "jaccard")
sp.paf.d <- vegdist(sp.pa.f, method = "jaccard")

# Mantel test -----------------------------------------------------------------------



