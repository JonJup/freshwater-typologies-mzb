# ——————————————————————— #
# ——— Pelodes for GLD ——— # 
# ——————————————————————— #

# ———————————————————————————————————
# date: 
#       21.07.21
# files in: 
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Prepare data from GLD for PELODES evaluation. 
# ————————————————



# SETUP -----------------------------------------------------------------------------
pacman::p_load(readxl, dplyr, tidyr, stringr, data.table, magrittr, readxl)

# LOAD DATA -------------------------------------------------------------------------
data <- readRDS("data/original data/gld/auxilliary/01_2021-07-21_data.rds")


#- we have 3687 sampling events  
n <- uniqueN(data$gr_sample_id)
site.group.beg <- seq(from = 1, to = n, by = 500)
site.group.end <- site.group.beg - 1
site.group.end <- site.group.end[-1]
site.group.end[length(site.group.end) + 1]  <- n

sites <- unique(data, by = "gr_sample_id")
sites %<>% select("gr_sample_id", typ, Nutzung)

samples <- copy(data)
samples %<>% select(gr_sample_id, DV, taxon, abundance)
sites.list   <- lapply(seq_along(site.group.beg), function(x) sites[site.group.beg[x]:site.group.end[x], ])
samples.list <- lapply(seq_along(site.group.beg), function(x) samples[gr_sample_id %in% sites.list[[x]]$gr_sample_id] )
samples.list <- lapply(samples.list, pivot_wider, names_from = "gr_sample_id", values_fill = 0, values_from = "abundance", values_fn = sum)


#- drop taxa that do not occur in subset
samples.list <- lapply(seq_along(samples.list), function(x) samples.list[[x]][which(rowSums(samples.list[[x]][,-c(1,2)]) > 0), ])


for (i in seq_along(samples.list)){
        if (i %in% 1:2) next()
        print(i)
        xlsx::write.xlsx(samples.list[[i]],    paste0("data/original data/gld/auxilliary/perlodes/",i, "_", Sys.Date(),"perlodes_taxa1.xlsx"))
        xlsx::write.xlsx(sites.list[[i]],      paste0("data/original data/gld/auxilliary/perlodes/",i, "_", Sys.Date(),"perlodes_taxa2.xlsx"))
}


# RESULTS ---------------------------------------------------------------------------

res.file <- fs::dir_ls("data/original data/gld/auxilliary/perlodes/results")

res.file <- lapply(seq_along(res.file), function(x) read_excel(res.file[x], sheet = 3))
res.file <- lapply(res.file, function(x) select(x, Probe, ÖZK))
res.file <- rbindlist(res.file)

saveRDS(res.file, "data/original data/gld/auxilliary/perlodes_results.rds")

