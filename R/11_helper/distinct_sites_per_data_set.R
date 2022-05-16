## -- Number of distinct sampling sites in each data set -- ## 


# setup -----------------------------------------------------------------------------------
pacman::p_load(data.table, stringr)

# load data -------------------------------------------------------------------------
data <- readRDS("data/01_all_mzb_combined.rds")

data[, site_id := str_remove(gr_sample_id, "date_.*_")]

data[, uniqueN(site_id), by = "data.set"]

