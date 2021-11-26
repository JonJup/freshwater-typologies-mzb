# ————————————————————————————— #
# ——— Families X Data sets  ——— # 
# ————————————————————————————— #

# ————————————————
#  date created: 06-10-21
# last modified: 14-10-21
#       Project: Evaluating European Broad River Types for Macroinvertebrates
#       Purpose: Which families/ orders occur in which data sets? 
# ————————————————


# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, xlsx)

# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/02_2021-10-14_core_taxa_data_aggregated.rds")

# create table ----------------------------------------------------------------------
data2 <- lapply(data,  function(x) unique(x, by = c("family", "data.set")))
data2 <- lapply(data2,  function(x) x[,c("data.set", "family", "abundance2")])
data2 <- lapply(data2, function(x) x[, abundance2 := 1])
data2 <- lapply(data2, function(x) x[!is.na(family)])
data3 <- lapply(data2, function(x) tidyr::pivot_wider(x, id_cols = data.set, names_from = family, values_from = abundance2, values_fill = 0))
data4 <- lapply(data3, function(x) dplyr::mutate(x, total = rowSums(x[,-1])))

data5 <- lapply(data,  function(x) unique(x, by = c("order", "data.set")))
data5 <- lapply(data5, function(x) x[, abundance2 := 1])
data6 <- lapply(data5, function(x) tidyr::pivot_wider(x, id_cols = data.set, names_from = order, values_from = abundance2, values_fill = 0))
data7 <- lapply(data6, function(x) dplyr::mutate(x, total = rowSums(x[,-1])))

# save to file  ---------------------------------------------------------------------
write.xlsx(data4[[1]], paste0("data/02_combined_data/",Sys.Date(),"family_x_dataset_2.xlsx"), sheetName = "spring")
write.xlsx(data4[[2]], paste0("data/02_combined_data/",Sys.Date(),"family_x_dataset_2.xlsx"), sheetName = "summer", append = TRUE)
write.xlsx(data4[[3]], paste0("data/02_combined_data/",Sys.Date(),"family_x_dataset_2.xlsx"), sheetName = "autumn", append = TRUE)
write.xlsx(data7[[1]], paste0("data/02_combined_data/",Sys.Date(),"order_x_dataset_2.xlsx"), sheetName = "spring")
write.xlsx(data7[[2]], paste0("data/02_combined_data/",Sys.Date(),"order_x_dataset_2.xlsx"), sheetName = "summer", append = TRUE)
write.xlsx(data7[[3]], paste0("data/02_combined_data/",Sys.Date(),"order_x_dataset_2.xlsx"), sheetName = "autumn", append = TRUE)


# inspect results -------------------------------------------------------------------
data.x <- data[[1]]


data.x[data.set == "Monitoring data from Romania" & is.na(order), original_name]

#- insights: 
#-> there are no Dipterans in the Ecoserv data set
#-> many families are missing from the Segura basin data set: 
