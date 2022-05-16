
library(data.table)
data.1 <- readRDS("data/02_combined_data/01_2022-05-04_combined_data_aggregated.rds")
data.2 <- readRDS("data/02_combined_data/02_2022-05-04_data_well_sampled_types.rds")
data.1.bind <- rbindlist(data.1)
data.2.bind <- rbindlist(data.2)

nrow(unique(data.1.bind, by = "gr_sample_id"))
nrow(unique(data.1.bind, by = c("data.set", "site_id")))
## how many samples 
nrow(unique(data.1$spring, by = "gr_sample_id"))
nrow(unique(data.1$summer, by = "gr_sample_id"))
nrow(unique(data.1$autumn, by = "gr_sample_id"))

## how many sites in total? 
nrow(unique(data.1$spring, by = c("site_id", "data.set")))
nrow(unique(data.1$summer, by = c("site_id", "data.set")))
nrow(unique(data.1$autumn, by = c("site_id", "data.set")))

## how many remain after removing rare types? 
nrow(unique(data.2.bind, by = "gr_sample_id"))
nrow(unique(data.2.bind, by = c("data.set", "site_id")))


## how many sites per type and seaons
data.2 <- rbindlist(data.2)
data.2 <- unique(data.2, by = "gr_sample_id")
table(data.2$season, data.2$brt12)
