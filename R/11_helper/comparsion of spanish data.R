### —————————————————————————————————————— ###
### ——— Comparison of Spanish data set ——— ### 
### —————————————————————————————————————— ###

# ————————————————
# date written: 10.01.2022
# date last modified: 10.01.2022 
# Project: Evaluating European Broad River Types with Macroinvertebrates
# Purpose: There has been some confusion about sampling methods in the spanish data sets. 
# Here I compare some of their properties to ensure their similarities. 
# ————————————————

# TEST 
# Dont Repeat Yourself  
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# Design by Contract 
# Fail fast/ defensive programming 

# setup -----------------------------------------------
pacman::p_load(data.table, dplyr, ggplot2, tidyr, vegan)
# load data -------------------------------------------
spain <- readRDS("data/01_original_data/monitoring_spain/2021-10-05_final_aggregated.rds")
ebro  <- readRDS("data/01_original_data/ebro/2021-10-05_final_aggregated.rds")

# prepare data ----------------------------------------
spain[, richness := uniqueN(family), by = "gr_sample_id"]
richness_spain <- 
        spain |> 
        unique(by = "gr_sample_id") |> 
        pull(richness)

ebro2 <- ebro[year > 2015]
ebro2[, richness := uniqueN(family), by = "gr_sample_id"]
richness_ebro <- 
        ebro2 |> 
        unique(by = "gr_sample_id") |> 
        pull(richness)

all1 <- data.table(data = "spain", 
           richness = richness_spain)
all2 <- data.table(data = "ebro", 
                   richness = richness_ebro)
all <- rbindlist(list(all1, all2))

ggplot(all, aes(x = richness, fill = data, col = data)) + geom_density(alpha = 0.5) + geom_rug()

t.test(richness_ebro, richness_spain)

combined <- rbindlist(list(ebro, spain))
comb_red <- combined[, .(gr_sample_id, brt12, least.impacted, season, family, data.set)]

comb_red2 <- comb_red[least.impacted == TRUE]
comb_red2[, richness := uniqueN(family), by = "gr_sample_id"]
ggplot(unique(comb_red2, by = "gr_sample_id"), aes(x = richness, fill = data.set)) + geom_density(alpha = 0.6) + facet_wrap(.~season)

comb_red2[, presence := 1]
comb_red3 <- unique(comb_red2, by = c("gr_sample_id", "family"))
data_spring <- comb_red3[season == "spring"]
data_summer <- comb_red3[season == "summer"]
data_autumn <- comb_red3[season == "autumn"]
data_spring <- pivot_wider(data_spring, id_cols = gr_sample_id, names_from = family, values_from = presence, values_fill = 0)
data_summer <- pivot_wider(data_summer, id_cols = gr_sample_id, names_from = family, values_from = presence, values_fill = 0)
data_autumn <- pivot_wider(data_autumn, id_cols = gr_sample_id, names_from = family, values_from = presence, values_fill = 0)
dist_spring <- vegdist(data_spring[,-1], method = "jaccard")
dist_summer <- vegdist(data_summer[,-1], method = "jaccard")
dist_autumn <- vegdist(data_autumn[,-1], method = "jaccard")
nmds_spring <- metaMDS(dist_spring)
nmds_summer <- metaMDS(dist_summer)
nmds_autumn <- metaMDS(dist_autumn)

nmds_spring2 <- data.table(nmds1 = nmds_spring$points[,1], 
                           nmds2 = nmds_spring$points[,2],
                           gr_sample_id = data_spring$gr_sample_id)
nmds_summer2 <- data.table(nmds1 = nmds_summer$points[,1], 
                           nmds2 = nmds_summer$points[,2],
                           gr_sample_id = data_summer$gr_sample_id)
nmds_autumn2 <- data.table(nmds1 = nmds_autumn$points[,1], 
                           nmds2 = nmds_autumn$points[,2],
                           gr_sample_id = data_autumn$gr_sample_id)

nmds_spring3 <- left_join(nmds_spring2,
                          comb_red3[,c("gr_sample_id", "data.set")],
                          on = "gr_sample_id")
nmds_summer3 <- left_join(nmds_summer2,
                          comb_red3[,c("gr_sample_id", "data.set")],
                          on = "gr_sample_id")
nmds_autumn3 <- left_join(nmds_autumn2,
                          comb_red3[,c("gr_sample_id", "data.set")],
                          on = "gr_sample_id")
ggplot(nmds_spring3, aes(x = nmds1, y = nmds2, col = data.set)) + geom_point()
ggplot(nmds_summer3, aes(x = nmds1, y = nmds2, col = data.set)) + geom_point()
ggplot(nmds_autumn3, aes(x = nmds1, y = nmds2, col = data.set)) + geom_point()

anosim_spring <- anosim(dist_spring, grouping = pull(unique(nmds_spring3, by = "gr_sample_id"), "data.set"))
anosim_summer <- anosim(dist_summer, grouping = pull(unique(nmds_summer3, by = "gr_sample_id"), "data.set"))
anosim_autumn <- anosim(dist_autumn, grouping = pull(unique(nmds_autumn3, by = "gr_sample_id"), "data.set"))

