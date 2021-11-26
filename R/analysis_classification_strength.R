# ———————————————————————— #
# ——— Analysis: CS ——————— # 
# ———————————————————————— #
# ———————————————————————————————————
#  date created: 27-09-21
# last modified: 14-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute Classification strength
# ———————————————————————————————————

# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/JonJup/jjmisc")
library(data.table)
library(ggplot2)
library(dplyr)
library(jjmisc)
# load data -------------------------------------------------------------------------
id   <- readRDS("data/02_combined_Data/2021-10-14_distance_ids.rds")
d.sp.i <- readRDS("data/02_combined_data/2021-10-14_distance_spring.rds")
d.su.i <- readRDS("data/02_combined_data/2021-10-14_distance_summer.rds")
d.au.i <- readRDS("data/02_combined_data/2021-10-14_distance_autumn.rds")
d.sp.l <- readRDS("data/02_combined_data/2021-10-14_distance_spring_li.rds")
d.su.l <- readRDS("data/02_combined_data/2021-10-14_distance_summer_li.rds")
d.au.l <- readRDS("data/02_combined_data/2021-10-14_distance_autumn_li.rds")


# analysis --------------------------------------------------------------------------

#- test function before uploading it 
# source("../../../projects/jjmisc/R/compute_cs.R")
# source("../../../projects/jjmisc/R/classification_strength.R")

compute_cs(dist = d.sp.i, grouping = id$sp.i$brt, season = "spring", typology = "brt")
classification_strength(dist = d.sp.i, grouping = id$sp.i$brt, season = "spring", typology = "brt", permutations = 10)

sp.l.brt <- classification_strength(dist = d.sp.l, grouping = id$sp.l$brt, season = "spring", typology = "brt", permutations = 99)
su.l.brt <- classification_strength(dist = d.su.l, grouping = id$su.l$brt, season = "summer", typology = "brt", permutations = 99)
au.l.brt <- classification_strength(dist = d.au.l, grouping = id$au.l$brt, season = "autumn", typology = "brt", permutations = 99)
sp.i.brt <- classification_strength(dist = d.sp.i, grouping = id$sp.i$brt, season = "spring", typology = "brt", permutations = 99)
su.i.brt <- classification_strength(dist = d.su.i, grouping = id$su.i$brt, season = "summer", typology = "brt", permutations = 99)
au.i.brt <- classification_strength(dist = d.au.i, grouping = id$au.i$brt, season = "autumn", typology = "brt", permutations = 99)
su.l.bgr <- classification_strength(dist = d.su.l, grouping = id$su.l$bgr, season = "summer", typology = "bgr", permutations = 99)
sp.l.bgr <- classification_strength(dist = d.sp.l, grouping = id$sp.l$bgr, season = "spring", typology = "bgr", permutations = 99)
au.l.bgr <- classification_strength(dist = d.au.l, grouping = id$au.l$bgr, season = "autumn", typology = "bgr", permutations = 99)
sp.i.bgr <- classification_strength(dist = d.sp.i, grouping = id$sp.i$bgr, season = "spring", typology = "bgr", permutations = 99)
su.i.bgr <- classification_strength(dist = d.su.i, grouping = id$su.i$bgr, season = "summer", typology = "bgr", permutations = 99)
au.i.bgr <- classification_strength(dist = d.au.i, grouping = id$au.i$bgr, season = "autumn", typology = "bgr", permutations = 99)
sp.l.ife <- classification_strength(dist = d.sp.l, grouping = id$sp.l$ife, season = "spring", typology = "ife", permutations = 99)
su.l.ife <- classification_strength(dist = d.su.l, grouping = id$su.l$ife, season = "summer", typology = "ife", permutations = 99)
au.l.ife <- classification_strength(dist = d.au.l, grouping = id$au.l$ife, season = "autumn", typology = "ife", permutations = 99)
sp.i.ife <- classification_strength(dist = d.sp.i, grouping = id$sp.i$ife, season = "spring", typology = "ife", permutations = 99)
su.i.ife <- classification_strength(dist = d.su.i, grouping = id$su.i$ife, season = "summer", typology = "ife", permutations = 99)
au.i.ife <- classification_strength(dist = d.au.i, grouping = id$au.i$ife, season = "autumn", typology = "ife", permutations = 99)


brt.l <- rbindlist(list(sp.l.brt, su.l.brt, au.l.brt)) |> {\(x) x[,impaired := FALSE]}()
brt.i <- rbindlist(list(sp.i.brt, su.i.brt, au.i.brt)) |> {\(x) x[,impaired := TRUE]}()

bgr.l <- rbindlist(list(sp.l.bgr, su.l.bgr, au.l.bgr)) |> {\(x) x[,impaired := FALSE]}()
bgr.i <- rbindlist(list(sp.i.bgr, su.i.bgr, au.i.bgr)) |> {\(x) x[,impaired := TRUE]}()

ife.l <- rbindlist(list(sp.l.ife, su.l.ife, au.l.ife)) |> {\(x) x[,impaired := FALSE]}()
ife.i <- rbindlist(list(sp.i.ife, su.i.ife, au.i.ife)) |> {\(x) x[,impaired := TRUE]}()

all <- rbindlist(list(brt.l, brt.i, bgr.i, bgr.l, ife.i, ife.l))



saveRDS(all, "data/02_combined_data/2021-10-14_cs_results.rds")
all <- readRDS("data/02_combined_data/2021-10-12_cs_results.rds")



all |> 
        unique(by = c("season", "typlogy", "impaired")) |> 
        ggplot(aes(x = classification_strength, 
                   y = typlogy)) + 
        geom_point(aes(col = impaired), size = 3) + 
        facet_wrap(.~season)




# mean.w.i.brt <- all |> filter(impaired == TRUE  & typlogy == "brt") |> pull(within_type)  |> mean(na.rm = TRUE) |> round(2)
# mean.b.i.brt <- all |> filter(impaired == TRUE  & typlogy == "brt") |> pull(between_type) |> mean(na.rm = TRUE) |> round(2)
# mean.w.l.brt <- all |> filter(impaired == FALSE & typlogy == "brt") |> pull(within_type)  |> mean(na.rm = TRUE) |> round(2)
# mean.b.l.brt <- all |> filter(impaired == FALSE & typlogy == "brt") |> pull(between_type) |> mean(na.rm = TRUE) |> round(2)
# mean.w.i.bgr <- all |> filter(impaired == TRUE  & typlogy == "bgr")   |> pull(within_type)  |> mean(na.rm = TRUE) |> round(2)
# mean.b.i.bgr <- all |> filter(impaired == TRUE  & typlogy == "bgr")   |> pull(between_type) |> mean(na.rm = TRUE) |> round(2)
# mean.w.l.bgr <- all |> filter(impaired == FALSE & typlogy == "bgr")   |> pull(within_type)  |> mean(na.rm = TRUE) |> round(2)
# mean.b.l.bgr <- all |> filter(impaired == FALSE & typlogy == "bgr")   |> pull(between_type) |> mean(na.rm = TRUE) |> round(2)
# mean.w.i.ife <- all |> filter(impaired == TRUE  & typlogy == "ife")   |> pull(within_type)  |> mean(na.rm = TRUE) |> round(2)
# mean.b.i.ife <- all |> filter(impaired == TRUE  & typlogy == "ife")   |> pull(between_type) |> mean(na.rm = TRUE) |> round(2)
# mean.w.l.ife <- all |> filter(impaired == FALSE & typlogy == "ife")   |> pull(within_type)  |> mean(na.rm = TRUE) |> round(2)
# mean.b.l.ife <- all |> filter(impaired == FALSE & typlogy == "ife")   |> pull(between_type) |> mean(na.rm = TRUE) |> round(2)
# 
# lines.tbl <- tibble(typology = c("bgr","brt", "ife"),
#                     between = c(mean.b.l.bgr,mean.b.l.brt, mean.b.l.ife),
#                     within = c(mean.w.l.bgr,mean.w.l.brt, mean.w.l.ife)
# )
# 
# all2 <- 
#         all|>  
#         filter(impaired == FALSE) |> 
#         mutate(type2 = paste0(typlogy, "_", type)) |> 
#         group_by(type2, season) |> 
#         summarise(within_type_mean = mean(within_type), 
#                   between_type_mean = mean(between_type),
#                   .groups = "keep") |>  
#         mutate(cs = round(within_type_mean - between_type_mean, 2), 
#                typology = stringr::str_remove(stringr::str_extract(type2, ".*_"), "_")) 
# 
# all3 <- 
#         all2 |> 
#         left_join(filter(all_props, impaired == TRUE), by = c("type2", "season"))
# 
# all3 |> 
#         filter(typology.x == "bgr") |> 
#         ggplot() +
#         facet_wrap(season ~ .) + 
#         # geom_vline(data = lines.tbl, aes(xintercept = between), col = "red" , lty = 2) +
#         # geom_vline(data = lines.tbl, aes(xintercept = within),  col = "blue", lty = 2) +
#         geom_segment(aes(x = between_type_mean, xend = within_type_mean, y = type2, yend = type2)) +
#         geom_point(aes(x = between_type_mean, y = type2, fill = proportion), shape = 21, size = 4) + 
#         geom_point(aes(x = within_type_mean, y = type2, fill = proportion) , shape = 22, size = 4) + 
#         geom_label(aes(label = cs, x = within_type_mean - ((within_type_mean - between_type_mean))/2, y = type2), nudge_y = .2) + 
#         xlab("similarity") + 
#         theme(panel.grid = element_blank())
# all3 |> 
#         filter(typology.x == "ife") |> 
#         ggplot() +
#         facet_wrap(season ~ .) + 
#         # geom_vline(data = lines.tbl, aes(xintercept = between), col = "red" , lty = 2) +
#         # geom_vline(data = lines.tbl, aes(xintercept = within),  col = "blue", lty = 2) +
#         geom_segment(aes(x = between_type_mean, xend = within_type_mean, y = type2, yend = type2)) +
#         geom_point(aes(x = between_type_mean, y = type2, fill = proportion), shape = 21, size = 4) + 
#         geom_point(aes(x = within_type_mean, y = type2, fill = proportion) , shape = 22, size = 4) + 
#         geom_label(aes(label = cs, x = within_type_mean - ((within_type_mean - between_type_mean))/2, y = type2), nudge_y = .2) + 
#         xlab("similarity") + 
#         theme(panel.grid = element_blank())
# all3 |> 
#         filter(typology.x == "brt") |> 
#         ggplot() +
#         facet_wrap(season ~ .) + 
#         # geom_vline(data = lines.tbl, aes(xintercept = between), col = "red" , lty = 2) +
#         # geom_vline(data = lines.tbl, aes(xintercept = within),  col = "blue", lty = 2) +
#         geom_segment(aes(x = between_type_mean, xend = within_type_mean, y = type2, yend = type2)) +
#         geom_point(aes(x = between_type_mean, y = type2, fill = proportion), shape = 21, size = 4) + 
#         geom_point(aes(x = within_type_mean, y = type2, fill = proportion) , shape = 22, size = 4) + 
#         geom_label(aes(label = cs, x = within_type_mean - ((within_type_mean - between_type_mean))/2, y = type2), nudge_y = .2) + 
#         xlab("similarity") + 
#         theme(panel.grid = element_blank())
#         
# all3 |> 
#         ggplot(aes(y = proportion, x = cs)) + 
#         geom_point(aes(fill = typology.x), shape = 21) + 
#         geom_smooth(se = FALSE, aes(col = typology.x)) +
#         facet_grid(.~typology.x)
# all3 |> 
#         ggplot(aes(y = proportion, x = within_type_mean)) + 
#         geom_point(aes(fill = typology.x), shape = 21) + 
#         geom_smooth(se = FALSE, aes(col = typology.x)) +
#         facet_grid(.~typology.x) + 
#         ggtitle("within")
# all3 |> 
#         ggplot(aes(y = proportion, x = between_type_mean)) + 
#         geom_point(aes(fill = typology.x), shape = 21) + 
#         geom_smooth(se = FALSE, aes(col = typology.x)) +
#         facet_grid(.~typology.x)
# 
#         

# old 14.10.21 ----------------------------------------------------------------------

# sp.l.brt |> 
#         mutate(between_type_mean)
# 
# 
# sp.l.brt$between_type_mean       <- mean(sp.l.brt$between_type)
# sp.l.brt$classification_strength <- sp.l.brt$within_type - sp.l.brt$between_type_mean
# sp.l.brt <- left_join(sp.l.brt, 
#           filter(all_props, typology == "brt" & season == "spring" & impaired == FALSE))
# sp.l.brt |> 
#         mutate(within_weighted = within_type * proportion) |> 
#         mutate(cs = sum(within_weighted) - between_type_mean)

# old 13.10.21 ----------------------------------------------------------------------
# 
# for (i in names(id)){
#         x <- id[[i]]
#         x.season <- ifelse(stringr::str_detect(i, "sp"), "spring", ifelse(stringr::str_detect(i, "su"), "summer", "autumn"))
#         x.impact <- ifelse(stringr::str_detect(i, "i$"), TRUE, FALSE)
#         for (j in c("brt", "bgr", "ife")){
#                 y <- x[[j]]
#                 y <- get_props(y)
#                 y$season <- x.season
#                 y$impaired <- x.impact
#                 y$typology <- j
#                 all_props[[length(all_props) + 1]] <- y
#         }
# }
# all_props <- rbindlist(all_props)
# all_props$type2 <- paste0(all_props$typology,"_",all_props$type)
