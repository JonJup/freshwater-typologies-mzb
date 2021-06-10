# --------------------------------------- #
### --- results typical assemblages --- ###
### --- script for paper            --- ### 
# --------------------------------------- #


# load data -------------------------------------------------------------------------
# data =  readRDS("data/08_typical_assemblages.rds")
# data2 = readRDS("data/07_indicator_list.rds")

ta.rt1 <- "data/typical_assemblages/21_brt12_ta_complete.rds" %>% readRDS %>% unlist
ta.rt1 <- data.table(taxon = ta.rt1, type  = str_remove(names(ta.rt1), "\\..*"), typology = "BRT12")
ta.rt2 <- "data/typical_assemblages/21_brt20_ta_complete.rds" %>% readRDS %>% unlist
ta.rt2 <- data.table(taxon = ta.rt2, type  = str_remove(names(ta.rt2), "\\..*"), typology = "BRT20")
ta.glo <- "data/typical_assemblages/21_gloric_ta_complete.rds" %>% readRDS %>% unlist
ta.glo <- data.table(taxon = ta.glo, type  = str_remove(names(ta.glo), "\\..*"), typology = "GloRiC")
ta.ill <- "data/typical_assemblages/21_illies_ta_complete.rds" %>% readRDS %>% unlist
ta.ill <- data.table(taxon = ta.ill, type  = str_remove(names(ta.ill), "\\..*"), typology = "Illies")
ta.bgr <- "data/typical_assemblages/21_bgr_ta_complete.rds" %>% readRDS %>% unlist
ta.bgr <- data.table(taxon = ta.bgr, type  = str_remove(names(ta.bgr), "\\..*"), typology = "BGR")

ta <-
        rbindlist(list(ta.rt1, ta.rt2, ta.glo, ta.ill, ta.bgr)) %>% 
        mutate(type2 = paste(type, typology))

# ta %>% 
#         group_by(typology) %>% 
#         count(taxon)

taxa.brt12 <- ncol(all_data$spe$brt12)  + ncol(all_data$gen$brt12) + ncol(all_data$foh$brt12) - 6
taxa.brt20 <- ncol(all_data$spe$brt20)  + ncol(all_data$gen$brt20) + ncol(all_data$foh$brt20) - 6
taxa.gloric <- ncol(all_data$spe$gloric) + ncol(all_data$gen$gloric) + ncol(all_data$foh$gloric) - 6
taxa.illies <- ncol(all_data$spe$illies) + ncol(all_data$gen$illies) + ncol(all_data$foh$illies) - 6
taxa.bgr <- ncol(all_data$spe$bgr)    + ncol(all_data$gen$bgr) + ncol(all_data$foh$bgr) - 6

ta.perc.brt12  <- ta[typology == "BRT12" , uniqueN(taxon)/taxa.brt12 * 100]  %>% round(1) %>% paste0("%")
ta.perc.brt20  <- ta[typology == "BRT20" , uniqueN(taxon)/taxa.brt20 * 100]  %>% round(1) %>% paste0("%")
ta.perc.gloric <- ta[typology == "GloRiC", uniqueN(taxon)/taxa.gloric * 100] %>% round(1) %>% paste0("%")
ta.perc.illies <- ta[typology == "Illies", uniqueN(taxon)/taxa.illies * 100] %>% round(1) %>% paste0("%")
ta.perc.bgr    <- ta[typology == "BGR"   , uniqueN(taxon)/taxa.bgr * 100]    %>% round(1) %>% paste0("%")

# how many TAs do taxa appear in 
ta.mu.occ <- list()
ta.mu.occ$rt1 <- ta %>% filter(typology == "BRT12")  %>% dplyr::select(taxon) %>% table %>% mean %>% round(1)
ta.mu.occ$rt2 <- ta %>% filter(typology == "BRT20")  %>% dplyr::select(taxon) %>% table %>% mean %>% round(1)
ta.mu.occ$glo <- ta %>% filter(typology == "GloRiC") %>% dplyr::select(taxon) %>% table %>% mean %>% round(1)
ta.mu.occ$ill <- ta %>% filter(typology == "Illies") %>% dplyr::select(taxon) %>% table %>% mean %>% round(1)
ta.mu.occ$bgr <- ta %>% filter(typology == "BGR")    %>% dplyr::select(taxon) %>% table %>% mean %>% round(1)

## -- how many typical assemblages are there? 
# ta %>% 
#         pull(type2) %>% 
#         unique %>% 
#         length

## -- most common taxa 
# ta %>% 
#         select(taxon) %>% 
#         table() %>% 
#         sort

## -- where is chironomidae missing 
# ta %>%
#         filter(taxon == "Chironomidae") %>% 
#         pull(type2) %>% 
#         unique -> 
#         chiro_types
# 
# ta %>% 
#         filter(!type2 %in% chiro_types) %>% 
#         pull(type2) %>% 
#         unique

### --- OLD 
# rows = map_int(data2,nrow)
# data2 = rbindlist(data2)
# data2$lvl = rep(c("species", "genus", "family"), times = rows)

# ta_total_taxa = uniqueN(data2$taxon)
# ta_used_taxa = uniqueN(data$taxon)
# 
# data[level == "fol", level := "family or lower level"]
# data[, rt := paste0("B",rt)]
# 
# 
# # most common level 
# ta_mostcommon_lvl = data %>% 
#         unique(by = "taxon") %>% 
#         dplyr::select(level) %>%
#         table() %>% 
#         sort() %>% 
#         .[3] %>% 
#         names()
# ta_secondcommon_lvl = data %>% 
#         unique(by = "taxon") %>% 
#         dplyr::select(level) %>%
#         table() %>% 
#         sort() %>% 
#         .[2] %>% 
#         names()
# ta_leastcommon_lvl = data %>% 
#         unique(by = "taxon") %>% 
#         dplyr::select(level) %>%
#         table() %>% 
#         sort() %>% 
#         .[1] %>% 
#         names()
# # observations in most common level 
# ta_mostcommon_lvl_n = data %>% 
#         unique(by = "taxon") %>% 
#         dplyr::select(level) %>%
#         table() %>% 
#         sort() %>% 
#         .[3]
#         
# ta_secondcommon_lvl_n = data %>% 
#         unique(by = "taxon") %>% 
#         dplyr::select(level) %>%
#         table() %>% 
#         sort() %>% 
#         .[2] 
# ta_leastcommon_lvl_n = data %>% 
#         unique(by = "taxon") %>% 
#         dplyr::select(level) %>%
#         table() %>% 
#         sort() %>% 
#         .[1] 
# 
# # how many TAs do taxa appear in 
# ta_mu_occ = data %>% 
#         dplyr::select(taxon) %>% 
#         table() %>% 
#         mean %>% 
#         round(1)
# ta_max_occ_name = data %>% 
#         dplyr::select(taxon) %>% 
#         table() %>% 
#         sort() %>% 
#         .[uniqueN(data$taxon)] %>% 
#         names()
# ta_max_occ_n = data %>% 
#         dplyr::select(taxon) %>% 
#         table() %>% 
#         sort() %>% 
#         .[uniqueN(data$taxon)] 
# 
# ## most common genus 
# data %>% 
#         filter(level == "genus") %>% 
#         dplyr::select(taxon) %>% 
#         table() %>% 
#         sort
# 
# ## - most common species 
# data %>% 
#         filter(level == "species") %>% 
#         dplyr::select(taxon) %>% 
#         table() %>% 
#         sort
# 
# ## - most common fol 
# data %>% 
#         filter(level == "family or lower level") %>% 
#         dplyr::select(taxon) %>% 
#         table() %>% 
#         sort
# 
# data %>% 
#         filter(taxon == "Serratella.ignita")
# # How big are the typical assemblages 
# pre_join = data[, .N, by = rt] 
# ta_mean_size = round(mean(pre_join$N),1)
# ta_sd_size = round(sd(pre_join$N),1)
# ta_biggest_name = pre_join[which.max(pre_join$N), rt]
# ta_biggest_size = pre_join[which.max(pre_join$N), N]
# ta_smallest_name = pre_join[which.min(pre_join$N), rt]
# ta_smallest_size = pre_join[which.min(pre_join$N), N]
# 
# # Average level of determination ----------------------------------------------------
# ald = data %>% 
#         filter( A != 0 & B != 0 ) %>% 
#         mutate(lvl_score = case_when(level == "species" ~ 3,
#                                      level == "genus" ~ 2,
#                                      level == "family or lower level" ~ 1)) %>% 
#         group_by(rt) %>% 
#         summarise(mean = mean(lvl_score)) 
# 
# ald_overall = 
#         ald %>% 
#         summarise(mean2 = mean(mean), 
#                   sd = sd(mean))
# 
# # mechanism - A or B ----------------------------------------------------------------
# ## -- how many through a 
# ta_through_a_total =  data %>% dplyr::filter(mechanism == "A") %>% nrow()
# ta_through_a_perce =  round( ta_through_a_total/nrow(data) * 100, 2)
# 
# ## -- where do A species occur? and how many 
# ta_where_a = 
#         data %>% 
#         dplyr::filter(mechanism == "A") %>% 
#         count(rt)
# ta_where_a_max_name = ta_where_a[which.max(ta_where_a$n), rt] 
# ta_where_a_max_value = ta_where_a[which.max(ta_where_a$n), n] 
# 
# data %>% dplyr::select(mechanism) %>% table()
# data %>% dplyr::filter(mechanism == "A")
# data %>% dplyr::filter(mechanism == "AB")
# 
# # Closer look -----------------------------------------------------------------------
# rt2 = data[rt == "RT2_3_8_9_10_11"]
# rt2[, c("rt", "A", "B", "mechanism", "level") := NULL]
# rt2$family = c(
#         "Ephemeroptera", #1
#         "Gatropoda",     #2
#         "Ephemeroptera", #3
#         "Ephemeroptera", #4
#         "Odonata",       #5
#         "Ephemeroptera", #6
#         "Elmidae", #7
#         "Ephemeroptera",#8 
#         "Elmidae", #9
#         "Gammaridae", #10
#         "Hydraenidae", #11
#         "Trichoptera", #12
#         "Trichoptera", #13
#         "Plecoptera", #14
#         "Elmidae", #15
#         "Trichoptera",#16 
#         "Elmidae", #17
#         "Bivalva", #18
#         "Trichoptera",#19 
#         "Gastropoda", #20
#         "Plecoptera", #21
#         "Ephemeroptera", #22
#         "Trichoptera", #23
#         "Trichoptera", #23
#         "Chrio", #24
#         "Oligo", #25
#         "Simuliidae"#26 
#         )
# #table(rt2$family)
# 
# data[rt == "RT15_16"]
