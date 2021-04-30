### --- results typical assemblages --- ###
# --------------------------------------- #


# load data -------------------------------------------------------------------------
data =  readRDS("data/08_typical_assemblages.rds")
data2 = readRDS("data/07_indicator_list.rds")

rows = map_int(data2,nrow)
data2 = rbindlist(data2)
data2$lvl = rep(c("species", "genus", "family"), times = rows)

ta_total_taxa = uniqueN(data2$taxon)
ta_used_taxa = uniqueN(data$taxon)

data[level == "fol", level := "family or lower level"]
data[, rt := paste0("B",rt)]


# most common level 
ta_mostcommon_lvl = data %>% 
        unique(by = "taxon") %>% 
        dplyr::select(level) %>%
        table() %>% 
        sort() %>% 
        .[3] %>% 
        names()
ta_secondcommon_lvl = data %>% 
        unique(by = "taxon") %>% 
        dplyr::select(level) %>%
        table() %>% 
        sort() %>% 
        .[2] %>% 
        names()
ta_leastcommon_lvl = data %>% 
        unique(by = "taxon") %>% 
        dplyr::select(level) %>%
        table() %>% 
        sort() %>% 
        .[1] %>% 
        names()
# observations in most common level 
ta_mostcommon_lvl_n = data %>% 
        unique(by = "taxon") %>% 
        dplyr::select(level) %>%
        table() %>% 
        sort() %>% 
        .[3]
        
ta_secondcommon_lvl_n = data %>% 
        unique(by = "taxon") %>% 
        dplyr::select(level) %>%
        table() %>% 
        sort() %>% 
        .[2] 
ta_leastcommon_lvl_n = data %>% 
        unique(by = "taxon") %>% 
        dplyr::select(level) %>%
        table() %>% 
        sort() %>% 
        .[1] 

# how many TAs do taxa appear in 
ta_mu_occ = data %>% 
        dplyr::select(taxon) %>% 
        table() %>% 
        mean %>% 
        round(1)
ta_max_occ_name = data %>% 
        dplyr::select(taxon) %>% 
        table() %>% 
        sort() %>% 
        .[uniqueN(data$taxon)] %>% 
        names()
ta_max_occ_n = data %>% 
        dplyr::select(taxon) %>% 
        table() %>% 
        sort() %>% 
        .[uniqueN(data$taxon)] 

## most common genus 
data %>% 
        filter(level == "genus") %>% 
        dplyr::select(taxon) %>% 
        table() %>% 
        sort

## - most common species 
data %>% 
        filter(level == "species") %>% 
        dplyr::select(taxon) %>% 
        table() %>% 
        sort

## - most common fol 
data %>% 
        filter(level == "family or lower level") %>% 
        dplyr::select(taxon) %>% 
        table() %>% 
        sort

data %>% 
        filter(taxon == "Serratella.ignita")
# How big are the typical assemblages 
pre_join = data[, .N, by = rt] 
ta_mean_size = round(mean(pre_join$N),1)
ta_sd_size = round(sd(pre_join$N),1)
ta_biggest_name = pre_join[which.max(pre_join$N), rt]
ta_biggest_size = pre_join[which.max(pre_join$N), N]
ta_smallest_name = pre_join[which.min(pre_join$N), rt]
ta_smallest_size = pre_join[which.min(pre_join$N), N]

# Average level of determination ----------------------------------------------------
ald = data %>% 
        filter( A != 0 & B != 0 ) %>% 
        mutate(lvl_score = case_when(level == "species" ~ 3,
                                     level == "genus" ~ 2,
                                     level == "family or lower level" ~ 1)) %>% 
        group_by(rt) %>% 
        summarise(mean = mean(lvl_score)) 

ald_overall = 
        ald %>% 
        summarise(mean2 = mean(mean), 
                  sd = sd(mean))

# mechanism - A or B ----------------------------------------------------------------
## -- how many through a 
ta_through_a_total =  data %>% dplyr::filter(mechanism == "A") %>% nrow()
ta_through_a_perce =  round( ta_through_a_total/nrow(data) * 100, 2)

## -- where do A species occur? and how many 
ta_where_a = 
        data %>% 
        dplyr::filter(mechanism == "A") %>% 
        count(rt)
ta_where_a_max_name = ta_where_a[which.max(ta_where_a$n), rt] 
ta_where_a_max_value = ta_where_a[which.max(ta_where_a$n), n] 

data %>% dplyr::select(mechanism) %>% table()
data %>% dplyr::filter(mechanism == "A")
data %>% dplyr::filter(mechanism == "AB")

# Closer look -----------------------------------------------------------------------
rt2 = data[rt == "RT2_3_8_9_10_11"]
rt2[, c("rt", "A", "B", "mechanism", "level") := NULL]
rt2$family = c(
        "Ephemeroptera", #1
        "Gatropoda",     #2
        "Ephemeroptera", #3
        "Ephemeroptera", #4
        "Odonata",       #5
        "Ephemeroptera", #6
        "Elmidae", #7
        "Ephemeroptera",#8 
        "Elmidae", #9
        "Gammaridae", #10
        "Hydraenidae", #11
        "Trichoptera", #12
        "Trichoptera", #13
        "Plecoptera", #14
        "Elmidae", #15
        "Trichoptera",#16 
        "Elmidae", #17
        "Bivalva", #18
        "Trichoptera",#19 
        "Gastropoda", #20
        "Plecoptera", #21
        "Ephemeroptera", #22
        "Trichoptera", #23
        "Trichoptera", #23
        "Chrio", #24
        "Oligo", #25
        "Simuliidae"#26 
        )
#table(rt2$family)

data[rt == "RT15_16"]
