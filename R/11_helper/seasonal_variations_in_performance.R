### ————————————— ###
### ——— TITLE ——— ### 
### ————————————— ###

# ————————————————
# date: 14.12.21
#
# Project: macorinvertebrates in broad river types
# 
# Purpose: check which species drive seasonal differences
#  
# ————————————————

# TEST 
# Don't Repeat Yourself  
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# Design by Contract 
# Fail fast/ defensive programming 

# setup -----------------------------------------------
pacman::p_load(data.table, dplyr,sf, ggplot2, tidyr,magrittr, indicspecies, tibble)
# load data -------------------------------------------
all_mzb <- readRDS("data/02_combined_data/01_2021-10-14_combined_data_aggregated.rds")

# prepare data ----------------------------------------
all_mzb_bound <- rbindlist(all_mzb)
split_brt <- split(all_mzb_bound, by = "brt12")
split_ife <- split(all_mzb_bound, by = "ife")
split_bgr <- split(all_mzb_bound, by = "bgr")

# analyze --------------------------------------------
# distribution of sites in types per seasons 
all_mzb_bound |> 
        unique(by = "gr_sample_id")  |> 
        st_as_sf() |> 
        st_drop_geometry() |> 
        group_by(season) |> 
        count(brt12) |> 
        pivot_wider(id_cols = brt12, names_from = "season", values_from = "n") |> 
        mutate(
                spring = spring/sum(spring),
                summer = summer/sum(summer),
                autumn = autumn/sum(autumn),
                ) |> 
        pivot_longer(cols = !brt12, names_to = "season", values_to = "n") |> 
        ggplot(aes(x = brt12, y = n, fill = season)) + 
        #geom_point(shape = 21) + 
        geom_line(aes(group = season, col = season))
all_mzb_bound |> 
        unique(by = "gr_sample_id")  |> 
        st_as_sf() |> 
        st_drop_geometry() |> 
        group_by(season) |> 
        count(ife) |> 
        pivot_wider(id_cols = ife, names_from = "season", values_from = "n", values_fill = 0) |> 
        mutate(
                spring = spring/sum(spring),
                summer = summer/sum(summer),
                autumn = autumn/sum(autumn),
                ) |> 
        pivot_longer(cols = !ife, names_to = "season", values_to = "n") |> 
        ggplot(aes(x = ife, y = n, fill = season)) + 
        #geom_point(shape = 21) + 
        geom_line(aes(group = season, col = season)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
all_mzb_bound |> 
        unique(by = "gr_sample_id")  |> 
        st_as_sf() |> 
        st_drop_geometry() |> 
        group_by(season) |> 
        count(bgr) |> 
        pivot_wider(id_cols = bgr, names_from = "season", values_from = "n", values_fill = 0) |> 
        mutate(
                spring = spring/sum(spring),
                summer = summer/sum(summer),
                autumn = autumn/sum(autumn),
        ) |> 
        pivot_longer(cols = !bgr, names_to = "season", values_to = "n") |> 
        ggplot(aes(x = bgr, y = n, fill = season)) + 
        #geom_point(shape = 21) + 
        geom_line(aes(group = season, col = season)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# taxa ------------------------------------------------------------------------------
split_ife2 <- lapply(split_ife, function(x) x[,c("family", "season", "gr_sample_id")])
split_ife2 %<>% lapply(function(x) x[season %in% c("summer", "autumn"), season := "non_spring"])
split_ife2 %<>% lapply(function(x) unique(x, by = c("gr_sample_id", "family")))
split_ife2 %<>% lapply(function(x) x[!is.na(family)])
split_ife2 %<>% lapply(function(x) x[, value := 1])
split_ife2_type <- lapply(split_ife2, function(x) x$season)
split_ife2 %<>% lapply(pivot_wider, id_cols = c("gr_sample_id", "season"), names_from = family, values_fill = 0)

for (i in 1:length(split_ife2)){
        ## spring indicators 
        if (i == 1) spring_indicators <- lst()
        if (i == 1) non_spring_indicators <- lst()
        ## type name 
        i.name <- names(split_ife)[i]
        ## extract season 
        i.seas <- split_ife2[[i]]$season
        ## only one season? skip 

        if (uniqueN(i.seas) == 1) next()
        
        ## extract X 
        X <- as.matrix(split_ife2[[i]][,-c(1:2)])
        Y <- multipatt(x = X, cluster = i.seas, duleg = TRUE)
        if (!"s.spring" %in% names(Y$sign)) next()
        spring_indicators[[i.name]] <- 
                sort(rownames(filter(Y$sign, p.value < 0.05 & s.spring == 1)))
        non_spring_indicators[[i.name]] <- 
                sort(rownames(filter(Y$sign, p.value < 0.05 & s.non_spring == 1)))

        
        # YA <- Y$A |> 
        #         data.frame() |> 
        #         rownames_to_column("taxon") |> 
        #         pivot_longer(cols = !taxon, names_to = "season", values_to = "A")
        # YB <- Y$B |> 
        #         data.frame() |> 
        #         rownames_to_column("taxon") |> 
        #         pivot_longer(cols = !taxon, names_to = "season", values_to = "B")
        #         
        # Y2 <- left_join(YA, YB, by = c("taxon", "season"))
        # 
        # Y3 = st_as_sf(Y2, coords=c("A","B"))
        # polys = Y3 %>% 
        #         dplyr::group_by(taxon) %>% 
        #         st_distance(polys)
        # 
        # for (i in 1:uniqueN(Y3$taxon)){
        #         if (i == 1) sv.lst <- lst()
        #         Y.i <- filter(Y3, taxon == unique(Y3$taxon)[i])
        #         Y.d <- max(st_distance(Y.i))
        #         out <- data.table(taxon = unique(Y3$taxon)[i], distance = Y.d)
        #         sv.lst[[length(sv.lst)+1]] <- out
        # }
        # Y4 <- rbindlist(sv.lst)
        # 
        # 
        # 
        # ggplot(Y2, aes(A,B)) + 
        #         geom_point(aes(fill = season), shape = 21)  + 
        #         geom_text(aes(label = taxon))
}
unlist(spring_indicators) |> table() |> sort()
unlist(non_spring_indicators) |> table() |> sort()

# save data -------------------------------------------