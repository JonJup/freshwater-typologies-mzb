# ————————————————————————————— #
# ——— Biological Clusters   ——— # 
# ————————————————————————————— #

# ————————————————
# date:
#       (26,28).07.21
# files in: 
#       -> distance tables (*date*_well_sampled_data.rds)
# files out:
#       <- the lookup table, updated with biological clusters (*date*_lookup_w_bio.rds)
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Create biological cluster solutions 
# ————————————————



# SETUP -----------------------------------------------------------------------------
pacman::p_load(cluster, 
               data.table,
               dplyr,
               fpc,
               ggplot2,
               magrittr,
               parallelDist,
               stringr,
               tidyr)

# LOAD DATA -------------------------------------------------------------------------
distance.jaccard <- readRDS("data/combined_data/05_2021-07-28_distance_jaccard.rds")
distance.dice    <- readRDS("data/combined_data/05_2021-07-28_distance_dice.rds")
distance.ochiai  <- readRDS("data/combined_data/05_2021-07-28_distance_ochiai.rds")
lookup           <- readRDS("data/combined_data/04_2021-07-28_lookup.rds")
subsets          <- readRDS("data/combined_data/04_2021-07-28_subsets.rds")


#- name list elements 
names(distance.jaccard) <- paste0("dist.jacc_", c("dat.genu", "dat.fami", "dat.gege", "dat.fage", "dat.gefr", "dat.fafr"))
names(distance.dice)    <- paste0("dist.dice_", c("dat.genu", "dat.fami", "dat.gege", "dat.fage", "dat.gefr", "dat.fafr"))
names(distance.ochiai)  <- paste0("dist.ochi_", c("dat.genu", "dat.fami", "dat.gege", "dat.fage", "dat.gefr", "dat.fafr"))

distance.list <- append(distance.jaccard, append(distance.dice, distance.ochiai))

# CREATE CLUSTER --------------------------------------------------------------------
#- beta cluster 
cluster.obj <- lapply(distance.list, agnes, method = "flexible", par.method = 0.625)
#- convert to hclust class
cluster.obj %<>% lapply(as.hclust)
#- Assign each observation to a group. Group IDs depend on the tree height. Tree height is varied between 2 and 30 final nodes. 
for (i in 1:length(cluster.obj)){
        #- In first round, initiate a storage object for the loop results.
        if (i == 1) cut.lst <- list()
        i.name <- names(cluster.obj)[i]
        i.clus.eval <- lapply(2:30, function(x) cutree(cluster.obj[[i]], k = x))
        names(i.clus.eval) <- paste0(i.name,"_nclu.", 2:30)
        cut.lst %<>% append(i.clus.eval)
        print(i)
        rm(list = ls()[grepl("^i\\.", x = ls())])
        rm(i)
}



#- Compute cluster statistics
#- this loops over the elements of cut.lst which contains the cutree objects. 
for (i in seq_along(cut.lst)){
        
        #- In first round, initiate a storage object for the loop results. 
        if (i == 1) clst.eval = list()
        
        #- Extract information about this loops object from its name. 
        i.name <- names(cut.lst)[i]
        i.data.dist <- str_remove(i.name, "_nclu.*")
        i.dist <- str_extract(i.name, "dist\\.[a-z]*_") |> str_remove("dist\\.") |> str_remove("_")
        i.data <- str_extract(i.name, "dat\\.[a-z]*_")  |> str_remove("dat\\.") |> str_remove("_")
        i.nclu <- str_extract(i.name, "nclu\\.[0-9]*")  |> str_remove("nclu\\.")
        
        i.data.dist = distance.list[[which(names(distance.list) == i.data.dist)]]
        
        if (anyNA(c(i.dist, i.data, i.nclu))){
                print("Either distance, taxonomic level, beta parameter or the number of clusters was not recognized")
                break()
        }
        
        #- Evaluate clustering 
        i.eval.obj <- cluster.stats(d = i.data.dist, clustering = cut.lst[[i]])
        #- Reshape 
        i.eval.obj <- data.table(
                ch = i.eval.obj$ch,
                asw = i.eval.obj$avg.silwidth,
                n.cluster = i.nclu,
                distance.metric = i.dist,
                data = i.data
        )
        
        #- Add to storage object. 
        clst.eval[[i]] <- i.eval.obj
        
        #- In the last round, bind all results. 
        if (i == length(cut.lst)) clst.eval = rbindlist(clst.eval)
        
        #- clean up 
        print(paste(i, Sys.time())) 
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        rm(i)
}

#- reshape 
clst.eval2 <- 
        pivot_longer(clst.eval, cols = c(ch, asw)) |> 
        mutate(n.cluster = as.numeric(n.cluster))

#- save results of bio cluster. 
saveRDS(clst.eval2, paste0("data/auxilliary/",Sys.Date(),"_biocluster_results.rds"))


# DETERMINE OPTIMAL CLUSTERS --------------------------------------------------------

#  ———— GENUS ————— #  
clst.eval2 |>
        filter(data == "genu") |>
        ggplot(aes(x = n.cluster, y = value)) +
        geom_line(aes(col = distance.metric)) +
        geom_point(aes(col = distance.metric)) +
        facet_wrap( ~ name, scale = "free")
#->  genus:4 and 11; both ochiai
fin.ge.4 <- cutree(cluster.obj[["dist.ochi_dat.genu"]], k = 4) 
fin.ge.11 <- cutree(cluster.obj[["dist.ochi_dat.genu"]], k = 11)
subsets$genus$ge.bio.4 <- fin.ge.4
subsets$genus$ge.bio.11 <- fin.ge.11

add <- subsets$genus[, c("gr_sample_id", "ge.bio.4", "ge.bio.11")]
lookup <- add[lookup, on = "gr_sample_id"]

#  ———— FAMILY ————— # 
clst.eval2 |>
        filter(data == "fami") |>
        ggplot(aes(x = n.cluster, y = value)) +
        geom_line(aes(col = distance.metric)) +
        geom_point(aes(col = distance.metric)) +
        facet_wrap( ~ name, scale = "free")
# ———> family:  9 dice 
clst <- cutree(cluster.obj[["dist.dice_dat.fami"]], k = 9) 
subsets$family$fa.bio.9 <- clst
add <- subsets$family[, c("gr_sample_id", "fa.bio.9")]
lookup <- add[lookup, on = "gr_sample_id"]

#  ———— GENUS GERMAN ————— # 
clst.eval2 |>
        filter(data == "gege") |>
        ggplot(aes(x = n.cluster, y = value)) +
        geom_line(aes(col = distance.metric)) +
        geom_point(aes(col = distance.metric)) +
        facet_wrap( ~ name, scale = "free")
# ———> genus german 3, 6, Ochiai
clst1 <- cutree(cluster.obj[["dist.ochi_dat.gege"]], k = 3) 
clst2 <- cutree(cluster.obj[["dist.ochi_dat.gege"]], k = 6) 
subsets$genus.german$gege.bio.3 <- clst1
subsets$genus.german$gege.bio.6 <- clst2
add <- subsets$genus.german[, c("gr_sample_id", "gege.bio.3", "gege.bio.6")]
lookup <- add[lookup, on = "gr_sample_id"]

#  ———— FAMILY GERMAN ————— # 
clst.eval2 |>
        filter(data == "fage") |>
        ggplot(aes(x = n.cluster, y = value)) +
        geom_line(aes(col = distance.metric)) +
        geom_point(aes(col = distance.metric)) +
        facet_wrap( ~ name, scale = "free")
# ———> family german 4, dice
clst <- cutree(cluster.obj[["dist.dice_dat.fage"]], k = 4) 
subsets$family.german$fage.bio.4 <- clst
add <- subsets$family.german[, c("gr_sample_id", "fage.bio.4")]
lookup <- add[lookup, on = "gr_sample_id"]

#  ———— GENUS FRANCE ————— # 
clst.eval2 |>
        filter(data == "gefr") |>
        ggplot(aes(x = n.cluster, y = value)) +
        geom_line(aes(col = distance.metric)) +
        geom_point(aes(col = distance.metric)) +
        facet_wrap( ~ name, scale = "free")

# ———> Genus-France: 4 groups, Ochiai distance 
clst <- cutree(cluster.obj[["dist.ochi_dat.gefr"]], k = 4) 
subsets$genus.french$gefr.bio.4 <- clst
add <- subsets$genus.french[, c("gr_sample_id", "gefr.bio.4")]
lookup <- add[lookup, on = "gr_sample_id"]

#  ———— GENUS FAMILY ————— # 
clst.eval2 |>
        filter(data == "fafr") |>
        ggplot(aes(x = n.cluster, y = value)) +
        geom_line(aes(col = distance.metric)) +
        geom_point(aes(col = distance.metric)) +
        facet_wrap( ~ name, scale = "free")

# ———> Family-France: 4 groups; Ochiai distance 

clst   <- cutree(cluster.obj[["dist.ochi_dat.fafr"]], k = 4) 
subsets$family.french$fafr.bio.4 <- 
        clst
add    <- subsets$family.french[, c("gr_sample_id", "fafr.bio.4")]
lookup <- add[lookup, on = "gr_sample_id"]

# SAVE TO FILE  ----------------------------------------------------------------------
lookup |> saveRDS(paste0("data/combined_data/",Sys.Date(),"_lookup_w_bio.rds"))


