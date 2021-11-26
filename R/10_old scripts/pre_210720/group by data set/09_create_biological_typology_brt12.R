### ————————————————————————————————— ###
### ——— Biological Classification ——— ### 
### ——— Group by data set         ——— ### 
### ————————————————————————————————— ###

# ————————————————
# date:
#       09.07.21
# files in: 
#       -> 09_sxs_genus_typology_wo_bio.rds
# files out:
#       <-  group by data set/auxilliary/09_biocluster_similarity_matrix.rds
#       <-  group by data set/auxilliary/09_beta_cluster.rds
#       <-  group by data set/auxilliary/09_cluster.eval.obj.rds
#       <-  group by data set/auxilliary/09_gensil_output.rds
#       <-  group by data set/09_sxs_genus_typology_with_bio.rds
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Create a new biological clustering. This time with the correct data set. 
# ————————————————


# setup ----------------------------------------------------------------------------
pacman::p_load(
        cluster, 
        data.table,
        dplyr,
        fpc,
        indicspecies,
        ggplot2, 
        magrittr,
        mapview,
        parallelDist,
        purrr,
        sf,
        tidyr,
        vegan
)

#- functions 
source("~/my documents/R/functions/genmean.R")
source("~/my documents/R/functions/mdist.R")
source("~/my documents/R/functions/silgen.R")
source("~/my documents/R/functions/call_gensil.R")

# input  ----------------------------------------------------------------------------
#- data 
sxs <- readRDS("data/group by data set/07_sxs_genus_brt12.rds")
dat <- readRDS("data/01_all_mzb_combined.rds")

# preparation  ----------------------------------------------------------------------

# ———————————————————— #
# — Remove rare taxa — #
# ———————————————————— # 
#- non taxa columns 
ntc <- which(names(sxs) %in% c("gr_sample_id", "data.set"))
#- rare: < 5 samples 
rare <- names(which(colSums(sxs[, -ntc, with = F])<5))
sxs  <- sxs[, setdiff(names(sxs), rare), with = F]
rm(rare);gc()
#- update ntc 
ntc <- which(names(sxs) %in% c("gr_sample_id", "data.set"))

# analysis --------------------------------------------------------------------------

# ————————————————————————————— #
# — Flexible Beta Clustering  — #
# ————————————————————————————— # 

#- compute distance matrix 
similaitry.matrix <- parallelDist(x = as.matrix(sxs[,-ntc, with = F]), method = "dice", threads = 12)
beepr::beep()
#- save or load distance matrix 
saveRDS(similaitry.matrix,   "data/group by data set/auxilliary/09_biocluster_similarity_matrix_brt12.rds")
similaitry.matrix <- readRDS("data/group by data set/auxilliary/09_biocluster_similarity_matrix_brt12.rds")

#- compute beta cluster 
cluster.obj <- agnes(x = similaitry.matrix,
                     method = "flexible",
                     par.method = 0.625)
cluster.obj %<>% as.hclust()
beepr::beep()
#- save or load beta clusters 
saveRDS(cluster.obj,       "data/group by data set/auxilliary/09_beta_cluster.rds")
cluster.obj     <- readRDS("data/group by data set/auxilliary/09_beta_cluster.rds")

#- cut tree 
cluster.obj.cut <- lapply(2:30, function(x) cutree(cluster.obj, k = x))

# ——————————————————————————— #
# ——— cluster statistics  ——— # 
# ——————————————————————————— #

cluster.eval.obj <- lapply(seq_along(cluster.obj.cut),
                           function(x)
                                   cluster.stats(d          = similaitry.matrix,
                                                 clustering = cluster.obj.cut[[x]]));beepr::beep()

#- save or load cluster.eval.obj
saveRDS(cluster.eval.obj, "data/group by data set/auxilliary/09_cluster.eval.obj.rds")
cluster.eval.obj <- readRDS("data/group by data set/auxilliary/09_cluster.eval.obj.rds")


#- reshape 
cluster.eval.obj <- lapply(1:length(cluster.eval.obj),
                            function(x)
                                    data.table(
                                            ch       = cluster.eval.obj[[x]]$ch,
                                            sw       = cluster.eval.obj[[x]]$avg.silwidth,
                                            typology = x+1
                                    ))
cluster.eval.obj <- rbindlist(cluster.eval.obj)        

# ———————————————————————— #
# ——— Indicator Value  ——— # 
# ———————————————————————— #

sa.eval.obj       <- lapply(1:length(cluster.obj.cut), function(x)
        strassoc(
                X       = as.matrix(sxs[,-ntc, with = F]), 
                cluster = cluster.obj.cut[[x]],
                func    = "r.ind.g"
        )
)
sa.eval.obj <- lapply(sa.eval.obj, function(x) apply(x, 1, function(y) y[which.max(y)]))
sa.eval.obj <- sapply(sa.eval.obj, mean)
sa.eval.obj <- data.table(typology = 2:30, 
                           indval   = sa.eval.obj)


# ——————————————————————————————— #
# ——— Classification Strength ——— # 
# ——————————————————————————————— #

ma_similarity <- 1-similaitry.matrix
ma_similarity %<>% as.matrix()

for (i in seq_along(cluster.obj.cut)) {
        print(i)
        if (i == 1) csi <- c()
        i.classes <- cluster.obj.cut[[i]]
        i.classes.u <- unique(i.classes)
        # single type
        for (j in seq_along(i.classes.u)) {
                if (j == 1)
                        csj <- c()
                id1    <- which(i.classes == i.classes.u[j])
                id.n1  <- which(i.classes != i.classes.u[j])
                sim1   <-  ma_similarity[id1, id1]
                sim.n1 <-  ma_similarity[id1, id.n1]
                ut     <- sim1[upper.tri(sim1)]
                lt     <- sim1[lower.tri(sim1)]
                ut.n   <- sim.n1[upper.tri(sim.n1)]
                lt.n   <- sim.n1[lower.tri(sim.n1)]
                csj[j] <- mean(append(ut, lt)) - mean(append(ut.n, lt.n))
        }
        csi[i] <- mean(csj)
}

rm(ma_similarity)

# ———————————————————————————————————— #
# ——— Generalized Silhouette Width ——— # 
# ———————————————————————————————————— #
dt_distance <- similaitry.matrix
gensil_output <- lapply(X = 1:length(cluster.obj.cut),
                        function(x) call_gensil(cluster.obj.cut[[x]], p_v = c(-Inf,Inf)))

#- save or load gensil_output 
saveRDS(gensil_output, "data/group by data set/auxilliary/09_gensil_output.rds")
gensil_output <- readRDS("data/group by data set/auxilliary/09_gensil_output.rds")

gensil_dt <- rbindlist(gensil_output)

typo_names <- 2:30

gensil_dt[, typology := rep(typo_names, each = 2)]
gensil_dt %<>% 
        mutate(p2 = case_when(p == -Inf ~"min",
                              p == -2 ~ "m2",
                              p == -1 ~ "harmonic",
                              p == 1 ~ "arithmetic",
                              p == 2 ~ "quadratic",
                              p == Inf ~ "max")) %>%  
        pivot_wider(id_cols = typology, names_from = p2, values_from = silhouette) 
setDT(gensil_dt)

# ———————————————————— #
# ——— Combine Data ——— # 
# ———————————————————— #

join1 <- cluster.eval.obj[sa.eval.obj, on = "typology"]
join1 <- join1[gensil_dt, on = "typology"]
join1$cs <- csi
join1 %<>% pivot_longer(!c("typology"))
setDT(join1)

ggplot(join1, aes(x = typology, y = value, col = typology)) + geom_point() + facet_wrap(.~name, scale = "free")

# —————————————————— #
# — Final Cluster  — #
# —————————————————— #

sxs$bio7   <- cutree(cluster.obj, k = 7)
sxs$bio12  <- cutree(cluster.obj, k = 12)

# ———————————————————— #
# — Map of Clusters  — #
# ———————————————————— # 


# dat2 <- dat[gr_sample_id %in% sxs$gr_sample_id]
# dat2 <- unique(dat2, by = "gr_sample_id")
# dat2 <- st_as_sf(dat2)
# dat2 <- left_join(x = dat2, 
#                   y = select(sxs, c("gr_sample_id", "bio6")))
# dat2 <- left_join(x = dat2, 
#                   y = select(sxs, c("gr_sample_id", "bio10")))
# #- This options needs to be set to save the map. 
# mapviewOptions(fgb = FALSE)
# #- create map of sampling sites 
# tes.map <- mapview(dat2, zcol = "bio6")
# #- save map to file 
# mapshot(x   = tes.map, 
#         url = "fig/group by data set/map_of_biotypology6.html")

# save to file ----------------------------------------------------------------------

saveRDS(sxs, "data/group by data set/09_sxs_genus_typology_with_bio.rds")
