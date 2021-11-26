### ——————————————————————————————— ###
### ——— New Biological Typology ——— ### 
### ——————————————————————————————— ###

# ————————————————
# date:
#       (5,6).07.21
# files in: 
#       -> 09_sxs_genus_typology_wo_bio.rds
# files out:
#       <-  map_of_biotypology.html
#       <-  temp/biocluster_beta_cluster.rds
#       <-  temp/biocluster_similarity_matrix.rds
#       <-  temp/biocluster_similarity_matrix.rds
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
        sf,
        tidyr,
        vegan
)

#- functions 
source("~/my documents/R/functions/genmean.R")
source("~/my documents/R/functions/mdist.R")
source("~/my documents/R/functions/silgen.R")

# input  ----------------------------------------------------------------------------
#- data 
sxs <- readRDS("data/09_sxs_genus_typology_wo_bio.rds") |> setDT()
dat <- readRDS("data/01_all_mzb_combined.rds")

# preparation  ----------------------------------------------------------------------

# ———————————————————— #
# — Remove rare taxa — #
# ———————————————————— # 
#- non taxa columns 
ntc <- which(names(sxs) %in% c("gr_sample_id", "ls20", "ls12", "illies", "eea", "gloric"))

#- determine impoverished sites 
rowSums(sxs[, -ntc, with = F]) |> summary()
rowSums(sxs[, -ntc, with = F]) |> sort(decreasing = TRUE)

#- check the ultradiverse sites
sxs$gr_sample_id[which(rowSums(sxs[, -ntc, with = F]) == 284)]

sxs

#- determine cutoff 
n.samples <- nrow(sxs)
one.percent <- n.samples/100
#- rare: < 123 samples 
rare <- names(which(colSums(sxs[, -ntc, with = F])<one.percent))
paste("Rare genera constitue", round(length(rare)/ncol(sxs[, -ntc, with = F]) * 100,2), "% of all taxa")
sxs  <- sxs[, setdiff(names(sxs), rare), with = F]
rm(rare);gc()
#- update ntc 
ntc <- which(names(sxs) %in% c("gr_sample_id", "ls20", "ls12", "illies", "eea", "gloric"))

#- are there any empty sites now? 

rowSums(sxs[, -ntc, with = F]) |> sort()



sxs$gr_sample_id[which(rowSums(sxs[, -ntc, with = F])==2)]

# ————————————————————— #
# — Remove rare types — #
# ————————————————————— # 
sxs %<>%
        filter(ls20 != "RT17") %>% 
        filter(!gloric %in%  c(17, 22)) %>% 
        filter(!illies %in%  c("Fenno-scandian shield")) 

# analysis --------------------------------------------------------------------------

# ————————————————————————————— #
# — Flexible Beta Clustering  — #
# ————————————————————————————— # 

#- distance matrix 
similaitry.matrix <- parallelDist(x = as.matrix(sxs[,-ntc, with = F]), method = "dice", threads = 5)
saveRDS(similaitry.matrix, "data/temp/biocluster_similarity_matrix.rds")
similaitry.matrix <- readRDS("data/temp/biocluster_similarity_matrix.rds")
#- beta cluster 
cluster.obj <- agnes(x = similaitry.matrix,
                     method = "flexible",
                     par.method = 0.625)
cluster.obj %<>% as.hclust()
saveRDS(cluster.obj, "data/temp/biocluster_beta_cluster.rds")
cluster.obj     <- readRDS("data/temp/biocluster_beta_cluster.rds")
cluster.obj.cut <- lapply(2:30, function(x) cutree(cluster.obj, k = x))

# ——————————————————————— #
# — Cluster Evaluation  — #
# ——————————————————————— # 

sil.obj  <- lapply(cluster.obj.cut, function(x) silhouette(x = x, dist = similaitry.matrix)[,3])
sil.obj2 <- sapply(sil.obj, mean) 

cluster.eval.obj <- lapply(1:29, 
                           function(x) cluster.stats(
                                   d = similaitry.matrix, 
                                   clustering = cluster.obj.cut[[x]]
                                                     )
                           );beepr::beep()
ch.obj   <- sapply(cluster.eval.obj, function(x) x$ch)
avbe.obj <- sapply(cluster.eval.obj, function(x) x$average.between)
avwi.obj <- sapply(cluster.eval.obj, function(x) x$average.within)
cs       <- avbe.obj - avwi.obj

plot(cs, type = "p")
plot(sil.obj2)
plot(ch.obj)
plot(avbe.obj)
plot(avwi.obj)


# —————————————————— #
# — Final Cluster  — #
# —————————————————— # 

cluster.obj.3 <- cutree(cluster.obj, k = 3) 
cluster.obj.4 <- cutree(cluster.obj, k = 4) 
cluster.obj.5 <- cutree(cluster.obj, k = 5) 
cluster.obj.6 <- cutree(cluster.obj, k = 6) 
cluster.obj.7 <- cutree(cluster.obj, k = 7) 
cluster.obj.8 <- cutree(cluster.obj, k = 8) 
cluster.obj.9 <- cutree(cluster.obj, k = 9) 

sxs$bio3   <- cluster.obj.3
sxs$bio4   <- cluster.obj.4
sxs$bio5   <- cluster.obj.5
sxs$bio6   <- cluster.obj.6
sxs$bio7   <- cluster.obj.7
sxs$bio8   <- cluster.obj.8
sxs$bio9   <- cluster.obj.9

# ———————————————————— #
# — Map of Clusters  — #
# ———————————————————— # 

dat2 <- dat[gr_sample_id %in% sxs$gr_sample_id]
dat2 <- unique(dat2, by = "gr_sample_id")
dat2 <- st_as_sf(dat2)
dat2 <- left_join(x = dat2, 
                  y = select(sxs, c("gr_sample_id", "bio.cluster")))
test <- dat2
#- This options needs to be set to save the map. 
mapviewOptions(fgb = FALSE)
#- create map of sampling sites 
tes.map <- mapview(test, zcol = "bio.cluster")
#- save map to file 
mapshot(x   = tes.map, 
        url = "fig/map_of_biotypology.html")

# save to file ----------------------------------------------------------------------

saveRDS(sxs, "data/09_sxs_genus_typology_with_bio.rds")
