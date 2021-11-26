### ————————————————————————————— ###
### ——— Evaluate  typologies  ——— ### 
### ——— Temporally Aggregated ——— ### 
### ————————————————————————————— ###

# ————————————————————————————— #
# date:
#               07.07.21
# files in:  
#               -> 09_sxs_genus_typology_wo_bio.rds
# files out:  
#               <- temporal_aggregation/auxilliary/10_cluster.eval.obj.rds
#               <- temporal_aggregation/auxilliary/10_cluster.eval.obj_random.rds
#               <- temporal_aggregation/auxilliary/10_gensil_output.rds
#               <- temporal_aggregation/auxilliary/10_gensil_output_random.rds
#               <- temporal_aggregation/10_class_eval_temporal_aggregation.rds
# Purpose: 
#               Compute cluster metrics for the different typologies.   
# ————————————————————————————— #

# setup ---------------------------------------------------------------------------------------------------------------------------------------------------
pacman::p_load(
        beepr, 
        data.table, 
        dplyr, 
        indicspecies, 
        fpc,
        future.apply, 
        ggplot2,
        magrittr,
        parallelDist,
        purrr,
        sf,
        tidyr
)

#- functions 
source("~/my documents/R/functions/genmean.R")
source("~/my documents/R/functions/mdist.R")
source("~/my documents/R/functions/silgen.R")
source("~/my documents/R/functions/call_gensil.R")

# load data  ----------------------------------------------------------------------------------------------------------------------------------------------
data <- readRDS("data/temporal_aggregation/09_sxs_genus_typology_with_bio.rds")
similaitry.matrix <- readRDS("data/temporal_aggregation/auxilliary/09_distance_dice.rds")


# analysis --------------------------------------------------------------------------
#- non taxa columns (ntc)
ntc <- which(names(data) %in% c("gr_sample_id", "brt20", "brt12", "illies", "bgr", "gloric", "bio4", "bio7"))
#- typology and classification columns (tcc)
tcc <- ntc[-1]

# ————————————————————————————— #
# ——— Random Classification ——— # 
# ————————————————————————————— #

#- random number generation seed 
set.seed(123)
group_sizes <- data[, lapply(.SD, uniqueN), .SDcols = tcc]
n_types     <- sample(min(group_sizes):max(group_sizes), 25, replace = TRUE)
null.types  <- lapply(1:25, function(x) sample(1:n_types[x], size = nrow(data), replace = TRUE))
names(null.types) <- paste0("null", 1:25)

#- clean up
rm(group_sizes, n_types)
gc()

# —————————————————————————— #
# ——— Cluster Evaluation ——— # 
# —————————————————————————— #
#- actual data 
cluster.eval.obj <-
        lapply(1:length(tcc), function(x)
                cluster.stats(d          = similaitry.matrix,
                              clustering = as.numeric(as.factor(pull(data, tcc[x])))))
beep()
#- save or load cluster evaluation object 
saveRDS(cluster.eval.obj  , "data/temporal_aggregation/auxilliary/10_cluster.eval.obj.rds")
cluster.eval.obj <- readRDS("data/temporal_aggregation/auxilliary/10_cluster.eval.obj.rds")

#- random classifications 
cluster.eval.obj2 <-
        lapply(1:length(null.types), function(x)
                cluster.stats(d          = similaitry.matrix,
                              clustering = null.types[[x]]))
beep()
#- save or load cluster evaluation object for random classifications
saveRDS(cluster.eval.obj2  , "data/temporal_aggregation/auxilliary/10_cluster.eval.obj_random.rds")
cluster.eval.obj2 <- readRDS("data/temporal_aggregation/auxilliary/10_cluster.eval.obj_random.rds")

cluster.eval.obj3 <- c(cluster.eval.obj, cluster.eval.obj2)

typo_names <- append(names(data)[tcc], paste0("null", 1:25))

#- reshape 
cluster.eval.obj <- 
        lapply(1:length(cluster.eval.obj3), function(x)
                                    data.table(
                                            ch       = cluster.eval.obj3[[x]]$ch,
                                            sw       = cluster.eval.obj3[[x]]$avg.silwidth
                                    )) |> 
        rbindlist()
cluster.eval.obj$typology <- typo_names

rm(cluster.eval.obj2, cluster.eval.obj3)
gc()
# ———————————————————————— #
# ——— Indicator Value  ——— # 
# ———————————————————————— #

sa.eval.obj       <- lapply(seq_along(tcc), function(x)
        strassoc(
                X       = as.matrix(data[,-ntc, with = F]), 
                cluster = as.numeric(as.factor(pull(data, tcc[x]))),
                func    = "r.ind.g"
        )
)
sa.eval.obj2       <- lapply(seq_along(null.types), function(x)
        strassoc(
                X       = as.matrix(data[,-ntc, with = F]), 
                cluster = null.types[[x]],
                func    = "r.ind.g"
        )
)

sa.eval.obj %<>% c(sa.eval.obj2)
sa.eval.obj <- lapply(sa.eval.obj, function(x) apply(x, 1, function(y) y[which.max(y)]))
sa.eval.obj <- sapply(sa.eval.obj, mean)
sa.eval.obj <- data.table(typology = typo_names, 
                           indval   = sa.eval.obj)

rm(sa.eval.obj2)
gc()
# ——————————————————————————————— #
# ——— Classification Strength ——— # 
# ——————————————————————————————— #

ma_similarity <- 1-similaitry.matrix
ma_similarity %<>% as.matrix()

for (i in 1:(length(tcc) + 25)) {
        #- feedback  
        print(i)
        #- initialize loop object in first iteration
        if (i == 1) csi <- c()
        #- typology or classification 
        if (i <= length(tcc)) {
                i.classes <- as.numeric(as.factor(pull(data, tcc[i])))
        } else {
                i.classes <- null.types[[i-length(tcc)]]
        }
        
        i.classes.u <- unique(i.classes)
        # single type
        for (j in seq_along(i.classes.u)) {
                if (j == 1)
                        csj <- c()
                j.id1    <- which(i.classes == i.classes.u[j])
                j.id.n1  <- which(i.classes != i.classes.u[j])
                j.sim1   <-  ma_similarity[j.id1, j.id1]
                j.sim.n1 <-  ma_similarity[j.id1, j.id.n1]
                j.ut     <- j.sim1[upper.tri(j.sim1)]
                j.lt     <- j.sim1[lower.tri(j.sim1)]
                j.ut.n   <- j.sim.n1[upper.tri(j.sim.n1)]
                j.lt.n   <- j.sim.n1[lower.tri(j.sim.n1)]
                csj[j] <- mean(append(j.ut, j.lt)) - mean(append(j.ut.n, j.lt.n))
                rm(list = ls()[grepl(x = ls(), pattern = "^j\\.")])
                rm(j)
        }
        csi[i] <- mean(csj)
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
        rm(i)
        rm(csj)
        gc()
}

rm(ma_similarity)

# ———————————————————————————————————— #
# ——— Generalized Silhouette Width ——— # 
# ———————————————————————————————————— #
dt_distance <- similaitry.matrix
gensil_output <- lapply(X = seq_along(tcc),
                        function(x) call_gensil(as.numeric(as.factor(pull(data, tcc[x]))), p_v = c(-Inf, Inf)))

#- save or load generalized silhouette widths 
saveRDS(gensil_output, "data/temporal_aggregation/auxilliary/10_gensil_output.rds")
gensil_output <- readRDS("data/temporal_aggregation/auxilliary/10_gensil_output.rds")

gensil_output2 <- lapply(X = seq_along(null.types),
                        function(x) call_gensil(null.types[[x]], p_v = c(-Inf, Inf)))

#- save or load generalized silhouette widths of random classifications 
saveRDS(gensil_output2,   "data/temporal_aggregation/auxilliary/10_gensil_output_random.rds")
gensil_output2 <- readRDS("data/temporal_aggregation/auxilliary/10_gensil_output_random.rds")

#- combine typologies and random classifications 
gensil_output %<>% c(gensil_output2)

gensil_dt <- rbindlist(gensil_output)

gensil_dt[, typology := rep(typo_names, each = 2)]
gensil_dt %<>% 
        mutate(p2 = case_when(p == -Inf ~"min",
                              p == Inf ~ "max")) %>%  
        pivot_wider(id_cols = typology, names_from = p2, values_from = silhouette) 

setDT(gensil_dt)

rm(dt_distance, gensil_output, gensil_output2, typo_names)
gc()

# ———————————————————— #
# ——— Combine Data ——— # 
# ———————————————————— #

final    <- cluster.eval.obj[sa.eval.obj, on = "typology"]
final    <- final[gensil_dt, on = "typology"]
final$cs <- csi
final %<>% pivot_longer(!"typology")
setDT(final)

#- give all ranodom classifications the same name 
final[grepl("null", typology), typology := "null"]

# ———————————————————— #
# ——— Plot Results ——— # 
# ———————————————————— #

ggplot(final, aes(x = typology, y = value, col = typology)) + geom_point() + facet_wrap(.~name, scale = "free")

final <- final[typology != "brt4"]

# save to file  ---------------------------------------------------------------------
saveRDS(final, "data/temporal_aggregation/10_class_eval_temporal_aggregation.rds")

