# ——————————————————————————— #
# ——— Evaluate Clusters   ——— # 
# ——————————————————————————— #

# ————————————————
# date:
#       27.07.21
# files in: 
#       -> distance tables (*date*_well_sampled_data.rds)
# files out:
#       <- 
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Evaluate all the different clusterings
# ————————————————

# TEST 
# Dont Repeat Yourself  
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# Design by Contract 
# Fail fast/ defensive programming 

# setup ---------------------------------------------------------------------------------------------------------------------------------------------------
pacman::p_load(
        data.table, 
        dplyr, 
        indicspecies, 
        fpc,
        future.apply, 
        ggplot2,
        magrittr,
        purrr,
        sf,
        stringr,
        tidyr
)

#- functions
source("~/my documents/R/functions/call_gensil.R")
source("~/my documents/R/functions/mdist.R")
source("~/my documents/R/functions/genmean.R")
source("~/my documents/R/functions/silgen.R")

# load data -------------------------------------------------------------------------
data <- readRDS("data/09_sxs_genus_typology_with_bio.rds")

# prepare data  ----------------------------------------------------------------

#- sites x species table as matrix and without typology columns
setDT(data)
ma_data <- 
        copy(data) |> 
        {\(x) x[][, c("gr_sample_id", "ls20", "ls12", "illies", "eea", "gloric", "bio3", "bio4", "bio5", 
                      "bio6", "bio7","bio8","bio9") := NULL]}()  |>  
        as.matrix()
#- list of classifications - those that are strings are transformed to numbers 
ls_class  <-  list(
        ls12   = as.numeric(factor(data$ls12)),
        ls20   = as.numeric(factor(data$ls20)),
        gloric = data$gloric,
        illies = as.numeric(factor(data$illies)),
        eea    = as.numeric(factor(data$eea)),
        bio3    = data$bio3,
        bio4    = data$bio4,
        bio5    = data$bio5,
        bio6    = data$bio6,
        bio7    = data$bio7,
        bio8    = data$bio8,
        bio9    = data$bio9
)
#- compute dice distance matrix of data 
dt_distance    <- readRDS("data/temp/biocluster_similarity_matrix.rds")
dt_similiarity <- 1 - dt_distance

# ————————————————————————————— #
# ——— Random Classification ——— # 
# ————————————————————————————— #

#- random number generation seed 
set.seed(123)

group_sizes <- data[, lapply(.SD, uniqueN), .SDcols = c("ls12", "ls20", "gloric", "eea", "illies", "bio3",
                                                        "bio4", "bio5", "bio6", "bio7","bio8","bio9")]
n_types     <- sample(min(group_sizes):max(group_sizes), 25, replace = TRUE)
null.types  <- lapply(1:25, function(x) sample(1:n_types[x], size = nrow(ma_data), replace = TRUE))
names(null.types) <- paste0("null", 1:25)
ls_class <- append(ls_class, null.types)
#- clean up
rm(group_sizes, n_types, null.types)
gc()

# compute cluster metrics ---------------------------------------------------------------------------------------------------------------------------------

# ——————————————————————————— #
# ——— cluster statistics  ——— # 
# ——————————————————————————— #

cluster.eval.obj <- lapply(1:length(ls_class),
                           function(x)
                                   cluster.stats(d          = dt_distance,
                                                 clustering = ls_class[[x]]));beepr::beep()
#- save to file 
saveRDS(cluster.eval.obj, "data/temp/11_eval_cluster.eval.obj.rds")
#- reshape 
cluster.eval.obj2 <- lapply(1:length(cluster.eval.obj),
                            function(x)
                                    data.table(
                                            ch       = cluster.eval.obj[[x]]$ch,
                                            sw       = cluster.eval.obj[[x]]$avg.silwidth,
                                            typology = names(ls_class)[x]
                                    ))
cluster.eval.obj3 <- rbindlist(cluster.eval.obj2)        

# ———————————————————————— #
# ——— Indicator Value  ——— # 
# ———————————————————————— #

sa.eval.obj       <- lapply(1:length(ls_class), function(x)
        strassoc(
                X       = ma_data, 
                cluster = ls_class[[x]],
                func    = "r.ind.g"
        )
)
sa.eval.obj2 <- lapply(sa.eval.obj, function(x) apply(x, 1, function(y) y[which.max(y)]))
sa.eval.obj2 <- sapply(sa.eval.obj2, mean)
sa.eval.obj3 <- data.table(typology = names(ls_class), 
                           indval   = sa.eval.obj2)


# ——————————————————————————————— #
# ——— Classification Strength ——— # 
# ——————————————————————————————— #

ma_similarity <- as.matrix(dt_similiarity)

for (i in seq_along(ls_class)) {
        print(i)
        if (i == 1) csi <- c()
        i.classes <- ls_class[[i]]
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

# ———————————————————————————————————— #
# ——— Generalized Silhouette Width ——— # 
# ———————————————————————————————————— #

gensil_output <- lapply(X = 1:length(ls_class),
                        function(x) call_gensil(ls_class[[x]]))

saveRDS(gensil_output, "data/temp/11_eval_gensil_output.rds")

gensil_dt <- rbindlist(gensil_output)

typo_names <- 
        append (c("brt12", "brt20", "gloric", "illies", "eea"), paste0("bio", 3:9)) |> 
        append(paste0("null",1:25))

gensil_dt[, typology := rep(typo_names, each = 6)]
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

join1 <- cluster.eval.obj3[sa.eval.obj3, on = "typology"]
join1[typology == "ls12", typology := "brt12"]
join1[typology == "ls20", typology := "brt20"]
join1 <- join1[gensil_dt, on = "typology"]
join1$cs <- csi
join1[str_detect(typology , "null"), typology  := "null"]
join1 %<>% pivot_longer(!c("typology"))
setDT(join1)

ggplot(join1, aes(x = typology, y = value, col = typology)) + geom_point() + facet_wrap(.~name, scale = "free")

#- restrict to bio7 

join1 <- join1[!typology %in% paste0("bio", c(3:6,8:9))]
join1 <- join1[!name %in% c("arithmetic", "m2", "harmonic")]
join1[typology == "bio7", typology := "biological classification"]

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(join1, "data/10_class_eval_mzb.rds")


# OLD CODE  -------------------------------------------------------------------------

#- silhouette width 
# sil.obj          <- lapply(ls_class, function(x) silhouette(x = x, dist = dt_distance))
# sil.obj2         <- lapply(1:length(sil.obj),
#                            function(x)
#                                    data.table(
#                                            cluster  = sil.obj[[x]][, 1],
#                                            neighbor = sil.obj[[x]][, 2],
#                                            sw       = sil.obj[[x]][, 3],
#                                            typology = names(ls_class)[x]
#                                    ))
# sil.obj3 <- rbindlist(sil.obj2)        
# sil.obj3[, sw := mean(sw), by = "typology"]
# sil.obj3 <- unique(sil.obj3, by = "typology")
# sil.obj3[, c("cluster", "neighbor") := NULL]


# IndVal ------------------------------------------------------------------

## -------------------- ##
## -- COMPUTE INDVAL -- ##
## -------------------- ##

## -- this crashed R once and will hence be run in chunks of ten. 
## -- Outputs are to be saved as temporary files after each run. 
## -- After the second run, check that results can be combined. 
## -- CHECKED: works 
## -- takes a long time (days)! 
# 
# ## -- what is the minimum p_value? 
# 0.05/602 < 1/15000
# 0.05/602 < 2/25000
# plan(multiprocess, workers = 4)
# for (i in 0:10){
#         
#         lp_name = paste0("results_indval",i+1)
#         from = paste0(i,"1")   %>% as.numeric()
#         to   = paste0(i+1,"0") %>% as.numeric()
#         if (i == 10)
#                 to = 107
#         assign(x = lp_name,
#                value = future_lapply(
#                        X = ls_class[from:to],
#                        FUN = function(x)
#                                multipatt(
#                                        ma_data,
#                                        cluster = x,
#                                        control = how(nperm = 25000),
#                                        max.order = 1,
#                                        print.perm = T
#                                )
#                )
#         )
#         xx = get(lp_name)
#         save_name = paste0("data/temp/eval_clust_indval_",i+1,".rds")
#         saveRDS(xx, save_name)
# }
# 
# ## ------------------------------------ ##
# ## -- START FROM HERE TO LOAD INDVAL -- ## 
# ## ------------------------------------ ## 
# 
# ## -- list files to load 
# files = dir("data/temp/", pattern = "eval_clust_indval")
# 
# ## -- load files in loop 
# for (i in seq_along(files)){
#         file_number = gsub(pattern = "eval_clust_indval_", replacement = "", x = files[i])
#         file_number = gsub(pattern = ".rds", replacement = "", x = file_number)
#         file_number = gsub(pattern = "_", replacement = "", x = file_number)
#         assign(
#                 x = paste0("results_indval",file_number),
#                 value = 
#                         readRDS(file.path("data/temp", files[i]))
#         )
#         rm(i, file_number);gc()
# }
# 
# ## -- bind files to list 
# all_results_indval = c(
#         results_indval1, 
#         results_indval2,
#         results_indval3,
#         results_indval4,
#         results_indval5,
#         results_indval6,
#         results_indval7,
#         results_indval8,
#         results_indval9,
#         results_indval10,
#         results_indval11
# )
# 
# rm(files,results_indval1, results_indval2,results_indval3,results_indval4,results_indval5,results_indval6,results_indval7,results_indval8,results_indval9,results_indval10,results_indval11)
# 
# ## --------------------------------- ##
# ## -- ADJUST FOR MULTIPLE TESTING -- ##
# ## --------------------------------- ##
# 
# ## -- reshape from indicator class format 
# all_results_indval2 = lapply(all_results_indval, 
#                              function(x) data.table(taxon  = rownames(x$A), 
#                                                     pvalue = x$sign$p.value,
#                                                     indval = x$sign$stat))
# 
# ## -- drop taxa that do not occur in the river type 
# all_results_indval2 %<>%
#         lapply(function(x) x[indval != 0])
# 
# ## -- number of tests = number of taxa. 
# ## -- Family-wise error only corrected within each river type
# n_tests =  lapply(all_results_indval2, nrow)
# 
# ## -- add a variable with the number of tests to each table 
# all_results_indval2 = map(.x = 1:length(all_results_indval2),
#                           .f = ~ all_results_indval2[[.x]][, n_tests := n_tests[.x]])
# 
# all_results_indval2 %<>%
#         ## -- add bonferroni p-value 
#         lapply(function(x)
#                 x[, bonferroni_p := 0.05 / n_tests]) %>%
#         ## -- compare to bonferroni p-value 
#         lapply(function(x)
#                 x[, indicator := pvalue <= bonferroni_p])
# 
# ## -- compute the IndVal Statistic 
# indval_statistic = lapply(all_results_indval2, function(x) x[significant == TRUE, sum(stat)/ncol(ma_data)])