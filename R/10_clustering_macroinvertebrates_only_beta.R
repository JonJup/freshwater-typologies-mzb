### -------------------------------------------- ###
### --- Clustering of Macroinvertebrate data --- ### 
### --- Only flexible beta                   --- ### 
### -------------------------------------------- ###

# --------------- #
# date:  
#       04.05.21
# files in:  
#       sxs_genus_typology_wo_bio.rds
# files out:
#      sxs_genus_typology_w_bio_beta.rds
# Project:
#         Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Find the optimal clustering of the biological data 
# --------------- #

# setup -----------------------------------------------
setwd(here::here())
source("R/setup.R")

# load data -------------------------------------------
data = readRDS("data/06_sxs_genus.RDS")

# prepare data ----------------------------------------

## ---------------------- ## 
## -- remove rare taxa -- ## 
## ---------------------- ## 
## -- remove all taxa that occur in less than 5 samples 
rare = which(colSums(data[,-c(1,2)])<5) + 2
rare = names(rare)
keep = setdiff(names(data), rare)
data = data[, keep, with = F]
rm(keep,rare);gc()

## -- non-taxa column that need to be dropped 
ntc = c(1,2)

## -- set with only taxa 
data_ot = copy(data)
data_ot = data_ot[,(ntc):=NULL]
## -- convert to matrix 
data_ot %<>% as.matrix()
## -- compute distance matrix. Binary = Jaccard  
data_dist = lapply(c("binary"), function(arg) parallelDist(x = data_ot, method = arg, threads = 8))

# compute clusters --------------------------------------------

beta_parameters = seq(from = -1, 
                      to   =  1, 
                      length.out = 10)

## -- calling on all resulted in an error.
## -- Call iteratively
ls_flex  = lapply(beta_parameters, 
                  function(i) as.hclust(
                          agnes(
                                  x = data_dist[[1]], 
                                  method = "flexible", 
                                  par.method = i)
                          )
                  )

ls_clust[[1]][[6]] = ls_flex[[1]]
ls_clust[[2]][[6]] = ls_flex[[2]]
ls_clust[[3]][[6]] = ls_flex[[3]]

methods = append(methods, "flexible")
ls_clust2 = flatten(ls_clust)
names(ls_clust2) = paste0(rep(c(
        "bin"
        #"och",
        #"dic"
), each = 6), "-",methods )

# Copheneitc correlation plots  -------------------------------------------
ls_coph = map(.x = ls_clust2,.f = cophenetic)
dist_vec = lapply(data_dist, as.vector)
ls_plot = list()
for (i in 1:length(ls_clust2)) {
        ls_plot[[i]] = cc(i)
        # if (i == 1) lp_collection = plot_grid(lp_plot) 
        # if (i > 1) lp_collection = plot_grid(lp_collection, lp_plot, nrow = 6, ncol = 3)
}
gg_coll = do.call(grid.arrange, ls_plot)
ggsave(plot = gg_coll, filename = "figures/cluster_eval/cophenetic_distances.pdf", height = 6.85, width = 5.88, units = "in")
rm(ls_coph)
gc()

# summary statistics ------------------------------------------------------
ls_clust2.1 = ls_clust2[1:6]
ls_clust2.2 = ls_clust2[7:12]
ls_clust2.3 = ls_clust2[13:18]

ls_clust2.1_eval = lapply(5, function(x) eval_cluster_fun(cl = ls_clust2.1, cut = x))

for ( i in 14:20) {
        out = eval_cluster_fun(cl = ls_clust2.1, 
                               cut = i)
        savename = paste0("data/temp/cluster_eval_",i,".rds")
        saveRDS(object = out, 
                file = savename)
        rm(out)
        rm(savename)
        gc()
}
# ls_clust2.2_eval = lapply(2:30, function(x) eval_cluster_fun(cl = ls_clust2.2, cut = x))
# ls_clust2.3_eval = lapply(2:30, function(x) eval_cluster_fun(cl = ls_clust2.3, cut = x))
beep()

temp_dir = fs::dir_ls("data/temp")[grepl(pattern = "cluster_eval_", x = fs::dir_ls("data/temp"))][c(12:16,1:11)]
ls_clust2.1_eval = list()
for (i in 1:length(temp_dir)) {
        
        ls_clust2.1_eval[[i]] = readRDS(temp_dir[[i]])
        if (i == length(temp_dir)) rm(temp_dir, i)
}


ls_clust2.1_eval2 = flatten(ls_clust2.1_eval)
# ls_clust2.2_eval2 = flatten(ls_clust2.2_eval)
# ls_clust2.3_eval2 = flatten(ls_clust2.3_eval)
ls_clust2.1_eval3 = rbindlist(ls_clust2.1_eval2, fill = TRUE)
#ls_clust2.2_eval3 = rbindlist(ls_clust2.2_eval2, fill = TRUE)
#ls_clust2.3_eval3 = rbindlist(ls_clust2.3_eval2, fill = TRUE)
dt_eval = ls_clust2.1_eval3
#dt_eval = rbindlist(list(ls_clust2.1_eval3, ls_clust2.2_eval3, ls_clust2.3_eval3))


dt_eval = dt_eval[,c(1:20)]
#dt_eval[, cluster := c(names(ls_clust2.1_eval2), names(ls_clust2.2_eval2), names(ls_clust2.3_eval2))]
dt_eval[, cluster := c(names(ls_clust2.1_eval2))]
dt_eval[, cluster_algorithm := sub(pattern = "^.*-", x = cluster, replacement = "")]
dt_eval[, distance_metric := sub(pattern = "-.*$", x = cluster, replacement = "")]
dt_eval[, distance_metric := ifelse(distance_metric == "bin", "Jaccard", ifelse(distance_metric == "dic", "Dice", "Ochiai"))]
dt_eval[, cluster := NULL]
library(tidyr)
dt_eval2 = pivot_longer(data=dt_eval,cols=!c("cluster_algorithm","cluster.number","distance_metric"))
setDT(dt_eval2)
dt_eval2[name == "cluster_size_range", value := value*unique(dt_eval2[name == "n", value])]


# quicksave ----------------------------------------------------------------
saveRDS(dt_eval2, "data/10_bio_cluster_evaluation.rds")
# quickload ---------------------------------------------------------------
dt_eval2 = readRDS("data/10_bio_cluster_evaluation.rds")


# plot ------------------------------------------------------------------------------

dt_eval2 %<>% mutate()
dt_eval2 %>% 
        filter(cluster_algorithm == "flexible") %>% 
        ggplot(aes(x = cluster.number, 
                   y = value,
                   col = cluster_algorithm )) + 
        geom_line() + 
        facet_wrap(.~name, scales = "free") 

data13 = filter(dt_eval2, 
                cluster.number == 9 & 
                        cluster_algorithm == "flexible" & 
                        name %in% c("wb.ratio",
                                    "avg.silwidth",
                                    "entropy",
                                    "ch",
                                    "sindex") #& 
                #  distance_metric == "Jaccard"
                
) 

dt_eval2 %>% 
        ##- select cluster algorithm and validity metrics 
        filter(
                cluster_algorithm == "flexible" &
                        name %in% c("wb.ratio",
                                    "avg.silwidth",
                                    "entropy",
                                    "ch",
                                    "sindex")
        ) %>% 
        #filter(distance_metric == "Jaccard") %>% 
        filter(cluster.number > 3) %>% 
        ##- plotting 
        ggplot(aes(x = cluster.number, 
                   y = value,
                   col = distance_metric)) + 
        geom_line() + 
        facet_wrap(.~name, scales = "free") + 
        geom_point(data = data13, aes(x=cluster.number, y = value), col = "black") + 
        xlab("Number of clusters") + 
        theme(axis.title.y = element_blank()) + 
        labs(col = "Distance Metric") + 
        xlim(4,30)


##- save to plot 
# landscape
#ggsave(filename="figures/size_difference.pdf", width =6.85, height = 5.88, unit="in" )
# average.between = average distance between clusters.

## -> I decided on dice with 10 groups flexible beta 
silopt_base =
        agnes(x = data_dist[[1]],
              par.method = 0.625,
              method = "flexible") %>%
        as.hclust() %>%
        cutree(k = 9)


# combine bio cluster with non-bio clusterings  ------------------------------------------
data$bio = silopt_base

# save to file  -----------------------------------------------------------
saveRDS(data,"data/11_sxs_genus_W_bio_typology.rds")
