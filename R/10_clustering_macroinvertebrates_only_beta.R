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
source("~/03_R/functions/cc2.R")
source("~/03_R/functions/eval_cluster_fun.R")
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

## -- these loops keep running into problems. I will start by hand. 
beta_clust_1 = agnes(x = data_dist[[1]], 
                     method = "flexible",
                     par.method = 0.1)
beta_clust_2 = agnes(x = data_dist[[1]], 
                     method = "flexible",
                     par.method = 0.2)
beta_clust_3 = agnes(x = data_dist[[1]], 
                     method = "flexible",
                     par.method = 0.3)
beta_clust_4 = agnes(x = data_dist[[1]], 
                     method = "flexible",
                     par.method = 0.4)
beta_clust_5 = agnes(x = data_dist[[1]], 
                     method = "flexible",
                     par.method = 0.5)
beta_clust_6 = agnes(x = data_dist[[1]], 
                     method = "flexible",
                     par.method = 0.6)
beta_clust_7 = agnes(x = data_dist[[1]], 
                     method = "flexible",
                     par.method = 0.7)
beta_clust_8 = agnes(x = data_dist[[1]], 
                     method = "flexible",
                     par.method = 0.8)
beta_clust_9 = agnes(x = data_dist[[1]], 
                     method = "flexible",
                     par.method = 0.9)
beta_clust_1_h = as.hclust(beta_clust_1)
beta_clust_2_h = as.hclust(beta_clust_2)
beta_clust_3_h = as.hclust(beta_clust_3)
beta_clust_4_h = as.hclust(beta_clust_4)
beta_clust_5_h = as.hclust(beta_clust_5)
beta_clust_6_h = as.hclust(beta_clust_6)
beta_clust_7_h = as.hclust(beta_clust_7)
beta_clust_8_h = as.hclust(beta_clust_8)
beta_clust_9_h = as.hclust(beta_clust_9)

ls_clust = list(beta_clust_1_h, 
                beta_clust_2_h,
                beta_clust_3_h,
                beta_clust_4_h,
                beta_clust_5_h,
                beta_clust_6_h,
                beta_clust_7_h,
                beta_clust_8_h,
                beta_clust_9_h
                )

saveRDS(ls_clust, "data/temp/beta_clusters.rds")
ls_clust = readRDS("data/temp/beta_clusters.rds")
# Copheneitc correlation plots  -------------------------------------------
ls_coph = map(.x = ls_clust,.f = cophenetic)
dist_vec = lapply(data_dist, as.vector)
ls_plot = list()
for (i in 1:length(ls_clust))  ls_plot[[i]] = cc2(i)
gg_coll = do.call(grid.arrange, ls_plot)
ggsave(plot = gg_coll, 
       filename = "fig/cluster_eval/cophenetic_distances_beta.png",
       height = 6.85, 
       width = 5.88, 
       units = "in")
ggsave(plot = gg_coll, 
       filename = "fig/cluster_eval/cophenetic_distances_beta.eps",
       height = 6.85, 
       width = 5.88, 
       units = "in")
rm(ls_coph)
gc()

# summary statistics ------------------------------------------------------
rm(i, ntc, beta_vec, dir, append_list, gen_mean, dist_vec)
gc()

ls_clust_eval <- lapply(2:3, 
                        function(x) eval_cluster_fun(cl = ls_clust, 
                                                     cut = x))
gc()

## -- loop over clusterings (i.e. beta parameters)
hold_sw <- vector(mode = "list", length = length(ls_clust))
for (i in seq_along(ls_clust)){
        lpi_clust = ls_clust[[i]]
        lpi_hold_sw <- vector(mode = "list", length = 21)
        for (j in 5:25){
                lpj_lcc = cutree(lpi_clust, k = j)
                lpj_sw = silhouette(x = lpj_lcc, dist = data_dist[[1]])
                lpj_sw = data.table(
                        cluster = lpj_sw[, 1],
                        neighbour = lpj_sw[, 2],
                        silhouette_width = lpj_sw[, 3],
                        beta = i,
                        groups = j
                )
                lpi_hold_sw[[j]] <- lpj_sw
                rm(list = ls()[grepl(x = ls(), pattern = "^lpj_")])
                print(paste(" -- j = ", j, " -- "))
                rm(j)
                gc()
        }
        hold_sw[[i]] <- rbindlist(lpi_hold_sw)
        rm(list = ls()[grepl(x = ls(), pattern = "^lpi_")])
        print(paste("i = ", i))
        rm(i)
        gc()
}
beepr::beep(
)

hold_sw2 <- rbindlist(hold_sw)

hold_sw2 %>% 
        mutate(beta = factor(beta)) %>% 
        group_by(groups, beta) %>% 
        summarize(mean = mean(silhouette_width),
                  sd   = sd(silhouette_width)) %>% 
        ggplot(aes(y=mean, x = groups, col = beta)) + 
        geom_point() + 
        geom_line()

hold_sw2 %>% 
        mutate(cluster = factor(cluster)) %>% 
        filter(beta == 5) %>%  
        ggplot(aes(y = silhouette_width, x = cluster, col = cluster)) + 
        geom_jitter() + 
        geom_hline(yintercept = 0) + 
        facet_wrap(.~groups)

## -- Entropy
my_entropy_fun = function(x, b, g){
        counts <- 
                filter(x, beta == b, groups == g) %>% 
                group_by(cluster) %>% 
                count() 
        total <- sum(counts$n)
        counts %<>% 
                mutate(rel_n = n/total) %>% 
                mutate(entropy = rel_n * log(rel_n))
        entropy = - sum(counts$entropy)
        out = data.table(beta = b, groups = g, entropy = entropy)
        out        
}

l1 = vector(mode = "list", length = 2)
for (i in 1:9){
        l2 = vector(mode = "list", length = 21)
        for(j in 5:25){
                l2[[j]] <-
                        my_entropy_fun(x = hold_sw2,
                                       b = i,
                                       g = j)
        }
        l2 = rbindlist(l2)
        l1[[i]] = l2
} 
test = rbindlist(l1)
test %<>% mutate(beta = factor(beta))
test %>% 
        ggplot(aes(x = groups, y = entropy, col = beta)) + 
        geom_point(size = .2) + 
        geom_smooth(se = FALSE)


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
