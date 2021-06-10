### -------------------------------------------- ###
### --- Clustering of Macroinvertebrate data --- ### 
### -------------------------------------------- ###

# --------------- #
# files in:  
#       <- sxs_genus_typology_wo_bio.rds
# files out:
#      -> sxs_genus_typology_w_bio_beta.rds
# Purpose:
#       find the optimal number of clusters for the biological data 
# --------------- #

# setup -----------------------------------------------
setwd(here::here())
pacman::p_load(
        cluster, 
        data.table,
        dplyr,
        fpc,
        ggplot2, 
        magrittr
)
# functions -------------------------------------------------------------------------

cc2 = function(x) {
        
        beta_vec = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
        list_name = names(ls_clust)[x]
        base1 = 1
        #base2 = grep(gsub(x=list_name, "^.*-", replacement = ""), methods)
        
        ft = tibble("distance" = dist_vec[[base1]], 
                    "cophenetic" = as.vector(ls_coph[[x]]))
        correlation = cor(ft)[2] %>% round(2)
        
        ft_sub = sample(1:nrow(ft), 3000)
        ft_sub = ft[ft_sub, ]
        
        cophcor = ggplot(ft_sub, aes(x=distance,y=cophenetic)) + 
                geom_point(alpha = 1, size = 0.1) + 
                geom_smooth(se = FALSE) + 
                geom_smooth(se = FALSE, method = "lm", col = "goldenrod2", linetype = 2) + 
                ggtitle(label = paste("beta = ", beta_vec[i], ": ", correlation)) + 
                theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(), 
                      axis.ticks = element_blank(), 
                      axis.text = element_blank(), 
                      title =element_text(size=8))
        return(cophcor)
}

internal_cluster_fun = function(y, di){
        cs = cluster.stats(d = data_dist[[1]], clustering = y)
        cs = as.data.table(cs)
        cs[, cluster_size_range := max(cs$cluster.size) - min(cs$cluster.size)]
        cs = cs[1,]
        options(warn = -1)
        cs[, c("cluster.size", "min.cluster.size", "noisen", "diameter", "average.distance", 
               "median.distance", "separation", "average.toother", "separation.matrix.V1", "separation.matrix.V2",
               "separation.matrix.V3", "separation.matrix.V4", "separation.matrix.V5", "ave.between.matrix.V1", 
               "ave.between.matrix.V2", "ave.between.matrix.V3", "ave.between.matrix.V4", "ave.between.matrix.V5",
               "clus.avg.silwidths", "ave.between.matrix", "separation.matrix", "vi", "corrected.rand", "g3", "g2") := NULL]
        options(warn = 1)
        return(cs)
}
eval_cluster_fun = function(cl, cut) {
        lcc = lapply(cl, cutree, k = cut)
        ls_clust_sum = lapply(lcc, internal_cluster_fun)
        return(ls_clust_sum)
}


# load data -------------------------------------------
data = readRDS("data/06_sxs_genus.RDS")

# prepare data ----------------------------------------

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

beta_clust_1 = agnes(x = data_dist[[1]],
                     method = "flexible",
                     par.method = 0.625)
beta_clust_1_h = as.hclust(beta_clust_1)
ls_clust = list(beta_clust_1_h)

# Copheneitc correlation plots  -------------------------------------------
ls_coph = map(.x = ls_clust, .f = cophenetic)
dist_vec = lapply(data_dist, as.vector)
ls_plot = list()
for (i in 1:length(ls_clust))
        ls_plot[[i]] = cc2(i)
gg_coll = do.call(grid.arrange, ls_plot)
ggsave(
        plot = gg_coll,
        filename = "fig/cluster_eval/cophenetic_distances_beta.png",
        height = 6.85,
        width = 5.88,
        units = "in"
)
ggsave(
        plot = gg_coll,
        filename = "fig/cluster_eval/cophenetic_distances_beta.eps",
        height = 6.85,
        width = 5.88,
        units = "in"
)
rm(ls_coph,i, ntc, beta_vec, dir, append_list, gen_mean, dist_vec)
gc()

# summary statistics ------------------------------------------------------

## -- compute quality metrics for all clusterings with between 4 and 30 clusters 
ls_clust_eval <- lapply(4:30, 
                        function(x) eval_cluster_fun(cl = ls_clust, 
                                                     cut = x))

## -- reshape results 
ls_clust2 = flatten(ls_clust_eval)
ls_clust2 = rbindlist(ls_clust2, fill = TRUE)
dt_eval = pivot_longer(data=ls_clust2,cols=!c("cluster.number"))
setDT(dt_eval)

# plot ------------------------------------------------------------------------------

data13 = filter(dt_eval, 
                cluster.number == 9 & 
                        cluster_algorithm == "flexible" & 
                        name %in% c("wb.ratio",
                                    "avg.silwidth",
                                    "entropy",
                                    "ch",
                                    "sindex")
                
) 

dt_eval %>% 
        ##- select cluster algorithm and validity metrics 
        filter(
                cluster_algorithm == "flexible" &
                        name %in% c("wb.ratio",
                                    "avg.silwidth",
                                    "entropy",
                                    "ch",
                                    "sindex")
        ) %>% 
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
