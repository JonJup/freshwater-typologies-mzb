### --- Evaluate bio-clusters from typologies --- ### 

# --------------- #
# date written:  22.01.21
# date run: 26.01.21
# files in: diatoms_sxs_all_clusters.RDS; 
# files out: class_eval_mzb.RDS
# GetReal - Diatoms & Macroinvertebrates  
# Compute cluster quality criteria for the different typologies.   
# --------------- #

# setup ---------------------------------------------------------------------------------------------------------------------------------------------------
pacman::p_load(
        cluster,
        clusterCrit,
        coseq,
        data.table,
        dplyr,
        ggplot2,
        here,
        indicspecies, 
        magrittr,
        optpart,
        parallelDist,
        spaa
)

setwd(here())


# load data  ----------------------------------------------------------------------------------------------------------------------------------------------
inverts = readRDS("03_pd/mzb_sxs_all_w_bio.RDS")


# reshape  ----------------------------------------------------------------
ma_inverts = copy(inverts)
ma_inverts[, c("gr_sample_id", "ls_bd_20", "ls12", "ls20", "illies", "eea", "gloric", "tc7", "tc13") := NULL] 
ma_inverts %<>% as.matrix
# drop rare taxa
rare_taxa = colSums(x = ma_inverts)
id = which(rare_taxa > 5)
ma_inverts = ma_inverts[, id]
rm(id, rare_taxa)

ls_classifications = list(
        ls12   = factor(inverts$ls12),
        ls20   = factor(inverts$ls20),
        gloric = factor(inverts$gloric),
        illies = factor(inverts$illies),
        eea    = factor(inverts$eea),
        tc7    = factor(inverts$tc7),
        tc13   = factor(inverts$tc13)
)

lapply(ls_classifications, nlevels)

# binary = Jaccard distance matrix 
# evaluate sensitivity towards other distance metrics 
ls_distance = list(binary   = NA,
                   ochiai   = NA,
                   dice     = NA)
ls_distance$binary = parallelDist(ma_inverts, method = "binary")
ls_distance$ochiai = parallelDist(ma_inverts, method = "ochiai")
ls_distance$dice   = parallelDist(ma_inverts, method = "dice")

# compute cluster metrics ---------------------------------------------------------------------------------------------------------------------------------
ch_distance = c("binary", "ochiai", "dice")
ch_class = c("ls12", "ls20", "gloric", "illies", "eea", "tc7", "tc13")
ch_metrics = c(
        "partana_ratio",
        "mean_silhouette_width",
        "indval",
        "optimclass_coef",
        "optimclass_faithful_1",
        "optimclass_faithful_2"
)
setDT(expand.grid(
        "invertebrates",
        ch_class,
        ch_metrics,
        ch_distance
)) -> dt_results
dt_results$value = 0
names(dt_results) = c("taxon", "classification", "metric", "distance")

# list to store isamic results 
ls_isamic = list()
M = ncol(ma_inverts)
# LOOP ----------------------------------------------------------------------------------------
for (i.class in ch_class) {
        # CLASSIFICATIONS
        # select classification 
        loop_class = ls_classifications[[i.class]]
        # compute isamic 
        ls_isamic[[length(ls_isamic) + 1]] = isamic(comm = ma_inverts, clustering = loop_class)
        names(ls_isamic)[length(ls_isamic)] = i.class
        print(paste(
                "Start computing Invdval",
                i.class,
                "@",
                Sys.time()
        ))
        # approx 12 minutes for invertebrates and ls12
        indval = multipatt(
                ma_inverts, cluster = loop_class,
                control = how(nperm = 10000),
                max.order = 1, print.perm = T
        )
        # Now there are many p-values and associated NHSTs. To control for multiple testing
        # I will use  Holm's step-down multiple testing procedure.
        # How many tests?
        # extract p-values and save as data.table
        indval_matrix = setDT(indval$sign)
        # remove NA p-values and add taxon names
        indval_matrix[, c("p.value", "taxon") := .(nafill(p.value, fill = 1),
                                                   rownames(indval$B))]
        # subset table
        indval_matrix = indval_matrix[, c("p.value", "index", "stat", "taxon")]
        # order matrix
        setorderv(indval_matrix, "p.value")
        # number of NHSTs
        n_test = nrow(indval_matrix)
        # Set the desired significance level
        alpha = 0.05
        indval_matrix[, holms_p := alpha / n_test:1]
        indval_matrix[, significant := holms_p > p.value]
        first_no = which(indval_matrix$significant == FALSE)[1]
        significant2   = rep(TRUE, times = first_no - 1)
        significant2_2 = rep(FALSE, times = nrow(indval_matrix) - length(significant2))
        significant2 %<>% append(significant2_2)
        indval_matrix[, significant := significant2]
        indval_statistic = indval_matrix[significant == TRUE, sum(stat)]
        indval_statistic = indval_statistic / M
        
        rm(indval_matrix,
           significant2,
           significant2_2,
           first_no,
           alpha,
           n_test)
        
        dt_results[classification == i.class & metric ==  "indval",
                   value := indval_statistic]
        dt_results[classification == i.class & metric ==  "optimclass_coef",
                   value := out_optim_class]
        dt_results[classification == i.class & metric ==  "optimclass_faithful_1",
                   value := out_optim_prop_005]
        dt_results[classification == i.class & metric ==  "optimclass_faithful_2",
                   value := out_optim_prop_0m6]
        
        for (i.dist in 1:3) {
                # begin loop over distance metrics
                # compute metrics
                ch_distance = switch(i.dist, "binary", "ochiai", "dice")
                loop_dist   = ls_distance[[ch_distance]]
                loop_partana_ratio = partana(c = loop_class, dist = loop_dist)$ratio
                loop_asw = mean(silhouette(dist = loop_dist, x = as.numeric(loop_class)))
                
                dt_results[classification == i.class & metric ==  "partana_ratio" & distance == ch_distance,
                           value := loop_partana_ratio]
                dt_results[classification == i.class & metric ==  "mean_silhouette_width" & distance == ch_distance,
                           value := loop_asw]
                rm(ch_distance,
                   loop_partana_ratio,
                   loop_asw)
        }# end loop over distance metrics
        rm(
                loop_class,
                indval_statistic,
                optimclass_model,
                out_optim_class
        )
        gc()
}# end loop over classifications

# call scripts for silhouette width 
source("02_rs/cluster_eval_mub_silhouette width.R")

ls_sil   %<>% saveRDS("03_pd/class_eval_silhouette_mzb.RDS")
ls_isamic %<>% saveRDS("03_pd/class_eval_isamic_mzb.RDS")
saveRDS(dt_results, "03_pd/class_eval_mzb2.RDS")
# INTERPRETATION GUIDE

# indval: higher = better
# inertia: smaller = better
# ISAMIC : Mean consistency of species
# silhouette width _ higher = better
# optimclass- regression parameter
# partana ratio: within-cluster similarity to between-cluster similarity
dt_results %>%
        filter(metric == "partana_ratio") %>% 
        ggplot(aes(y = value, x = classification)) +
        geom_point(aes(col = distance, shape = distance), size = 2) + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) + 
        ggtitle("Partana ratio") + 
        ylab("") + xlab("")

# silhouette width plot 
num_silhouette = unlist(ls_sil)
ch_class_rep = factor(rep(ch_class, each = 1942))
silhouette_data = data.table(
        classification = ch_class_rep,
        silhouette = num_silhouette)
silhouette_data %>% 
        ggplot(aes(y = classification, 
                   x = silhouette)) + 
        geom_violin(aes(fill = classification), draw_quantiles = c(.25, 0.5, 0.75), alpha = 0.5) + 
        #geom_jitter(aes(fill = classification), shape = 21, alpha = 0.02) + 
        geom_vline(xintercept = 0, col = "black", size = 1, shape = 2) + 
        theme(legend.position = "none", 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

# ISAMIC plot 
#ls_isamic = ls_isamic[-c(1:2)]
num_isamic = unlist(ls_isamic)
ch_class_rep = factor(rep(ch_class, each = 438))
isamic_data = data.table(
        classification = ch_class_rep,
        isamic = num_isamic,
        taxon = rep(colnames(ma_inverts), times = length(ch_class))
)

min_eea = isamic_data[classification == "eea", min(isamic)]
min_ls12 = isamic_data[classification == "ls12", min(isamic)]
min_ls20 = isamic_data[classification == "ls20", min(isamic)]
min_tc7 = isamic_data[classification == "tc7", min(isamic)]
min_tc13 = isamic_data[classification == "tc13", min(isamic)]
min_illies = isamic_data[classification == "illies", min(isamic)]
min_gloric = isamic_data[classification == "gloric", min(isamic)]

isamic_data %>% 
        ggplot(aes(x = classification, 
                   y = isamic)) + 
        geom_jitter(aes(fill = classification), shape = 21) +
        annotate("label", x = 1, y = 0.18, label = isamic_data[classification == "eea"    & isamic == min_eea,    "taxon"]) + 
        annotate("label", x = 2, y = 0.30, label = isamic_data[classification == "gloric" & isamic == min_gloric, "taxon"]) + 
        annotate("label", x = 3, y = 0.30, label = isamic_data[classification == "illies" & isamic == min_illies, "taxon"]) + 
        annotate("label", x = 4, y = 0.12, label = isamic_data[classification == "ls12"   & isamic == min_ls12, "taxon"]) +
        annotate("label", x = 5, y = 0.12, label = isamic_data[classification == "ls20"   & isamic == min_ls20, "taxon"]) + 
        annotate("label", x = 6, y = 0.20, label = isamic_data[classification == "tc13"   & isamic == min_tc13, "taxon"][1]) + 
        annotate("label", x = 6, y = 0.30, label = isamic_data[classification == "tc13"   & isamic == min_tc13, "taxon"][2]) + 
        annotate("label", x = 7, y = 0.30, label = isamic_data[classification == "tc7"    & isamic == min_tc7, "taxon"]) + 
        # stat_summary(
        #         geom = "crossbar",
        #         fun = "mean",
        #         size = 1,
        #         aes(col = classification)
        # ) + 
        stat_summary(
                geom = "crossbar",
                fun = "quantile",
                fun.args = list(probs = c(0.25,0.75)),
                size = 1,
                
                aes(col = classification)
        ) + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.position = "none")

# understaniding clsuter metrics  -----------------------------------------
data_matrix = data.frame(
        spa = rep(, times = 50),
        spb = rep(1, 100),
        spc = rep(1, 100)
)
test_cluster = rep(c(1, 2), each = 50)

isamic(comm = data_matrix,
       clustering = test_cluster)

loop_list = list()
for (i in 1:100) {
        lsda = matrix(data = 0,
                      nrow = 100,
                      ncol = 600)
        for (j in 1:600) {
                lsda[, j] = rbinom(n = 100, 1, prob = runif(1, 0, 1))
        }
        clustering = sample(1:i, 100, replace = TRUE)
        loop_list[[i]] = data.table(isamic(comm = lsda, clustering = clustering))
}
test = rbindlist(loop_list)
test$n_clust = factor(rep(c(1:100), each = 600))

test %>%
        dplyr::group_by(n_clust) %>%
        ggplot(aes(x = n_clust, y = V1)) +
        geom_boxplot() 
## -- Silhouette width 
sil = silhouette(dist = ls_distance$binary, x = as.numeric(ls_classifications$ls12))
sil = sil[,3]


# old code  ---------------------------------------------------------------

# loop_cluster_crit = intCriteria(
#         traj = ma_inverts,
#         part = as.integer(loop_class),
#         crit = c("C_index", "Gamma", "Point_Biserial")
# )
# loop_inertia = sum(clusterInertia(
#         profiles = ma_inverts,
#         clusters = as.numeric(loop_class)
# ))
