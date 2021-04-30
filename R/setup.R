pacman::p_load(
        beepr,
        cluster,
        clusterCrit,
        cowplot,
        data.table,
        dplyr,
        FD,
        fpc,
        future.apply,
        fuzzySim,
        ggplot2,
        ggrepel,
        gridExtra,
        here,
        huxtable,
        indicspecies,
        kableExtra,
        lubridate,
        magrittr,
        optpart,
        parallelDist,
        purrr,
        sf,
        stringr,
        taxize,
        tibble,
        tidyr,
        tmap, 
        treeClust,
        wesanderson, 
        png,
        grid
)

dir = list()
dir$dt = here("data/")
dir$ls     = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/"
dir$gloric   = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/03_GloRiC_v10/"
dir$ecoregions = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/04_wfd_ecoregions/"
dir$eea        = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/05_bfn_eea_bioregions/"
dir$mzb = here("../../02_getreal/02_wp2/data/invertebrates/")
dir$trt = "D://Arbeit/Data/trait data/stefan_freshwater_ecology/"

# load functions ----------------------------------------------------------
source("~/03_R/functions/cc.R")
source("~/03_R/functions/extr_traits.r")
source("~/03_R/functions/pcoa_project_variables.R")
source("~/03_R/functions/plot_pcoa.R")


append_list = function(x,y,i){
        x[[i]] = append(x[[i]], y[[i]])
}

gen_mean = function(x,p){
        fact = 1/length(x)
        out =  (fact * (sum(x^p)))^(1/p)
        out
}


my_sil = function(x, dist) {
        out = silhouette(x = x, dist = dist)
        n = nrow(out)
        out %<>% matrix()
        out =  tibble(
                cluster = out[1:n],
                neighbor = out[(n + 1):(2 * n)],
                sil_width = out[(2 * n + 1):(3 * n)]
        )
        out
}
internal_cluster_fun = function(y, di){
        cs = cluster.stats(d = data_dist[[di]], clustering = y)
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
        distance_indicator = ifelse(all(grepl(pattern = "bin", x = names(lcc))), 1, ifelse(all(grepl(
                pattern = "och", x = names(lcc)
        )), 2, 3))
        ls_clust_sum = lapply(lcc, internal_cluster_fun, di = distance_indicator)
        return(ls_clust_sum)
}