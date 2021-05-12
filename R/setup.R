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
