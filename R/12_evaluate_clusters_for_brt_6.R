### --------------------------------------------- ###
### --- Evaluate bio-clusters from typologies --- ### 
### --------------------------------------------- ###

# --------------- #
# date:   
#               22.03.21
# files in:  
#               -> 11_sxs_genus_W_bio_typology.rds
#               -> 12_sxs_genus_typology_wo_bio.rds
# files out:  
#               <- 13_class_eval_mzb.rds
# Project: 
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose: 
#               Compute cluster metrics for the different typologies.   
# --------------- #

# setup ---------------------------------------------------------------------------------------------------------------------------------------------------
source("R/setup.R")
## -- functions 
# - for generalized silhouette width
source("~/03_R/functions/genmean.R")
source("~/03_R/functions/mdist.R")
source("~/03_R/functions/silgen.R")
source("~/03_R/functions/call_gensil.R")

# load data  ----------------------------------------------------------------------------------------------------------------------------------------------
data = readRDS("data/12_sxs_genus_typology_wo_bio.rds")
data2 = readRDS("data/11_sxs_genus_W_bio_typology.rds")

# join typology with bioclusters  ---------------------------------------------------
data2 %<>% dplyr::select(gr_sample_id, bio)
data = left_join(x = data, 
                 y = data2, 
                 by = "gr_sample_id"
)
rm(data2);gc()

## -- remove observations from RT17 
data = data[ls20 != "RT17"]

# prepare data  ----------------------------------------------------------------
## -- sites x species table as matrix and without typology columns
setDT(data)
ma_data = 
        copy(data) %>% 
        .[, c("gr_sample_id", "ls20", "ls12", "illies", "eea", "gloric", "bio") := NULL] %>% 
        as.matrix

## -- create brt6
ls_class_pre = case_when(
        data$ls20 == "RT1"  ~ "RT1",
        data$ls20 == "RT14" ~ "RT14",
        data$ls20 == "RT18" ~ "RT18",
        data$ls20 %in% c("RT4" , "RT5")  ~ "RT4_5",
        data$ls20 %in% c("RT14", "RT15") ~ "RT14_15",
        data$ls20 %in% paste0("RT", c(2,3,8,9,10,11)) ~ "RT2_3_8_9_10_11")

ls_class = list(
        ls6 = as.numeric(factor(ls_class_pre))
)

eval = lapply(
        1:length(ls_class),
        function (x) cluster.stats(
                d = dt_distance,
                clustering = ls_class[[x]],
                silhouette = FALSE,
                wgap = FALSE,
                sepindex = FALSE,
                sepprob = FALSE,
                sepwithnoise = FALSE
        )
) %>% 
        flatten(); beep()



