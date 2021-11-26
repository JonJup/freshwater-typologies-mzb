# —————————————————————————————————————— #
### ——— Derive typical assemblages ——— ### 
### ——— Group by data.set          ——— ### 
# —————————————————————————————————————— #

# ———————————————————————————————————— 
# date 
#               09.07.21
# files in 
#               -> 
# files out:
#               <- 
# calls scripts: 
#               -> 
#               -> 
#               -> 
#               -> 
#               -> 
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates
# Purpose: 
#               Derive typical assemblages  
# ———————————————————————————————————— 


# setup -------------------------------------------------------------------
pacman::p_load(corrplot, data.table, dplyr, magrittr)

# load data  --------------------------------------------------------------
brt12 <- readRDS("data/group by data set/07_sxs_list_brt12.rds")
illie <- readRDS("data/group by data set/07_sxs_list_illies.rds")

# settings ----------------------------------------------------------------
#- thresholds to derive typical assemblages
thresholds = list(
        spe = list(a = 2, b = .20, b2 = 0.05),
        gen = list(a = 2, b = .33, b2 = 0.05),
        fol = list(a = 2, b = .66, b2 = 0.05)
)

choose_typology <- c("data.set")
combine_types   <- list()

#- the codes below assume that the object that holds the data is classed ls_mzb 
for (i in c("brt12", "illie")){
        ls_mzb <- get(i)
        save.name.var <- i   
# compute indvals ---------------------------------------------------------
source("R/group by data set/08b_compute_indvals.R")

# setup ta analysis -------------------------------------------------------
source("R/group by data set/08c_setup_ta_analysis.R")

# redundancy analysis -----------------------------------------------------
opt.plot <- FALSE 
opt.save <- TRUE
source("R/group by data set/08d_redundancy.R")

# lists of typical taxa -------------------------------------------------------------
source(file.path("R/helper/07_g_ta_table.R"))
saveRDS(dt_mzb_list,           paste0("data/group by data set/ta_",choose_typology,"_redundant.rds"))
xlsx::write.xlsx2(excel_table, paste0("data/group by data set/ta_",choose_typology,"_redundant.xlsx"))
}