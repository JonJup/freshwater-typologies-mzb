# ---------------------------------------- #
### --- Derive typical assemblages   --- ### 
# ---------------------------------------- #

# --------------- #
# files in 
#               -> 07_sxs_list_all_typologies.rds
# files out:
#               <- 08_indicator_list.rds | list of indicators 
# calls scripts: 
#               -> 07_b_compute_indvals.R
#               -> 07_c_setup_ta_analysis.R
#               -> 07_d_redundancy.R
#               -> 07_f_make_ta_lists.R
#               -> 07_g_ta_table.R
# Purpose: 
#               Derive typical assemblages  
# --------------- #


# setup -------------------------------------------------------------------
library(corrplot)
library(data.table)
library(dplyr)
library(magrittr)



# load data  --------------------------------------------------------------
ls_mzb <-  readRDS("data/temporal_aggregation/07_sxs_list_all_typologies.rds")



# settings ----------------------------------------------------------------
## -- thresholds to derive typical assemblages
thresholds = list(
        spe = list(a = 2, b = .20, b2 = 0.05),
        gen = list(a = 2, b = .33, b2 = 0.05),
        fol = list(a = 2, b = .66, b2 = 0.05)
)

## -- brt12 -  
choose_typology = c("brt12")
#combine_types = list(c(2,3,4), c(6,7),c(8,9))
## -- brt20 -
#choose_typology = c("brt20")
# combine_types = list(c(2,4,5), c(8,9,10,11,16))
## -- GloRiC -
#choose_typology <- "gloric"
# combine_types   <- list(c(4,15), c(6,8), c(5,22))
## -- Illies - 
#choose_typology <- "illies"
# combine_types   <- list()
# ## -- BRG 
# choose_typology <- "bgr"
combine_types   <- list()


# compute indvals ---------------------------------------------------------
source("R/temporal_aggregation/08b_compute_indvals.R")

# setup ta analysis -------------------------------------------------------
source("R/temporal_aggregation/08c_setup_ta_analysis.R")

# redundancy analysis -----------------------------------------------------
opt.plot <- FALSE 
source("R/temporal_aggregation/08d_redundancy.R")

# lists of typical taxa -------------------------------------------------------------
source(file.path("R/helper/07_g_ta_table.R"))
saveRDS(dt_mzb_list,           paste0("data/temporal_aggregation/ta_",choose_typology,"_no-redundant.rds"))
xlsx::write.xlsx2(excel_table, paste0("data/temporal_aggregation/ta_",choose_typology,"_no-redundant.xlsx"))
