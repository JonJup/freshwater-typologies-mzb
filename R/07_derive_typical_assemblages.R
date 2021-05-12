# ---------------------------------------- #
### --- Derive typical assemblages   --- ### 
# ---------------------------------------- #

# --------------- #
# date:  11.05..21
# files out:
#               <- 07_indicator_list.rds | list of indicators 
# calls scripts: 
#               -> 07_b_compute_indvals.R
#               -> 07_c_setup_ta_analysis.R
#               -> 07_d_redundancy.R
#               -> 07_f_make_ta_lists.R
#               -> 07_g_ta_table.R
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates
# Purpose: 
#               Derive typical assemblages  
# --------------- #


# setup -------------------------------------------------------------------
#source("R/setup_combined_inv.R")
library(data.table)
library(dplyr)
library(magrittr)
# load data  --------------------------------------------------------------
ls_mzb = readRDS("data/06_sxs_list_all_typologies.rds")

# settings ----------------------------------------------------------------
## -- thresholds to derive typical assemblages
thresholds = list(
        spe = list(a = 2, b = .20, b2 = 0.05),
        gen = list(a = 2, b = .33, b2 = 0.05),
        fol = list(a = 2, b = .66, b2 = 0.05)
)

## -- typology 
choose_typology = c("brt12")
#combine_types = list()
## -- brt12 
combine_types = list(c(2,3,4,5,6,7), c(8,9))
## -- brt20 
#combine_types = list(c(8,9,16), c(2,3,4,5,10,11))
## -- GloRiC
#combine_types = list(c(4,6))
## -- Illies 
# combine_types = list(c("Western plains", "Western highlands"), 
#                      c("England", "Ireland and Northern Ireland")
# )
# ## -- BRG 
#combine_types = list()

# compute indvals ---------------------------------------------------------
source("R/helper/07_b_compute_indvals.R")

# setup ta analysis -------------------------------------------------------
source("R/helper/07_c_setup_ta_analysis.R")

# redundancy analysis -----------------------------------------------------
source("R/helper/07_d_redundancy.R")


# save similarities -----------------------------------------------------------------
#saveRDS(ma_redundnat, "data/20_bgr_redundancy.rds")

# lists of typical taxa -------------------------------------------------------------
source(file.path("R/helper/07_g_ta_table.R"))
saveRDS(dt_mzb_list, "data/21_brt12_ta_non-redundant.rds")
xlsx::write.xlsx2(excel_table, "data/22_brt12_ta_non-redundant.xlsx")



# # sensitivity analysis -----------------------------------------------------
#source(file.path(dir$rs, "07_e_sensitivity_analysis.R"))

# # make lists -----------------------------------------------------
#source(file.path(dir$hlp, "07_f_make_ta_lists.R"))

#make table for paper----------------------------------------------------------------------------------------------------------------------------------------------
source(file.path(dir$hlp, "07_g_ta_table.R"))


## -- save plot to file 
# setEPS()                                             # Set postscript arguments
# postscript("fig/ta_redundancies/glroic_round6.eps")                           # Start graphics device driver
# corrplot::corrplot(ma_redundnat, 
#                    method = "number", 
#                    is.corr = FALSE, 
#                    #order = "FPC", 
#                    #diag = F, 
#                    #type = "lower", 
#                    tl.cex = .7,
#                    number.cex = 0.6)                                  # Create plot
# dev.off()   