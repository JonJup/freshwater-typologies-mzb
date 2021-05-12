# ---------------------------------------- #
### --- Derive typical assemblages   --- ### 
# ---------------------------------------- #

# --------------- #
# date:  17.03.21
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
source("R/setup_combined_inv.R")

# settings ----------------------------------------------------------------
# thresholds to derive typical assemblages; # the following line is referred to in "08_seasonal_typical_assemblages.R" as line 27:29 
x_ls_thesholds = list(spe = list(a = 2, b = .20, b2 = 0.05),
                      gen = list(a = 2, b = .33, b2 = 0.05), 
                      fol = list(a = 2, b = .66, b2 = 0.05))

# the following line is referred to in "08_seasonal_typical_assemblages.R" as line 32 
#x_ls_combine = list(c(2,3,8,9, 10, 11), c(4,5), c(15,16))
x_ls_combine = list(c(2,3,8,9,10,11), c(4,5), c(15,16))
#x_ls_combine = list(c(4,5), c(8,10,11,18), c(14,15,16))

# compute indvals ---------------------------------------------------------
source(file.path(dir$hlp, "07_b_compute_indvals.R"))

# setup ta analysis -------------------------------------------------------
source(file.path(dir$hlp, "07_c_setup_ta_analysis.R"))

# redundancy analysis -----------------------------------------------------
source(file.path(dir$hlp, "07_d_redundancy.R"))

## -- save plot to file 
setEPS()                                             # Set postscript arguments
postscript("figures/ta_redundancies/round6.eps")                           # Start graphics device driver
corrplot::corrplot(ma_redundnat, 
                   method = "number", 
                   is.corr = FALSE, 
                   #order = "FPC", 
                   #diag = F, 
                   #type = "lower", 
                   tl.cex = .7,
                   number.cex = 0.6)                                  # Create plot
dev.off()   
beepr::beep()
stop()
# # sensitivity analysis -----------------------------------------------------
#source(file.path(dir$rs, "07_e_sensitivity_analysis.R"))

# # make lists -----------------------------------------------------
#source(file.path(dir$hlp, "07_f_make_ta_lists.R"))

#make table for paper----------------------------------------------------------------------------------------------------------------------------------------------
source(file.path(dir$hlp, "07_g_ta_table.R"))


