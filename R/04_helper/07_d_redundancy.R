# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------- Macroinvertebrates ------- ###
### ---------- Redundancy   ---------- ###
# -------------------------------------- #

# --------------- #
# date:         17.03.21
#
# called by:
#               07_derive_typical_assemblages.R
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#               Evaluate redundancy between typical assemblages 
# --------------- #


#function
source("R/helper/f_01_redundant.R")

names(dt_mzb)[2] = "group"

ma_redundnat = redundant(dt_mzb)
diag(ma_redundnat) = 2
ma_data <- c(ma_redundnat)

#print(ma_redundnat)
x11()
#ma_redundnat[which(ma_redundnat < 0.8)] = 0
corrplot::corrplot(ma_redundnat, 
                   method = "number", 
                   is.corr = FALSE, 
                  # order = "FPC", 
                   diag = F, 
                   #type = "lower", 
                   tl.cex = .7,
                   number.cex = 0.6) 


rm(redundant)

print("#--------------------------------------------------------#")