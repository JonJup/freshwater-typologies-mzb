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


#- load function
source("R/helper/f_01_redundant.R")

#- rename type column
names(dt_mzb)[2]  <-  "group"
#- compute redundancies 
ma_redundant      <-  redundant(dt_mzb)

if (opt.plot){

        #- set non informative diagonal elements of the matrix to zero for display
        diag(ma_redundant) <- 0
        #- transform to vector for display
        ma_data <- c(ma_redundant)
        x11()
        corrplot(
                ma_redundant,
                method = "number",
                is.corr = T,
                diag = F,
                tl.cex = .7,
                number.cex = 0.6
        ) 
        rm(ma_data)
}

#- set non informative diagonal elements of the matrix to two. This makes it easier to remove later. 
diag(ma_redundant) <- 2
if (opt.save) {
        save.name <-
                paste0("data/summer season/",
                       "redundancies_",
                       choose_typology,
                       ".rds")
        saveRDS(ma_redundant, save.name)
}
rm(redundant,ma_redundant, save.name)

print("#--------------------------------------------------------#")