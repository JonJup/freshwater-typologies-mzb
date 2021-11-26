# ———————————————————————————————————————————————————— #
# ——— Typology evaluations of indivudal data sets  ——— # 
# ———————————————————————————————————————————————————— #

# ———————————————————————————————————
# date: 
#       28.07.21
# files in: 
#       ->   
# files out:
#       <- 
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       If I combine all samples the data set often accounts for much of the variation. 
#       Here I will evaluate the clusters in the individual data sets to understand this. 
# ———————————————————————————————————

#TODO in its current state my SIMPER analysis doesn't really make sense ... drop or think of another way 


# SETUP -----------------------------------------------------------------------------
pacman::p_load(
        cluster,
        cowplot,
        data.table,
        dplyr,
        fpc,
        ggplot2,
        ggExtra,
        ggsignif,
        indicspecies,
        magrittr,
        mapview,
        parallelDist,
        plotly,
        purrr,
        sf,
        stringr,
        tidyr,
        vegan,
        tmap
)

source("R/combined data/xx_individual_helper.R")
data <- readRDS("data/combined_data/03_2021-08-05_core_taxa_data.rds")
all_seasons <- c("spring", "summer", "autumn", "winter")
data <- lapply(data, function(x) x[brt_distance <= 500])


# COMMENTS --------------------------------------------------------------------------

# ——— What is typologies_all? 
# ——> At some steps I use all this vector to remove all
#     typology columns (i.e. characters vectors) from the data.table. Since I have included a
#     step that removes typologies with only one type form the typology vector, the typology
#     vector can no longer be used for this purpose.


# GERMAN MONITORING DATA ----------------------------------------------------------------------------
typologies <- typologies_all  <- c("brt12", "brt12_illies", "illies", "german_type", "bgr", "brt12_bgr")
data.set   <- "German monitoring data"
out1       <- call_all1(subset.name = "German federal monitoring data", german.type = T)
out.indval <- call_indval(call1.obj = out1, plot.results = TRUE)
out.cs     <- call_calssification.strength(call1.obj = out1, plot.results = TRUE)
out.anosim <- call_anosim(call1.obj = out1, plot.results = TRUE)
out.sil    <- call_silhouette(call1.obj = out1, plot.results = TRUE)
out.all    <- list(data = out1,
                   indval = out.indval, 
                   cs = out.cs, 
                   #anosim = out.anosim, 
                   silhouette = out.sil
                   )
saveRDS(out.all, paste0("data/combined_data/auxilliary/",Sys.Date(),"_",str_replace_all(data.set,"\\ ","_"),"_evaluation.rds"))

# GERMAN MONITORING DATA FROM HESSE ----------------------------------------------------------------------------
typologies <- typologies_all  <- c("brt12", "brt12_illies", "illies", "german_type", "bgr", "brt12_bgr")
data.set   <- "German federal monitoring data Hesse"
out1       <- call_all1(subset.name = "German federal monitoring data from Hesse", german.type = T)
out.indval <- call_indval(call1.obj = out1, plot.results = FALSE)
out.cs     <- call_calssification.strength(call1.obj = out1)
out.anosim <- call_anosim(call1.obj = out1)
out.sil    <- call_silhouette(call1.obj = out1, plot.results = TRUE)
out.all    <- list(data = out1, indval = out.indval, cs = out.cs, anosim = out.anosim, silhouette = out.sil)
saveRDS(out.all, paste0("data/combined_data/auxilliary/",Sys.Date(),"_",str_replace_all(data.set,"\\ ","_"),"_evaluation.rds"))

# GERMAN MONITROING DATA FROM SAXONY ANHALT -----------------------------------------
typologies <- typologies_all  <- c("brt12", "brt12_illies", "illies", "german_type", "bgr", "brt12_bgr")
data.set   <- "German Monitoring Saxony Anhalt"
out1       <- call_all1(subset.name = "German federal monitoring data from Saxony Anhalt", german.type = T)
out.indval <- call_indval(call1.obj = out1, plot.results = FALSE)
out.cs     <- call_calssification.strength(call1.obj = out1)
out.anosim <- call_anosim(call1.obj = out1)
out.sil    <- call_silhouette(call1.obj = out1, plot.results = TRUE)
out.all    <- list(data = out1, indval = out.indval, cs = out.cs, anosim = out.anosim, silhouette = out.sil)
saveRDS(out.all, paste0("data/combined_data/auxilliary/",Sys.Date(),"_",str_replace_all(data.set,"\\ ","_"),"_evaluation.rds"))


# ALL OF GERMANY --------------------------------------------------------------------
# typologies <- c("brt12", "brt12_illies", "illies", "german_type", "bgr", "brt12_bgr")
# data.set = "German Monitoring Saxony Anhalt"
# call_all(subset.name = c("German federal monitoring data from Saxony Anhalt", 
#                          "German federal monitoring data from Hesse", 
#                          "German federal monitoring data"), german.type = T)

# NAIADES ---------------------------------------------------------------------------
typologies <- c("brt12", "brt12_illies", "illies", "her", "bgr", "brt12_bgr")
data.set = "Naiades"
out1       <- call_all1(subset.name = "Naiades", german.type = T)
out.indval <- call_indval(call1.obj = out1, plot.results = FALSE)
out.cs     <- call_calssification.strength(call1.obj = out1)
out.anosim <- call_anosim(call1.obj = out1)
out.sil    <- call_silhouette(call1.obj = out1, plot.results = TRUE)
out.all    <- list(data = out1, indval = out.indval, cs = out.cs, anosim = out.anosim, silhouette = out.sil)
saveRDS(out.all, paste0("data/combined_data/auxilliary/",Sys.Date(),"_",str_replace_all(data.set,"\\ ","_"),"_evaluation.rds"))

# RCS -------------------------------------------------------------------------------
typologies <- c("brt12", "brt12_illies", "illies", "her", "bgr", "brt12_bgr")
data.set = "RCS"
out1       <- call_all1(subset.name = "Monitoring data from the RCS national network", german.type = T)
out.indval <- call_indval(call1.obj = out1, plot.results = FALSE)
out.cs     <- call_calssification.strength(call1.obj = out1)
out.anosim <- call_anosim(call1.obj = out1)
out.sil    <- call_silhouette(call1.obj = out1, plot.results = TRUE)
out.all    <- list(data = out1, indval = out.indval, cs = out.cs, anosim = out.anosim, silhouette = out.sil)
saveRDS(out.all, paste0("data/combined_data/auxilliary/",Sys.Date(),"_",str_replace_all(data.set,"\\ ","_"),"_evaluation.rds"))


# ALL OF FRANCE ----------------------------------------------------------------------
typologies <- c("brt12", "brt12_illies", "illies", "her", "bgr", "brt12_bgr")
data.set = "all_of_france"
call_all(subset.name = c("Monitoring data from the RCS national network",
                         "Naiades"), her = T)

# FRANCE AND GERMANY ----------------------------------------------------------------
# ALL OF FRANCE ----------------------------------------------------------------------
typologies <- c("brt12", "brt12_illies", "illies", "bgr", "brt12_bgr")
data.set = "France_and_Germany"
call_all(subset.name = c("Monitoring data from the RCS national network",
                         "Naiades",
                         "German federal monitoring data from Saxony Anhalt", 
                         "German federal monitoring data from Hesse", 
                         "German federal monitoring data"))

# ALL DATA --------------------------------------------------------------------------
typologies <- c("brt12", "brt12_illies", "illies", "bgr", "brt12_bgr")
data.set = "all"
call_all(subset.name = "all")







# OLD OR MISC  ----------------------------------------------------------------------
#  ————— EXAMPLE NMDS ————— #  
# #- prep4: compute NMDS - not crucial - takes very long - consider skipping
# #- skipped on run 05.08.21 
# spe5 <- spe4 |>  prep4() 
# gen5 <- gen4 |>  prep4() 
# fam5 <- fam4 |>  prep4() 
# beepr::beep()
# # ————> plot step1 
# spe6 <- spe5 |> prep5(spe2, mode = "text")
# gen6 <- gen5 |> prep5(gen2)
# fam6 <- fam5 |> prep5(fam2)
# # ————> plot step2 
# spe6 <- spe5 |> prep6(spe2)
# gen6 <- gen5 |> prep6(gen2)
# fam6 <- fam5 |> prep6(fam2)
# # ————> Evaluate clusters 
# spe7 <- spe4 |> prep7(spe2)
# gen7 <- gen4 |> prep7(gen2)
# fam7 <- fam4 |> prep7(fam2)


