# ———————————————————————— #
# ——— Analysis: ANOSIM ——— # 
# ———————————————————————— #

# ———————————————————————————————————
#  date created: 27-09-21
# last modified: 15-09-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute ANOSIM
# ———————————————————————————————————


# setup -----------------------------------------------------------------------------
library(vegan)
library(data.table)
library(stringr)
# load data -------------------------------------------------------------------------
id   <- readRDS("data/02_combined_Data/2021-10-14_distance_ids.rds")
d.sp.i <- readRDS("data/02_combined_data/2021-10-14_distance_spring.rds")
d.su.i <- readRDS("data/02_combined_data/2021-10-14_distance_summer.rds")
d.au.i <- readRDS("data/02_combined_data/2021-10-14_distance_autumn.rds")
d.sp.l <- readRDS("data/02_combined_data/2021-10-14_distance_spring_li.rds")
d.su.l <- readRDS("data/02_combined_data/2021-10-14_distance_summer_li.rds")
d.au.l <- readRDS("data/02_combined_data/2021-10-14_distance_autumn_li.rds")

# analysis --------------------------------------------------------------------------
## all ANOSIM are run with the default number of permutations (999)

cores = 6

##  ——— BRT
an.sp.i.brt <- anosim(x = d.sp.i, grouping = id$sp.i$brt, parallel = cores)
saveRDS(an.sp.i.brt, "temp/an.sp.i.brt.rds")
an.su.i.brt <- anosim(x = d.su.i, grouping = id$su.i$brt, parallel = cores)
saveRDS(an.su.i.brt, "temp/an.su.i.brt.rds")
an.au.i.brt <- anosim(x = d.au.i, grouping = id$au.i$brt, parallel = cores)
saveRDS(an.au.i.brt, "temp/an.au.i.brt.rds")
an.sp.l.brt <- anosim(x = d.sp.l, grouping = id$sp.l$brt, parallel = cores)
saveRDS(an.sp.l.brt, "temp/an.sp.l.brt.rds")
an.su.l.brt <- anosim(x = d.su.l, grouping = id$su.l$brt, parallel = cores)
saveRDS(an.su.l.brt, "temp/an.su.l.brt.rds")
an.au.l.brt <- anosim(x = d.au.l, grouping = id$au.l$brt, parallel = cores)
saveRDS(an.au.l.brt, "temp/an.au.l.brt.rds")
##  ——— BGR
an.sp.i.bgr <- anosim(x = d.sp.i, grouping = id$sp.i$bgr, parallel = cores)
saveRDS(an.sp.i.bgr, "temp/an.sp.i.bgr.rds")
an.su.i.bgr <- anosim(x = d.su.i, grouping = id$su.i$bgr, parallel = cores)
saveRDS(an.su.i.bgr, "temp/an.su.i.bgr.rds")
an.au.i.bgr <- anosim(x = d.au.i, grouping = id$au.i$bgr, parallel = cores)
saveRDS(an.au.i.bgr, "temp/an.au.i.bgr.rds")
an.sp.l.bgr <- anosim(x = d.sp.l, grouping = id$sp.l$bgr, parallel = cores)
saveRDS(an.sp.l.bgr, "temp/an.sp.l.bgr.rds")
an.su.l.bgr <- anosim(x = d.su.l, grouping = id$su.l$bgr, parallel = cores)
saveRDS(an.su.l.bgr, "temp/an.su.l.bgr.rds")
an.au.l.bgr <- anosim(x = d.au.l, grouping = id$au.l$bgr, parallel = cores)
saveRDS(an.au.l.bgr, "temp/an.au.l.bgr.rds")
##  ——— IFE
an.sp.i.ife <- anosim(x = d.sp.i, grouping = id$sp.i$ife, parallel = cores)
saveRDS(an.sp.i.ife, "temp/an_sp_i_ife.rds")
an.su.i.ife <- anosim(x = d.su.i, grouping = id$su.i$ife, parallel = cores)
saveRDS(an.su.i.ife, "temp/an_su_i_ife.rds")
an.au.i.ife <- anosim(x = d.au.i, grouping = id$au.i$ife, parallel = cores)
saveRDS(an.au.i.ife, "temp/an_au_i_ife.rds")
an.sp.l.ife <- anosim(x = d.sp.l, grouping = id$sp.l$ife, parallel = cores)
saveRDS(an.sp.l.ife, "temp/an_sp_l_ife.rds")
an.su.l.ife <- anosim(x = d.su.l, grouping = id$su.l$ife, parallel = cores)
saveRDS(an.su.l.ife, "temp/an_su_l_ife.rds")
an.au.l.ife <- anosim(x = d.au.l, grouping = id$au.l$ife, parallel = cores)
saveRDS(an.au.l.ife, "temp/an_au_l_ife.rds")

# reshape ---------------------------------------------------------------------------
## first load the anosim reusults
anosim_files <- fs::dir_ls("temp/")
## reshape to data.table and store in list
for (i in seq_along(anosim_files)){
        ## in first iteration, create list to store results 
        if (i == 1) out.ls <- list()
        ## anosim file for this iteration 
        i.file <- anosim_files[i]
        ## determine season 
        i.season <- 
                dplyr::case_when(
                        str_detect(i.file, "an_sp") ~ "spring",
                        str_detect(i.file, "an_su") ~ "summer",
                        str_detect(i.file, "an_au") ~ "autumn"
                )
        ## determined level of anthropogenic impact 
        i.impaired <- 
                dplyr::case_when(
                        str_detect(i.file, "_i_") ~ "impaired",
                        str_detect(i.file, "_l_") ~ "least impaired"
                )
        ## determine typology 
        i.typ <-
                dplyr::case_when(
                        str_detect(i.file, "brt") ~ "brt",
                        str_detect(i.file, "ife") ~ "ife",
                        str_detect(i.file, "bgr") ~ "bgr"
                )
        ## read anosim file 
        i.file <- readRDS(i.file)
        ## reshape into data.table 
        i.out <- data.table(
                season = i.season, 
                typology = i.typ,
                impaired = i.impaired,
                R = i.file$statistic, 
                p_value = i.file$signif)
        ## store in list 
        out.ls[[i]] <- i.out 
        ## print feedback 
        print(i)
        ## clean envitonment 
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}


# save to file  ---------------------------------------------------------------------
results <- rbindlist(out.ls)
saveRDS(results, paste0("data/02_combined_data/",Sys.Date(),"_results_anosim.rds"))


