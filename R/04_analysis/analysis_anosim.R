# ------------------------- #
# --- Analysis: ANOSIM  --- # 
# ------------------------- #


#  date created: 27-09-21
# last modified: 04-05-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute ANOSIM


# setup -----------------------------------------------------------------------------
library(vegan)
library(data.table)
library(stringr)
library(rstudioapi)
library(dplyr)
library(fs)

x<-getActiveDocumentContext()
sink(file = paste0("R/02_combined_data/log_files/anosim","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
id     <-readRDS("data/02_combined_data/04_2022-05-04_distance_ids.rds")
d.sp.i <-readRDS("data/02_combined_data/04_2022-05-04_distance_spring.rds")
d.su.i <-readRDS("data/02_combined_data/04_2022-05-04_distance_summer.rds")
d.au.i <-readRDS("data/02_combined_data/04_2022-05-04_distance_autumn.rds")

# analysis --------------------------------------------------------------------------
## all ANOSIM are run with the default number of permutations (999)

cores = 6

##   ---BRT
an.sp.i.brt <-anosim(x = d.sp.i, grouping = id$sp.i$brt, parallel = cores)
saveRDS(an.sp.i.brt, "temp/an.sp.i.brt.rds")
an.su.i.brt <-anosim(x = d.su.i, grouping = id$su.i$brt, parallel = cores)
saveRDS(an.su.i.brt, "temp/an.su.i.brt.rds")
an.au.i.brt <-anosim(x = d.au.i, grouping = id$au.i$brt, parallel = cores)
saveRDS(an.au.i.brt, "temp/an.au.i.brt.rds")
##   ---BGR
an.sp.i.bgr <-anosim(x = d.sp.i, grouping = id$sp.i$bgr, parallel = cores)
saveRDS(an.sp.i.bgr, "temp/an.sp.i.bgr.rds")
an.su.i.bgr <-anosim(x = d.su.i, grouping = id$su.i$bgr, parallel = cores)
saveRDS(an.su.i.bgr, "temp/an.su.i.bgr.rds")
an.au.i.bgr <-anosim(x = d.au.i, grouping = id$au.i$bgr, parallel = cores)
saveRDS(an.au.i.bgr, "temp/an.au.i.bgr.rds")
##   ---IFE
an.sp.i.ife <-anosim(x = d.sp.i, grouping = id$sp.i$ife, parallel = cores)
saveRDS(an.sp.i.ife, "temp/an_sp_i_ife.rds")
an.su.i.ife <-anosim(x = d.su.i, grouping = id$su.i$ife, parallel = cores)
saveRDS(an.su.i.ife, "temp/an_su_i_ife.rds")
an.au.i.ife <-anosim(x = d.au.i, grouping = id$au.i$ife, parallel = cores)
saveRDS(an.au.i.ife, "temp/an_au_i_ife.rds")

# reshape ---------------------------------------------------------------------------
## first load the anosim reusults
anosim_files <-dir_ls("temp/")
## reshape to data.table and store in list
for (i in seq_along(anosim_files)){
        ## in first iteration, create list to store results 
        if (i == 1) out.ls <-list()
        ## anosim file for this iteration 
        i.file <- anosim_files[i]
        if (str_detect(i.file, "an\\.")){
                i.file2 <- str_replace(i.file, "an\\.", "an\\_")
        } else {
                i.file2 <- i.file
        }
        ## determine season 
        i.season <-
                case_when(
                        str_detect(i.file2, "an_sp") ~ "spring",
                        str_detect(i.file2, "an_su") ~ "summer",
                        str_detect(i.file2, "an_au") ~ "autumn"
                )
        ## determine typology 
        i.typ <-
                case_when(
                        str_detect(i.file, "brt") ~ "brt",
                        str_detect(i.file, "ife") ~ "ife",
                        str_detect(i.file, "bgr") ~ "bgr"
                )
        ## read anosim file 
        i.file <-readRDS(i.file)
        ## reshape into data.table 
        i.out <-data.table(
                season = i.season, 
                typology = i.typ,
                R = i.file$statistic, 
                p_value = i.file$signif)
        ## store in list 
        out.ls[[i]] <-i.out 
        ## print feedback 
        print(i)
        ## clean envitonment 
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}


# save to file  ---------------------------------------------------------------------
results <-rbindlist(out.ls)
saveRDS(results, paste0("data/02_combined_data/",Sys.Date(),"_results_anosim.rds"))


