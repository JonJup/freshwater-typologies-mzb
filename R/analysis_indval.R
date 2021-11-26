# ———————————————————————————— #
# ——— Analysis: IndVal ——————— # 
# ———————————————————————————— #

# ———————————————————————————————————
#  date created: 28-09-21
# last modified: 19-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute IndVal Statistic
# ———————————————————————————————————


# setup -----------------------------------------------------------------------------
devtools::install_github("https://github.com/JonJup/jjmisc")
library(data.table)
#library(labdsv)
library(indicspecies)
library(jjmisc)
library(dplyr)
library(stringr)
# load data -------------------------------------------------------------------------
id   <- readRDS("data/02_combined_data/2021-10-14_distance_ids.rds")


sp.i <- readRDS("data/02_combined_data/2021-10-14_community_data_spring_impacted.rds")
su.i <- readRDS("data/02_combined_data/2021-10-14_community_data_summer_impacted.rds")
au.i <- readRDS("data/02_combined_data/2021-10-14_community_data_autumn_impacted.rds")
sp.l <- readRDS("data/02_combined_data/2021-10-14_community_data_spring_least_impacted.rds")
su.l <- readRDS("data/02_combined_data/2021-10-14_community_data_summer_least_impacted.rds")
au.l <- readRDS("data/02_combined_data/2021-10-14_community_data_autumn_least_impacted.rds")


# analysis --------------------------------------------------------------------------

## files for loop 
communities <- c("sp.i", "su.i", "au.i", "sp.l", "su.l", "au.l")

safe_ind <- purrr::safely(indvalstat)

for (i in seq_along(communities)){
        i.com <- communities[i]
        i.dat <- get(i.com)
        ## season of community
        i.season <- case_when(
                str_detect(i.com, "sp") ~ "spring",
                str_detect(i.com, "su") ~ "summer",
                str_detect(i.com, "au") ~ "autumn"
                )
        ## impairment of community 
        i.impair <- case_when(
                str_detect(i.com, "\\.i$") ~ "FALSE",
                str_detect(i.com, "\\.l$") ~ "TRUE"
        )
        ## loop over typologies 
        for (j in c("brt", "bgr", "ife")){
                
                if (i == 1 & j == "brt") 
                        next()
                
                print("# ———— Computation 1  ———— #")
                ## analysis 
                j.res <- safe_ind(
                        com = i.dat[, -1], 
                        grouping = id[[i.com]][[j]],
                        perm = 99, 
                        season = i.season,
                        typology = j,
                        least.impaired = i.impair
                                  ) 
                ## if necessary: repeat until succesfull 
                while(!is.null(j.res$error)){
                        print("# ———— Computation 2  ———— #")
                        j.res <- safe_ind(
                                com = i.dat[, -1], 
                                grouping = id[[i.com]][[j]],
                                perm = 99, 
                                season = i.season,
                                typology = j,
                                least.impaired = i.impair
                        ) 
                }
                
                ## drop list structure that was created by the safe execution
                j.res <-j.res$result
                ## create name to save object to file 
                j.save_name <- 
                        paste0(i.com,".",j) |> 
                        str_replace_all("\\.", "_") |> 
                        paste0(".rds")
                ## save to file 
                saveRDS(j.res, 
                        paste0(
                                "data/02_combined_data/indval_single_results/", 
                                Sys.Date(), 
                                "_",
                                j.save_name
                                )
                        )
                ## print feedback 
                print(paste(
                        "# ———— ",
                        format(Sys.time(), "%H:%M"),
                        " ————",
                        j.save_name,
                        "saved to file. ———— #"
                ))
                ## clean j loop 
                rm(list = ls()[grepl("^j\\.", x = ls())])
                rm(j)
                gc()
        } # END OF J LOOP 
        ## clean i loop 
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        rm(i)
        gc()
} # END OF I LOOP

## load data 
indval_files <- fs::dir_ls(path = "data/02_combined_data/indval_single_results/")
for (i in indval_files){
        x <- readRDS(i)
        object_name <- 
                str_extract(i,"19_.*") |> 
                        str_remove("19_") |> 
                        str_remove("\\.rds")
        assign(x = object_name, 
               value = x)
}


all_data <- rbindlist(list(
               sp_i_brt$data,
               su_i_brt$data,
               au_i_brt$data,
               sp_l_brt$data,
               su_l_brt$data,
               au_l_brt$data,
               sp_i_bgr$data,
               su_i_bgr$data,
               au_i_bgr$data,
               sp_l_bgr$data,
               su_l_bgr$data,
               au_l_bgr$data,
               sp_i_ife$data,
               su_i_ife$data,
               au_i_ife$data,
               sp_l_ife$data,
               su_l_ife$data,
               au_l_ife$data
               )
               )
all_pvalue <- rbindlist(list(
        sp_i_brt$p.values,
        su_i_brt$p.values,
        au_i_brt$p.values,
        sp_l_brt$p.values,
        su_l_brt$p.values,
        au_l_brt$p.values,
        sp_i_bgr$p.values,
        su_i_bgr$p.values,
        au_i_bgr$p.values,
        sp_l_bgr$p.values,
        su_l_bgr$p.values,
        au_l_bgr$p.values,
        sp_i_ife$p.values,
        su_i_ife$p.values,
        au_i_ife$p.values,
        sp_l_ife$p.values,
        su_l_ife$p.values,
        au_l_ife$p.values
)
)


# save to file ----------------------------------------------------------------------
saveRDS(all_data, 
        "data/02_combined_data/2021_11_19_results_indval_data.rds")
saveRDS(all_pvalue, 
        "data/02_combined_data/2021_11_19_results_indval_pvalue.rds")

        
