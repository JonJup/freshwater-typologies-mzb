# ---------------------------- #
# --- Analysis: IndVal ------- # 
# ---------------------------- #


#  date created: 28-09-21
# last modified: 04-05-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute IndVal Statistic



# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/JonJup/jjmisc")

library(data.table)
library(indicspecies)
library(jjmisc)
library(dplyr)
library(stringr)
library(rstudioapi)

x<-getActiveDocumentContext()
sink(file = paste0("R/02_combined_data/log_files/indval","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
id   <- readRDS("data/02_combined_data/04_2022-05-04_distance_ids.rds")
sp.i <- readRDS("data/02_combined_data/04_2022-05-04_community_data_spring.rds")
su.i <- readRDS("data/02_combined_data/04_2022-05-04_community_data_summer.rds")
au.i <- readRDS("data/02_combined_data/04_2022-05-04_community_data_autumn.rds")
# analysis --------------------------------------------------------------------------

## files for loop 
communities <- c("sp.i", "su.i", "au.i")

safe_ind <- purrr::safely(indvalstat)

for (i in seq_along(communities)){
        
        if (i %in% c(1,2)) next()
        
        i.com <- communities[i]
        i.dat <- get(i.com)
        
        ## season of community
        i.season <- case_when(
                str_detect(i.com, "sp") ~ "spring",
                str_detect(i.com, "su") ~ "summer",
                str_detect(i.com, "au") ~ "autumn"
                )

        ## loop over typologies 
        for (j in c("brt", "bgr", "ife")){
                print(j)
                
                print("# ---- Computation 1  ---- #")
                ## analysis 
                j.res <- safe_ind(
                        community = i.dat[, -1], 
                        grouping  = id[[i.com]][[j]],
                        perm      = 99, 
                        season    = i.season,
                        typology  = j, 
                        least.impaired = T
                                  ) 
                j.res <- out
                ## if necessary: repeat until successful 
                while(!is.null(j.res$error)){
                        print("# ---- Computation 2  ---- #")
                        
                        j.res <- safe_ind(
                                com = i.dat[, -1], 
                                grouping = id[[i.com]][[j]],
                                perm = 99, 
                                season = i.season,
                                typology = j
                        ) 
                }
                
                ## drop list structure that was created by the safe execution
                #j.res <-j.res$result
                ## create name to save object to file 
                j.save_name <- 
                        paste0(i.com,".",j) |> 
                        str_replace_all("\\.", "_") |> 
                        paste0(".rds")
                ## save to file 
                saveRDS(j.res, 
                        paste0(
                                "data/02_combined_data/indval_single_results//", 
                                Sys.Date(), 
                                "_",
                                j.save_name
                                )
                        )
                ## print feedback 
                print(paste(
                        "# ---- ",
                        format(Sys.time(), "%H:%M"),
                        " ----",
                        j.save_name,
                        "saved to file. ---- #"
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
                str_extract(i,"04_.*") |> 
                        str_remove("04_") |> 
                        str_remove("\\.rds")
        assign(x = object_name, 
               value = x)
}

all_data <- rbindlist(
        list(
               sp_i_brt$data,
               su_i_brt$data,
               au_i_brt$data,
               sp_i_bgr$data,
               su_i_bgr$data,
               au_i_bgr$data,
               sp_i_ife$data,
               su_i_ife$data,
               au_i_ife$data
               )
               )

all_pvalue <- rbindlist(list(
        sp_i_brt$p.values,
        su_i_brt$p.values,
        au_i_brt$p.values,
        sp_i_bgr$p.values,
        su_i_bgr$p.values,
        au_i_bgr$p.values,
        sp_i_ife$p.values,
        su_i_ife$p.values,
        au_i_ife$p.values
)
)


# save to file ----------------------------------------------------------------------
saveRDS(all_data, 
        paste0("data/02_combined_data/",Sys.Date(),"_results_indval_data.rds"))
saveRDS(all_pvalue, 
        paste0("data/02_combined_data/",Sys.Date(),"_results_indval_pvalue.rds"))

        
