# ---------------------------- #
# --- Analysis: IndVal ------- # 
# ---------------------------- #

#__________________________
#  date created: 29-04-22
# last modified: 29-04-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute IndVal Statistic
#__________________________


# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/JonJup/jjmisc")

library(pacman)
p_load(
        data.table,
        indicspecies,
        jjmisc,
        dplyr,
        stringr,
        rstudioapi,
        purrr
)

x<-getActiveDocumentContext()
sink(file = paste0("R/STOTEN-reivew/008","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)


# load data -------------------------------------------------------------------------
id   <- readRDS("data/review/2022-04-29_distance_ids.rds")


sp.i <- readRDS("data/review/2022-04-29_community_data_spring.rds")
su.i <- readRDS("data/review/2022-04-29_community_data_summer.rds")
au.i <- readRDS("data/review/2022-04-29_community_data_autumn.rds")

# analysis --------------------------------------------------------------------------

## files for loop 
communities <- c("sp.i", "su.i", "au.i")

safe_ind <- safely(indvalstat)

for (i in seq_along(communities)){
        if (i != 1) next()
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
                if (j != "brt") next()
                # if (i == 1 & j == "brt") 
                #         next()
                
                print("# ---- Computation 1  ---- #")
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
                        print("# ---- Computation 2  ---- #")
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
                                "data/review/indval_single_results/", 
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
indval_files <- fs::dir_ls(path = "data/review/indval_single_results/")
for (i in indval_files){
        x <- readRDS(i)
        object_name <- 
                str_extract(i,"29_.*") |> 
                str_remove("29_") |> 
                str_remove("\\.rds")
        assign(x = object_name, 
               value = x)
}


all_data <- rbindlist(list(
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
        paste0("data/review/",Sys.Date(),"_results_indval_data_review.rds"))
saveRDS(all_pvalue, 
        paste0("data/review/",Sys.Date(),"_results_indval_pvalue_review.rds"))


