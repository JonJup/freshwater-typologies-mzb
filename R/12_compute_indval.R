### --- Indicator genera Macroinvertebrates --- ### 

# --------------- #
# date:  05.02.21
# files in 
# files out
# macroinvertebrates ins lyche-solheim paper  
# determine Indicator Genera for the stream types of Lyche-Solheim et al. (2020)
# --------------- #

# setup -----------------------------------------------
source(here::here("R/setup_combined_inv.R"))

# load data -------------------------------------------
data = readRDS("data/06_sxs_genus.RDS")


# prepare data ------------------------------------------------------------
setDT(data)
data2 = copy(data)
data2[,c("gr_sample_id", "ls_bd_20") := NULL]
accepted_rivers = paste0("RT", c(1,2,3,4,5,8,9, 10, 11, 14, 15, 16, 18))

for (i in seq_along(accepted_rivers)) {
        
        if (i == 1){
                ls_indval = list()
        }
        
        group_var = accepted_rivers[i]
        print(paste(group_var, "start @", format(Sys.time(), "%H:%M:%S")))

        if (nrow(data[ls_bd_20 == group_var]) == 0)
                next()

        ls_indval[[i]] = indicators(
                X = data2,
                cluster = data$ls_bd_20,
                group = group_var,
                max.order = 1,
                verbose = FALSE,
                At = 0,
                Bt = 0,
                func = "IndVal.g",
                control = how(nperm = 15000)
        )
        
        saveRDS(ls_indval, paste0("data/temp/indval_",i,".rds"))
        print(paste(group_var, "ended @", format(Sys.time(), "%H:%M:%S")))
        rm(group_var, i)
}

saveRDS(ls_indval, "data/14_indval_list.rds")

# Find Indicator Taxa  ----------------------------------------------------

ls_indval = readRDS("data/14_indval_list.rds")
## -- reshape from indicator class format 
ls_indval2 = lapply(ls_indval, 
                    function(x) data.table(taxon  = x$candidates, 
                                           pvalue = x$p.value,
                                           indval = x$sqrtIV))
## -- drop taxa that do not occur in the river type 
ls_indval2 %<>%
        lapply(function(x) x[indval != 0])
## -- number of tests = number of taxa. 
## -- Family-wise error only corrected within each river type
n_tests =  lapply(ls_indval2, nrow)

## -- Holm's step down procedure. Not a good idea because permutation based p-values
## -- result in many entries with the exact same p-value. Im those cases the alphabetic order 
## -- can influence whether a taxon is found to be statistically significat indicator 
## -- or not. 

#ls_indval2 %<>% lapply(setorderv,
#"pvalue") %>% lapply(function(x) x[, holms_p := 0.05 / n_tests:(n_tests-(nrow(x)-1))])
#%>% lapply(function(x) x[, indicator := pvalue <= holms_p])

## ---------------- ##
## -- Bonferroni -- ## 
## ---------------- ##

## -- add a variable with the number of tests to each table 
ls_indval2 = map(.x = 1:length(ls_indval2),
                 .f = ~ ls_indval2[[.x]][, n_tests := n_tests[.x]])

ls_indval2 %<>%
        ## -- add bonferroni p-value 
        lapply(function(x)
                x[, bonferroni_p := 0.05 / n_tests]) %>%
        ## -- compare to bonferroni p-value 
        lapply(function(x)
                x[, indicator_bonferroni := pvalue <= bonferroni_p])

## ------------------ ##
## --- Dunn Sidak --- ##
## ------------------ ## 

ls_indval2 %<>%
        ## -- add Dunn-Sidak p-value 
        lapply(function(x)
                x[, dunn_sidak_p := 1-(1-0.05)^(1/n_tests)]) %>%
        ## -- compare to bonferroni p-value 
        lapply(function(x)
                x[, indicator_dunn_sidak := pvalue <= dunn_sidak_p])


## ---------------------- ## 
## --- inspect results -- ## 
## ---------------------- ## 
## -- does every type have indicators 
for (i in 1:length(ls_indval2)){
        print(any(ls_indval2[[i]]$indicator_bonferroni))
        
}
## -- how many?
for (i in 1:length(ls_indval2)){
        
        sum1 = sum(ls_indval2[[i]]$indicator_bonferroni)
        sum2 = sum(ls_indval2[[i]]$indicator_dunn_sidak)
        
        print(paste(sum1, sum2))
        
}

## --> It does not matter which correction we use 

## -- subset to indicator taxa 
ls_indval3 = lapply(ls_indval2, function(x) x[indicator == TRUE])

# save data -------------------------------------------

saveRDS(ls_indval3, "data/15_indicator_list.rds")
saveRDS(ls_indval2, "data/15_indicator_list_w_non_significant.rds")

