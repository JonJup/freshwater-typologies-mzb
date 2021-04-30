# ------------------------------------- #
### --- Compute Indicator Values  --- ### 
# ------------------------------------- #

# --------------- #
# date:         17.03.21
# files out:
#               06_sxs_list.RDS | species X sites table   
# called by:
#               07_derive_typical_assemblages.R
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#               Compute A and B values of IndVal  
# --------------- #

# load data  --------------------------------------------------------------
ls_mzb = readRDS("data/06_sxs_list.RDS")

ls_names = list()
# join data ---------------------------------------------------------------
ls_names$spe <- names(ls_mzb$spe)[-c(1,2)]
ls_names$gen <- names(ls_mzb$gen)[-c(1,2)]
ls_names$foh <- names(ls_mzb$foh)[-c(1,2)]

if(any(ls_names$spe %in% ls_names$gen) |
   any(ls_names$foh %in% ls_names$gen) |
   any(ls_names$foh %in% ls_names$spe)) {
        print("quality check failed")
}

dt_all = ls_mzb$spe[ls_mzb$gen, on = "gr_sample_id"]
dt_all = dt_all[ls_mzb$foh, on = "gr_sample_id"]

dt_all[is.na(ls_bd_20) & !is.na(i.ls_bd_20), ls_bd_20 := i.ls_bd_20]
dt_all[is.na(ls_bd_20) & !is.na(i.ls_bd_20.1), ls_bd_20 := i.ls_bd_20.1]
dt_all[, c("i.ls_bd_20", "i.ls_bd_20.1") := NULL]

# Remove NAs 
for (j in seq_len(ncol(dt_all))) set(dt_all, which(is.na(dt_all[[j]])), j, 0)

rm(j, ls_mzb);gc()

# clean data --------------------------------------------------------------
# How well a stream type is represented was decided visually based on sample site /stream type maps
# Following lines are referred to in "08_seasonal_typical_assemblages.R" as 47:50
ch_acc    = paste0("RT", c(1,2,3,4,5,8,9, 10, 11, 14, 15, 16, 18))

# subset data sets according to the visual categorization of stream type representation
dt_all <- dt_all[ls_bd_20 %in% ch_acc]

rm(ch_acc)
# read x_ls_combine if it is missing 
if (!"x_ls_combine" %in% ls()) {
        source(textConnection(readLines("R/07_derive_typical_assemblages.R")[29]))
}
# combined streame types 
for (i in seq_along(x_ls_combine)){
        loop_var = as.character(x_ls_combine[[i]])
        ch_new_name = paste0(loop_var, collapse  = "_")
        ch_new_name = paste0("RT",ch_new_name)
        loop_var = paste0("RT", loop_var)
        dt_all[ls_bd_20 %in% loop_var, ls_bd_20 := ch_new_name]
        rm(ch_new_name, loop_var, i)
}

# Turn river type into factor. 
dt_all$ls_bd_20 %<>%
        factor %>%
        droplevels

ch_rt = unique(dt_all$ls_bd_20)

taxa = colnames(dt_all)[-c(1,2)]

new_indval = data.table(
        taxon = rep(taxa, times = length(ch_rt)),
        rt = rep(ch_rt, each = length(taxa)),
        A  = 0,
        B = 0
)
for (i in seq_along(taxa)){
        
        loop_var = taxa[i]
        loop_dt = dt_all[,c("ls_bd_20", loop_var), with = F]
        id = as.vector(loop_dt[,2] == 1)
        loop_dt = loop_dt[id]
        loop_tbl = table(loop_dt)
        loop_div = nrow(loop_dt)
        loop_vec = as.vector(table(loop_dt)/nrow(loop_dt) )
        loop_rt = rownames(loop_tbl)
        
        for(k in seq_along(loop_vec)){
                new_indval[taxon == loop_var & rt == loop_rt[k], A := loop_vec[k]]   
        }
        rm(list = ls()[grepl("^loop", ls())])
        rm(i)
}
for (i in seq_along(ch_rt)){
        
        loop_var = ch_rt[i]
        loop_dt = dt_all[ls_bd_20 == loop_var]
        n_sites = nrow(loop_dt)
        loop_cs = colSums(loop_dt[,-c(1,2)])
        loop_cs2 = loop_cs/n_sites
        names(loop_cs2)[1]
        for(k in seq_along(loop_cs2)){
                new_indval[taxon == names(loop_cs2)[k] & rt == loop_var, B := loop_cs2[k]]   
        }
        rm(i)
}

ls_mzb = list()
ls_mzb$spe = new_indval[taxon %in% ls_names$spe]
ls_mzb$gen = new_indval[taxon %in% ls_names$gen]
ls_mzb$fol = new_indval[taxon %in% ls_names$foh]

# save to file ------------------------------------------------------------
saveRDS(object = ls_mzb,   file = "data/07_indicator_list.rds")


# clean environment -------------------------------------------------------

rm_files = ls()[grepl(pattern = "^dt_", x = ls())]
rm(list = rm_files)
rm_files = ls()[grepl(pattern = "^ch_", x = ls())]
rm(list = rm_files)
rm_files = ls()[grepl(pattern = "^ls_", x = ls())]
rm(list = rm_files)
rm(rm_files)

rm(k, id, n_sites, new_indval, taxa)

print("#--------------------------------------------------------#")
# if (readline("delete dt_all? ") == "yes") rm(list = ls())
