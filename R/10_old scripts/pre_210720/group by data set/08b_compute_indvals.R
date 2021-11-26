# ————————————————————————————————————— #
### ——— Compute Indicator Values  ——— ### 
### ——— Group by data set         ——— ### 
# ————————————————————————————————————— #
 
# —————————————————————————————————————
# date:         09.07.21
# files out:
#               06_sxs_list.RDS | species X sites table   
# called by:
#               07_derive_typical_assemblages.R
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#               Compute A and B values of IndVal  (really A and B?)
# —————————————————————————————————————


# prepare objects  ------------------------------------------------------------------
#- list holding three character vectors. Each holding the names of all taxa in the elements of ls_mzb. 
ls_names           <- vector(mode = "list", length = 3)
#- vector holding the positions of the data.set column in the elements of ls_mzb
typology_column    <- vector(mode = "integer", length = 3)

#- name elements 
names(ls_names)    <- c("spe", "gen", "foh")

#- fill typology_column 
typology_column[1] <- which(names(ls_mzb$spe) == choose_typology)
typology_column[2] <- which(names(ls_mzb$gen) == choose_typology)
typology_column[3] <- which(names(ls_mzb$foh) == choose_typology)

#- fill ls_names
ls_names$spe <- names(ls_mzb$spe)[-c(1,typology_column[1])]
ls_names$gen <- names(ls_mzb$gen)[-c(1,typology_column[2])]
ls_names$foh <- names(ls_mzb$foh)[-c(1,typology_column[3])]

#- check if any names occur in multiple taxonomic levels 
if(any(ls_names$spe %in% ls_names$gen) |
   any(ls_names$foh %in% ls_names$gen) |
   any(ls_names$foh %in% ls_names$spe)) {
        print("quality check failed")
} else {print("quality check passed")}

#- join elements of ls_mzb
dt_all <- ls_mzb$spe[ls_mzb$gen, on = "gr_sample_id"]
dt_all <- dt_all[ls_mzb$foh, on = "gr_sample_id"]

#- now we have multiple data.set columns. 
#- find their locations 
typology_column <- which(stringr::str_detect(names(dt_all), choose_typology))
#- create a data.table with the id and all data.set columns
site_type <- data.table(gr_sample_id = unique(dt_all$gr_sample_id), 
                       typ1 = pull(dt_all[,typology_column[1], with = F]),
                       typ2 = pull(dt_all[,typology_column[2], with = F]),
                       typ3 = pull(dt_all[,typology_column[3], with = F])
                       )
#- if they are not NA they should all be equal for each observation (i.e. site)
if (all (site_type[!is.na(typ1) & !is.na(typ2), all(typ1 == typ2)], 
         site_type[!is.na(typ2) & !is.na(typ3), all(typ2 == typ3)],
         site_type[!is.na(typ1) & !is.na(typ3), all(typ1 == typ3)])){
        print("quality check passed") 
} else {
        print("quality check failed")
}
#- fill up columns 
site_type[is.na(typ1) & !is.na(typ2), typ1 := typ2]
site_type[is.na(typ1) & !is.na(typ3), typ1 := typ3]
#- drop later two column as all their information is now in the second column 
site_type = site_type[,c(1,2)]

#- remove original typology columns from dt_all 
del_names = names(dt_all)[typology_column]
dt_all[, (del_names) := NULL]
dt_all2 = dt_all[site_type, on = "gr_sample_id"]

# Fill NAs with 0 
for (j in seq_len(ncol(dt_all2))) set(dt_all2, which(is.na(dt_all2[[j]])), j, 0)

#-  combine types 
for (i in seq_along(combine_types)){
        loop_var = as.character(combine_types[[i]])
        ch_new_name = paste0(loop_var, collapse  = "_")
        loop_var = paste0(loop_var)
        dt_all2[typ1 %in% loop_var, typ1 := ch_new_name]
        rm(ch_new_name, loop_var, i)
}

#- Turn data.set into factor. 
dt_all2$typ1 %<>%
        factor %>%
        droplevels

#- character vector with unique data.sets
ch_rt = unique(dt_all2$typ1)
#- character vector with all taxa names 
taxa = colnames(dt_all2)[-c(1,which(names(dt_all2) == "typ1"))]
#- prepare table for indval 
new_indval = data.table(
        taxon = rep(taxa, times = length(ch_rt)),
        rt = rep(ch_rt, each = length(taxa)),
        A  = 0,
        B = 0
)
#- compute A
for (i in seq_along(taxa)){
        
        loop_var = taxa[i]
        loop_dt = dt_all2[,c("typ1", loop_var), with = F]
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
#- compute B
for (i in seq_along(ch_rt)){
        
        loop_var = ch_rt[i]
        loop_dt = dt_all2[typ1 == loop_var]
        n_sites = nrow(loop_dt)
        loop_cs <- loop_dt[, -c("gr_sample_id","typ1")]
        loop_cs = colSums(loop_cs)
        loop_cs = loop_cs/n_sites
        names(loop_cs)[1]
        for(k in seq_along(loop_cs)){
                new_indval[taxon == names(loop_cs)[k] & rt == loop_var, B := loop_cs[k]]   
        }
        rm(i, loop_cs, n_sites, loop_var)
}
#- divide the indicator values in taxonomic lelves 
ls_mzb3 = list()
ls_mzb3$spe = new_indval[taxon %in% ls_names$spe]
ls_mzb3$gen = new_indval[taxon %in% ls_names$gen]
ls_mzb3$fol = new_indval[taxon %in% ls_names$foh]

# save to file ------------------------------------------------------------
save_name <- paste0("data/group by data set/07_indicator_list_", save.name.var ,".rds")
saveRDS(object = ls_mzb3,   file = save_name)


# clean environment -------------------------------------------------------
# keep = c("ls_mzb3", "choose_typology", "combine_types", "thresholds", "ls_mzb", "keep")
# remove = setdiff(ls(), keep)
# rm(list = remove)
# rm(keep, remove)
# gc()

print("#--------------------------------------------------------#")
