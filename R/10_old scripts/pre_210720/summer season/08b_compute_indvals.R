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

# prepare data  ---------------------------------------------------------------------

## -- prepare for eea/ bgr 
if (choose_typology == "bgr") {
        choose_typology2 = "eea"
} else {
        choose_typology2 = choose_typology
}

## -- reduce to chosen typology 
typ_id = which(names(ls_mzb$spe) == choose_typology)
ls_mzb2 <- ls_mzb 
ls_mzb2$spe <- ls_mzb2$spe[[typ_id]] 
ls_mzb2$gen <- ls_mzb2$gen[[typ_id]] 
ls_mzb2$foh <- ls_mzb2$foh[[typ_id]] 
rm(typ_id)


# join data ---------------------------------------------------------------
ls_names = vector(mode = "list", length = 3)
names(ls_names) <- c("spe", "gen", "foh")
typology_column = vector(mode = "integer", length = 3)
typology_column[1] <- which(names(ls_mzb2$spe) == choose_typology2)
typology_column[2] <- which(names(ls_mzb2$gen) == choose_typology2)
typology_column[3] <- which(names(ls_mzb2$foh) == choose_typology2)


ls_names$spe <- names(ls_mzb2$spe)[-c(1,typology_column[1])]
ls_names$gen <- names(ls_mzb2$gen)[-c(1,typology_column[2])]
ls_names$foh <- names(ls_mzb2$foh)[-c(1,typology_column[3])]

if(any(ls_names$spe %in% ls_names$gen) |
   any(ls_names$foh %in% ls_names$gen) |
   any(ls_names$foh %in% ls_names$spe)) {
        print("quality check failed")
}

dt_all = ls_mzb2$spe[ls_mzb2$gen, on = "gr_sample_id"]
dt_all = dt_all[ls_mzb2$foh, on = "gr_sample_id"]


typology_column = which(stringr::str_detect(names(dt_all), choose_typology2))
site_type = data.table(gr_sample_id = unique(dt_all$gr_sample_id), 
                       typ1 = pull(dt_all[,typology_column[1], with = F]),
                       typ2 = pull(dt_all[,typology_column[2], with = F]),
                       typ3 = pull(dt_all[,typology_column[3], with = F])
                       )
site_type[is.na(typ1) & !is.na(typ2), typ1 := typ2]
site_type[is.na(typ1) & !is.na(typ3), typ1 := typ3]
#names(site_type)[2] = choose_typology
site_type = site_type[,c(1,2)]
del_names = names(dt_all)[typology_column]
dt_all[, (del_names) := NULL]
dt_all2 = dt_all[site_type, on = "gr_sample_id"]

# Remove NAs 
for (j in seq_len(ncol(dt_all2))) set(dt_all2, which(is.na(dt_all2[[j]])), j, 0)

# clean data --------------------------------------------------------------

if (choose_typology == "brt12") {
        ch_acc    =  paste0("RT", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
        dt_all2 <- dt_all2[typ1 %in% ch_acc]
        rm(ch_acc)
} else if (choose_typology == "brt20") {
        ch_acc    =  paste0("RT", c(1, 2, 3, 4, 5, 8, 9, 10, 11, 14, 15, 16, 18))
        dt_all2 <- dt_all2[typ1 %in% ch_acc]
        rm(ch_acc)
}

unique(dt_all2$typ1)


dt_all2[, typ1 := stringr::str_remove(typ1, "RT")]
## --  combine types 
for (i in seq_along(combine_types)){
        loop_var = as.character(combine_types[[i]])
        ch_new_name = paste0(loop_var, collapse  = "_")
        loop_var = paste0(loop_var)
        dt_all2[typ1 %in% loop_var, typ1 := ch_new_name]
        rm(ch_new_name, loop_var, i)
}

# Turn river type into factor. 
dt_all2$typ1 %<>%
        factor %>%
        droplevels

ch_rt = unique(dt_all2$typ1)

taxa = colnames(dt_all2)[-c(1,which(names(dt_all2) == "typ1"))]

new_indval = data.table(
        taxon = rep(taxa, times = length(ch_rt)),
        rt = rep(ch_rt, each = length(taxa)),
        A  = 0,
        B = 0
)

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

ls_mzb3 = list()
ls_mzb3$spe = new_indval[taxon %in% ls_names$spe]
ls_mzb3$gen = new_indval[taxon %in% ls_names$gen]
ls_mzb3$fol = new_indval[taxon %in% ls_names$foh]

# save to file ------------------------------------------------------------
save_name = paste0("data/summer season/07_indicator_list_", choose_typology ,".rds")
saveRDS(object = ls_mzb3,   file = save_name)


# clean environment -------------------------------------------------------
keep = c("ls_mzb3", "choose_typology", "combine_types", "thresholds", "ls_mzb", "keep")
remove = setdiff(ls(), keep)
rm(list = remove)
rm(keep, remove)
gc()

print("#--------------------------------------------------------#")
# if (readline("delete dt_all? ") == "yes") rm(list = ls())
