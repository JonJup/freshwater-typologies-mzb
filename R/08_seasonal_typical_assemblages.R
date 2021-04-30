# ----------------------------------------------------- #
### --- Macroinvertebrates -------------------------- ### 
### --- Seasonal Typical Assemblages   -------------- ### 
# ----------------------------------------------------- #


# --------------- #
# date:  
#               17.03.21
# files in 
#               -> 05_final_taxon.rds | 
#               -> 08_sxs_list.rds    | 
# files out
#               
# Project: 
#               Evaluating European Broad River Types for Macroinvertebrates
# Purpose: 
#               Create seasonal typical assemblages
# --------------- #

# setup -----------------------------------------------
source("R/setup_combined_inv.R") 

# load data -------------------------------------------
dt_all     = readRDS("data/05_final_taxon.rds")
dt_sxs     = readRDS("data/06_sxs_list.rds")
ch_col     = c("#d95f02", "#666666", "#5f64ff", "#dcce00")

# prepare data ---------------------------------------------------------------
# subset to accepted river types
source(textConnection(readLines(file.path(dir$hlp, "07_b_compute_indvals.R"))[47:50]))

# rename river type variable 
dt_all[,c("rt", "ls_bd_20") := .(ls_bd_20, NULL)]
# drop cols 
dt_all[, c("species", "genus", "family", "order") := NULL]
# create season
dt_all[, 
       season := case_when(
               month(date) %in% c(3, 4, 5)   ~ "spring",
               month(date) %in% c(6, 7, 8)   ~ "summer",
               month(date) %in% c(9, 10, 11) ~ "autumn",
               month(date) %in% c(12, 1, 2)  ~ "winter", 
                           )
       ]
# drop rows 
dt_all = dt_all[!is.na(rt) & 
                          !is.na(season) & 
                          !is.na(geometry) & 
                          (date >= as.Date("2000-01-01") | is.na(date)) & 
                          rt %in% ch_acc &
                          (rt %in% dt_sxs$spe$ls_bd_20 | rt %in% dt_sxs$gen$ls_bd_20) 
]

# fix season column 
dt_all$season %<>% 
        factor(levels = c("spring", "summer", "autumn", "winter")) %>% 
        droplevels()

# combine river types  
source(textConnection(readLines("R/07_derive_typical_assemblages.R")[32]))

for (i in seq_along(x_ls_combine)){
        loop_var    = as.character(x_ls_combine[[i]])
        ch_new_name = paste0(loop_var, collapse  = "_")
        ch_new_name = paste0("RT",ch_new_name)
        loop_var    = paste0("RT", loop_var)
        dt_all[rt %in% loop_var, rt := ch_new_name]
        rm(ch_new_name, loop_var,i)
}

dt_data = dt_all
rm(dt_all)
# explore and subset data for seasonal assemblages ------------------------
source("R/helper/08_b_explore_seasonal_assemblages.R")

# site X taxon tables  ------------------------------------------------------
ch_river_types = unique(dt_data$rt)
# one data.table for each river type 
ls_rt = split.data.frame(dt_data, dt_data$rt)
# subset 
ls_rt %<>% lapply(function(x)x[,.(gr_sample_id, final_taxon, final_taxon_level, season)] )

# split taxa 
ls_spe = lapply(ls_rt, function(x)x[final_taxon_level == "species"])
ls_gen = lapply(ls_rt, function(x)x[final_taxon_level == "genus"])
ls_foh = lapply(ls_rt, function(x)x[final_taxon_level == "family"])

# loop creating sxs 
for (i in seq_along(ls_rt)) {
        
        ld_spe = ls_spe[[i]]
        ld_gen = ls_gen[[i]]
        ld_foh = ls_foh[[i]]
        
        ld_spe[, final_taxon_level := NULL]
        ld_gen[, final_taxon_level := NULL]
        ld_foh[, final_taxon_level := NULL]
        
        # extra table with id and season
        ldj_spe <- copy(ld_spe)
        ldj_gen <- copy(ld_gen)
        ldj_foh <- copy(ld_foh)
        ldj_spe[, final_taxon  := NULL] 
        ldj_gen[, final_taxon  := NULL]
        ldj_foh[, final_taxon  := NULL]
        ldj_spe %<>% unique(by = "gr_sample_id")
        ldj_gen %<>% unique(by = "gr_sample_id")
        ldj_foh %<>% unique(by = "gr_sample_id")
        
        spe_sxs =  splist2presabs(data = ld_spe, sites.col = 1, sp.col = 2) %>% setDT
        gen_sxs =  splist2presabs(data = ld_gen, sites.col = 1, sp.col = 2) %>% setDT
        foh_sxs =  splist2presabs(data = ld_foh, sites.col = 1, sp.col = 2) %>% setDT
        
        ls_spe[[i]] <- ldj_spe[spe_sxs, on = "gr_sample_id"]
        ls_gen[[i]] <- ldj_gen[gen_sxs, on = "gr_sample_id"]
        ls_foh[[i]] <- ldj_foh[foh_sxs, on = "gr_sample_id"]
        
        rm(ld_spe,ld_gen,ld_foh,
           ldj_spe,i,ldj_gen, ldj_foh,
           spe_sxs,gen_sxs,foh_sxs);gc()
        
}

# prepare NMDS --------------------------------------------------------------------
ls_mzb = list(ls_spe, ls_gen, ls_foh)
ls_mzb$join = list()

# loop over river types 
for (i in seq_along(ls_mzb[[2]])) {
        # select different taxonomic level for a river type 
        lo_spe = ls_mzb[[1]][[i]]
        lo_gen = ls_mzb[[2]][[i]]
        lo_foh = ls_mzb[[3]][[i]]
        
        # join species and genera and then both with family. Add now table to 
        # 4th element of list "join". 
        ls_mzb$join[[i]] = merge(lo_gen, lo_spe, by = "gr_sample_id", all = TRUE)
        ls_mzb$join[[i]] = merge(lo_foh, ls_mzb$join[[i]], by = "gr_sample_id", all = TRUE ) 
        
        # sanity checks 
        if (nrow(ls_mzb$join[[i]]) < max(nrow(lo_spe),  nrow(lo_gen), nrow(lo_foh)) ){
                print(paste("broke at ",i))
                break()
        }
        if (any(duplicated(ls_mzb$join[[i]]$gr_sample_id))) {
                print(paste(i, "after 1"))
        }
        
        names(ls_mzb$join)[i] = names(ls_mzb[[1]])[i]
        rm(lo_spe, lo_gen, lo_foh, i)
        gc()
}
# combine season columns
ls_mzb$join %<>% lapply(fill_season)

# check if any NAs in season column remain
for (i in seq_along(ls_mzb$join)){
        if (i == 1) seas_na = c()
        seas_na[i] = nrow(ls_mzb$join[[i]][is.na(season)])
        if (i == length(ls_mzb$join)) {
                print(paste("Quality Check", ifelse(sum(seas_na) == 0, "passed", "failed")))
                rm(i, seas_na)
        }
}

# remove season.x and season.y columns that was created during the joins. 
# Also replace NAs in occurrences with zeros 
for (i in seq_along(ls_mzb$join)) {
        ls_mzb$join[[i]][, c("season.x", "season.y") := NULL]
        for (j in seq_len(ncol(ls_mzb$join[[i]]))) {
                set(ls_mzb$join[[i]], which(is.na(ls_mzb$join[[i]][[j]])), j, 0)
        }
        rm(i, j)
        gc()
}

# compute distance --------------------------------------------------

# loop to create distance matrices 
ls_season = list()
ls_mzb$distance = list()
for (i in seq_along(ls_mzb$join)) {
        ld                   = t(ls_mzb$join[[i]])
        colnames(ld)         = ld[1,]
        ld                   = ld[-1,]
        ls_season[[i]]       = ld[1,]
        names(ls_season)[i]  = names(ls_mzb$join)[i]
        ld                   = ld[-1,]
        taxa_names           = rownames(ld)
        ld                   = apply(ld, 2, as.numeric)
        rownames(ld)         = taxa_names
        ls_mzb$distance[[i]] = parallelDist(x = ld,
                                                    method = "binary",
                                                    threads = 4)
        names(ls_mzb$distance)[i]  = names(ls_mzb$join)[i]
        print(i)
        rm(ld, i)
        gc()
}

# run NMDS ----------------------------------------------------------------
ls_mzb$nmds = list()
for (i in seq_along(ls_mzb$distance)) {
        
        # skip already finished ones 
        # if (i == 1) next()
        ch_save_name = names(ls_mzb$join)[i]
        converged    = FALSE
        iter         = 0
        
        while (converged == FALSE & iter < 2000) {
                
                iter = iter + 1000
                print(paste(iter, "for", i))
                
                if (iter == 1000) {
                        
                        loop_nmds  = metaMDS(comm = ls_mzb$distance[[i]],
                                             try = iter,
                                             k = 2,
                                             parallel = 8)
                        
                } else {
                        
                        loop_nmds = metaMDS(
                                comm = ls_mzb$distance[[i]],
                                try = iter,
                                k = 2,
                                previous.best = loop_nmds,
                                parallel = 8
                        )
                        
                }
                
                converged = loop_nmds$converged
        }
        
        ch_save_name = ifelse(
                loop_nmds$converged,
                paste0(ch_save_name, "_iter", iter, "_converged"),
                paste0(ch_save_name, "_iter", iter, "_not_converged")
        )
        
        saveRDS(object = loop_nmds,
                file = paste0("data/nmds/",ch_save_name, ".rds")
                )
        ls_mzb$nmds[[i]] = loop_nmds
        names( ls_mzb$nmds)[i] = names(ls_mzb$join)[i]
        rm(i, ch_save_name, loop_nmds)
}
names(ls_mzb$nmds) %<>% 
        str_remove("iter[0-9]*") %>% 
        str_remove("__") %>% 
        str_remove("not") %>% 
        str_remove("converged.RDS") %>% 
        str_remove("_$")

# plot 
for (i in seq_along(ls_mzb$nmds)) {
        if (i == 1) ls_mzb$dt_plot = ls_mzb$dt_hull = list()
        
        in_seas_id = which(names(ls_season) == names(ls_mzb$nmds)[i])
        
        ls_mzb$dt_plot[[i]] = 
                data.table(NMDS1 = scores(ls_mzb$nmds[[i]])[,1],  
                           NMDS2 = scores(ls_mzb$nmds[[i]])[,2], 
                           season = factor(ls_season[[in_seas_id]], 
                                           levels = c("spring", "summer", "autumn", "winter")
                           )
                )
        # hulls 
        ls_mzb$dt_hull[[i]] = 
                ls_mzb$dt_plot[[i]]  %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
        
        print(paste("Finished", i))
        rm(in_seas_id, i)
        
        gc()
}

# color palette 
ch_col_ord = c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")

for (i in seq_along(ls_mzb$nmds)) {
        if (i == 1) ls_mzb$gg = list()
        
        id = which(names(ls_mzb$join) == names(ls_mzb$nmds)[i])
        
        ls_mzb$gg[[i]] = 
                ggplot(data = ls_mzb$dt_plot[[i]],
                       aes(x = NMDS1, y = NMDS2)) +
                # geom_polygon(data = ls_mzb$dt_hull[[i]],   
                #              alpha = 0.5, 
                #              aes(fill = season)) +
                # stat_density_2d(aes(fill = season), geom = "polygon", alpha = 0.5) + 
                stat_density_2d(aes(fill = season), geom = "polygon", alpha = 0.4, h = .25) + 
                facet_wrap(.~season) + 
                geom_point(aes(fill = season), 
                           shape = 21) +
                ggtitle(paste0("NMDS", " ", names(ls_mzb$join)[id])) +
                labs(fil = "Season",
                     subtitle = paste0("Macroinvertebrates, Stress: ", round(ls_mzb$nmds[[i]]$stress,   2))) +
                scale_fill_manual(values = ch_col_ord[c(1, 2, 4, 6)]) + 
                scale_color_manual(values = ch_col_ord[c(1, 2, 4, 6)])  + 
                theme_minimal()
        
        
}

for (i in seq_along(ls_mzb$gg)) {
        id = which(names(ls_mzb$join) == names(ls_mzb$nmds)[i])
        
        ggsave(filename = file.path(dir$plt_ndms,
                                    paste0(names(
                                            ls_mzb$join
                                    )[id],
                                    ".png")),
               plot = ls_mzb$gg[[i]])
}

# derive TA  --------------------------------------------------------------
rt2s = ls_spe$RT2
rt2g = ls_gen$RT2
rt2f = ls_foh$RT2

rt8s = ls_spe$RT8_10_11_18
rt8g = ls_gen$RT8_10_11_18
rt8f = ls_foh$RT8_10_11_18

n_spe_2 <- ncol(rt2s) - 2
n_gen_2 <- ncol(rt2g) - 2
n_fol_2 <- ncol(rt2f) - 2
n_spe_8 <- ncol(rt8s) - 2
n_gen_8 <- ncol(rt8g) - 2
n_fol_8 <- ncol(rt8f) - 2

rt2all = rt2s[rt2g, on = "gr_sample_id"]
rt2all = rt2all[rt2f, on = "gr_sample_id"]
rt8all = rt8s[rt8g, on = "gr_sample_id"]
rt8all = rt8all[rt8f, on = "gr_sample_id"]

rt2all[is.na(season) & !is.na(i.season), season := i.season]
rt2all[is.na(season) & !is.na(i.season.1), season := i.season.1]
rt2all[, c("i.season", "i.season.1") := NULL]
rt8all[is.na(season) & !is.na(i.season), season := i.season]
rt8all[is.na(season) & !is.na(i.season.1), season := i.season.1]
rt8all[, c("i.season", "i.season.1") := NULL]

# Remove NAs 
for (j in seq_len(ncol(rt2all))) set(rt2all, which(is.na(rt2all[[j]])), j, 0)
for (j in seq_len(ncol(rt8all))) set(rt8all, which(is.na(rt8all[[j]])), j, 0)

# test that taxa vector will work 
if (ncol(rt2all) == 2 + n_spe_2 + n_gen_2 + n_fol_2 &
    ncol(rt8all) == 2 + n_spe_8 + n_gen_8 + n_fol_8)
        print ("Quality Check passed") else
                print("Quality Check faied")

rm(rt2f, rt2g, rt2s, rt8f, rt8g, rt8s, j);gc()
        
rt2all$season %<>% droplevels()
rt8all$season %<>% droplevels()

# compute indval ----------------------------------------------------------

# combine in one list 
l_pre_lp = list(rt2all[,c(3:n_spe_2), with = F],
                  rt2all[,c((n_spe_2 + 1):(n_spe_2 + n_gen_2)), with = F],
                  rt2all[,c((n_spe_2 + n_gen_2 + 1):ncol(rt2all)), with = F],
                  rt8all[,c(3:n_spe_8), with = F],
                  rt8all[,c((n_spe_8 + 1):(n_spe_8 + n_gen_8)), with = F],
                  rt8all[,c((n_spe_8 + n_gen_8 + 1):ncol(rt8all)), with = F]
                  )
# unique season levels 
ls_seasons = list("two" = levels(rt2all$season),
                  "eight" = levels(rt8all$season))
# variables to use as grouping variable in j loop 
ls_season_group = list(rt2all$season, 
                       rt8all$season)

# load thresholds 
source(textConnection(readLines("R/07_derive_typical_assemblages.R")[27:29]))

# loop over river types 
for (k in seq_along(ls_seasons)) {
    # loop over taxonomic levels
    for (i in 1:3) {
        if (i == 1) {
            l_l = list(spe = list(),
                      gen = list(),
                      fol = list())
        }
        lp_dt = l_pre_lp[[i+(k-1)*3]]
        # loop over seasons 
        for (j in seq_along(ls_seasons[[k]])) {
            lp_season = ls_seasons[[k]][j]
            rt = indicators(
                X = lp_dt,
                cluster = ls_season_group[[k]],
                group = lp_season,
                permutations = 1,
                max.order = 1,
                func = "IndVal.g"
            )
            l_l[[i]][[j]] = data.table(
                taxon = rt$finalsplist,
                A     = rt$A,
                B     = rt$B
            )
            names(l_l[[i]])[j] = lp_season
        } # END j 
    }# END i 
    
    l_l$spe %<>% 
        lapply(function(x) x[(A>x_ls_thesholds$spe$a & B > x_ls_thesholds$spe$b2)| B>x_ls_thesholds$spe$b])
    l_l$gen %<>% 
        lapply(function(x) x[B>x_ls_thesholds$gen$b])
    l_l$fol %<>% 
        lapply(function(x) x[B>x_ls_thesholds$gen$b])
    
    lp_spring = rbindlist(
        list(
            l_l$spe$spring,
            l_l$gen$spring,
            l_l$fol$spring
        )
    )
    assign(x = paste0("ta_spring_", c(2,8)[k]),
           value = lp_spring)
    lp_summer = rbindlist(
        list(
            l_l$spe$summer,
            l_l$gen$summer,
            l_l$fol$summer
        )
    )
    assign(x = paste0("ta_summer_", c(2,8)[k]),
           value = lp_summer)
    lp_autumn = rbindlist(
        list(
            l_l$spe$autumn,
            l_l$gen$autumn,
            l_l$fol$autumn
        )
    )
    assign(x = paste0("ta_autumn_", c(2,8)[k]),
           value = lp_autumn)
    if ("winter" %in% ls_seasons[k]){
        lp_winter = rbindlist(
            list(
                l_l$spe$winter,
                l_l$gen$winter,
                l_l$fol$winter
            )
        )
        assign(x = paste0("ta_winter_", c(2,8)[k]),
               value = lp_winter)  
    }
}# END k 

# add some information and save to file 
ta_files = ls()[grepl("^ta_", ls())]
ta_ls = list()
for (i in seq_along(ta_files)){
    x = get(ta_files[i])
    x %<>% 
        mutate(season = str_remove_all(str_extract(ta_files[i], "_.*_"),"_"), 
               taxon = str_replace_all(taxon, "\\.", "\\ "), 
               river_type = str_remove_all(str_extract(ta_files[i], "_[1-8]$"),"_")) %>% 
        select(!c("A", "B"))
    ta_ls[[i]] = x
}
ta_ls %<>% rbindlist()

saveRDS(ta_ls, "08_seasonal_typical_assemblages.rds")
