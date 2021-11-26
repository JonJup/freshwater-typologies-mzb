# —————————————————————————————— #
# ——— Analysis: Mantel Tests ——— # 
# —————————————————————————————— #

# ———————————————————————————————————
#  date created: 22-10-21
# last modified: 22-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute MANTEL TESTS
# ———————————————————————————————————

# ——— SUMMARY ——— # 
## AQEM ROMANIA         -- FULL 
## AQEM SWEDEN          -- FULL 
## Project Biodrought   -- Spring and Autumn; all taxonomic levels; full abundance
## Monitoring Czech     -- FULL
## Ebro Hydrographic    -- All seasons, only family, full abundance
## Monitoring Croatia   -- Spring and summer, all taxonomic level, full abundance  
## Monitoring Dutch     -- all seasons, genus and family, full abundance 
## Monitoring Germany   -- FULL
## Monitoring Greece    -- Only PA for family -> no Mantel tests
## Monitoring Poland    -- Only PA for family -> no Mantel tests
## Monitoring Portugal  -- Spring and Summer; only family; full abundance
## Monitoring Slovakia  -- Spring; 
# ——————————————— # 

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, 
               dplyr,
               parallelDist,
               purrr, 
               tidyr,
               vegan)

source("R/helper_functions.R")
# load data -------------------------------------------------------------------------
most.recent.data <-
        fs::dir_ls("data/02_combined_data/", regexp = "_core_taxa_data_aggregated.rds") |>
        stringr::str_remove("data/02_combined_data/02_") |>
        stringr::str_remove("_core_taxa_data_aggregated.rds") |>
        lubridate::ymd() |>
        {
                \(x) x[which.max(x)]
        }()

data <-
        readRDS(
                file = paste0(
                        "data/02_combined_data/02_",
                        most.recent.data,
                        "_core_taxa_data_aggregated.rds"
                )
        )


# Functions  ------------------------------------------------------------------------
setNAcols <- function(x){
        #- How many columns are in x that are not "gr_sample_id"
        x1 <- length(setdiff(names(x), "gr_sample_id"))
        #- How many columns are in x 
        x2 <- ncol(x)
        #- Columns in append("gr_sample_id", typologies) + 1
        x3 <- x2 - x1 + 1
        #- Column range between all columns and columns in append("gr_sample_id", typologies) + 1
        #- This corresponds to all taxa columns.
        x4 <- x3:x2
        #- return output 
        x4
}
prep1 <- function(data, taxon, seasons = season_var){
        #- Character vector with column names to keep
        col.vec <- (c("gr_sample_id", taxon, "abundance2"))
        #- Subset data to columns from col.vec
        x1 <- lapply(seasons, function(x) data[[x]][, .SD, .SDcols = col.vec])
        #- Drop rows where the taxonomic level is coarser then what is required by the
        #- taxon argument.
        
        #- Make sure that each taxon only occurs once per sample. This might be violated
        #- when a coarse taxonomic resolution is used and several taxa from this group were
        #- observed.
        if (taxon == "species"){
                x1 <- lapply(seasons, function(x) x1[[x]][!is.na(species)]) 
                x1 <- lapply(seasons, function(x) x1[[x]][, abundance := sum(abundance2), by = c("species", "gr_sample_id")])
        } else if (taxon == "genus") {
                x1 <- lapply(seasons, function(x) x1[[x]][!is.na(genus)]) 
                x1 <- lapply(seasons, function(x) x1[[x]][, abundance := sum(abundance2), by = c("genus", "gr_sample_id")])
        } else if (taxon == "family"){
                x1 <- lapply(seasons, function(x) x1[[x]][!is.na(family)]) 
                x1 <- lapply(seasons, function(x) x1[[x]][, abundance := sum(abundance2), by = c("family", "gr_sample_id")])
        }
        
        x1 <- lapply(seasons, function(x) unique(x1[[x]], by = c("gr_sample_id", taxon)))
        
        #- Turn to wide format with one column for each taxon and one row per site. 
        x1 <- lapply(x1, pivot_wider, id_cols = "gr_sample_id", names_from = all_of(taxon), values_from = abundance, values_fill = 0)
        #- Turn to data.table. 
        x1 <- lapply(x1, setDT)
        #- Turn NAs to 0 i.e. absence.  
        x2 <- lapply(x1, setNAcols)
        x1 <- lapply(seasons, function(z) setnafill(x = x1[[z]], type = "const", cols = x2[[z]], fill = 0))
        #- remove columns of taxa that are always absent.
        rm.col <- lapply(seasons, function(x) names(which(colSums(x1[[x]][,x2[[x]],with=F]) == 0))) 
        for (i in seasons){
                if(length(rm.col[[i]]) > 0){
                        x1[[i]][, (rm.col[[i]]) := NULL]
                }
        }
        x2 <- lapply(x1, setNAcols)
        #- Remove sites without taxa 
        rm.rows <- lapply(seasons, function(x) which(rowSums(x1[[x]][,x2[[x]],with=F]) == 0))
        for (i in seasons){
                if(length(rm.rows[[i]]) > 0){
                        x1[[x]][-rm.rows[[x]]]
                }
        }
        x1
}
prep2 <- function(x) {
        lapply(
                seq_along(x), 
                function(y) 
                        as.matrix(
                                select(x[[y]], !gr_sample_id)
                                )
        )
}
compile_mantel <- function(x, type, levels = NA, seasons = c("spring", "summer", "autumn") ){
        out <- data.table(
                seasons, 
                data_set = data_set, 
                statistic = unlist(transpose(x)$statistic),
                p_value   = unlist(transpose(x)$signif),
                type,
                levels
        )
        out
}


# Prep ------------------------------------------------------------------------------

## This list will be used to store all results 
final_results <- list()

# AQEM Romania ----------------------------------------------------------------------
data_set <- "Project AQEM (Romania)"
da <- lapply(data, function(x) x[data.set == data_set])
das <- prep1(data = da, taxon = "species")
dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(1:2, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(1:2, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(1:2, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist1[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
        compile_mantel(mantel.tax.sg, type = "taxonomic level", level = "species to genus"),
        compile_mantel(mantel.tax.sf, type = "taxonomic level", level = "species to family"),
        compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
        compile_mantel(mantel.res.ss, type = "numerical resolution", level = "species"),
        compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
        compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
        compile_mantel(mantel.res_tax.sg, type = "both", level = "species to genus"),
        compile_mantel(mantel.res_tax.sf, type = "both", level = "species to family"),
        compile_mantel(mantel.res_tax.gf, type = "both", level = "genus to family")
))
final_results[[1]] <- mantel_results


# AQEM SWEDEN -----------------------------------------------------------------------

data_set <- "Project AQEM (Sweden)"
da <- lapply(data, function(x) x[data.set == data_set])
das <- prep1(data = da, taxon = "species")
dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(1:2, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(1:2, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(1:2, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist1[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg, type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf, type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss, type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, type = "both", level = "genus to family")
        ))
final_results[[2]] <- mantel_results

# Project Biodrought ----------------------------------------------------------------------    
data_set <- "Project Biodrought"
da  <- lapply(data, function(x) x[data.set == data_set])
lapply(da, nrow)
da <- da[c(1,3)]

season_var = 1:2

das <- prep1(data = da, taxon = "species", seasons = season_var)
dag <- prep1(data = da, taxon = "genus",   seasons = season_var)
daf <- prep1(data = da, taxon = "family",  seasons = season_var)

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist1[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg,     seasons = c("spring", "autumn"), type = "taxonomic level",      level = "species to genus"),
                compile_mantel(mantel.tax.sf,     seasons = c("spring", "autumn"), type = "taxonomic level",      level = "species to family"),
                compile_mantel(mantel.tax.gf,     seasons = c("spring", "autumn"), type = "taxonomic level",      level = "genus to family"),
                compile_mantel(mantel.res.ss,     seasons = c("spring", "autumn"), type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg,     seasons = c("spring", "autumn"), type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff,     seasons = c("spring", "autumn"), type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, seasons = c("spring", "autumn"), type = "both",                 level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, seasons = c("spring", "autumn"), type = "both",                 level = "species to family"),
                compile_mantel(mantel.res_tax.gf, seasons = c("spring", "autumn"), type = "both",                 level = "genus to family")
        ))
final_results[[3]] <- mantel_results

# Monitoring data from the Czech Republic ----------------------------------------------------------------------
data_set <- "Monitoring data from the Czech Republic"
da <- lapply(data, function(x) x[data.set == data_set])
lapply(da, nrow)
season_var = 1:3
das <- prep1(data = da, taxon = "species")
dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist1[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg, type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf, type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss, type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, type = "both", level = "genus to family")
        ))
final_results[[4]] <- mantel_results
# Ebro Hydrographic Confederation ---------------------------------------------------------------------- 
data_set <- "Ebro Hydrographic Confederation"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
season_var = 1:3

#das <- prep1(data = da, taxon = "species")
#dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(daf, function(x) uniqueN(x$gr_sample_id))
daf2 <- prep2(daf)

daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))

## ... for numeric resolution 
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))
## reshape results 
mantel_results <- 
        rbindlist(list(

                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family")

        ))
final_results[[5]] <- mantel_results
# Monitoring data from Croatia ----------------------------------------------------------------------   
data_set <- "Monitoring data from Croatia"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
da <- da[c(1,2)]
season_var = 1:2

das <- prep1(data = da, taxon = "species", seasons = season_var)
dag <- prep1(data = da, taxon = "genus", seasons = season_var)
daf <- prep1(data = da, taxon = "family", seasons = season_var)

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

pre.dist.dag <- list()
pre.dist.daf <- list()

for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist1[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg,     seasons = c("spring", "summer"), type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf,     seasons = c("spring", "summer"), type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf,     seasons = c("spring", "summer"), type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss,     seasons = c("spring", "summer"), type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg,     seasons = c("spring", "summer"), type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff,     seasons = c("spring", "summer"), type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, seasons = c("spring", "summer"), type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, seasons = c("spring", "summer"), type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, seasons = c("spring", "summer"), type = "both", level = "genus to family")
        ))
final_results[[6]] <- mantel_results
# Monitoring data from the Netherlands ----------------------------------------------------------------------
data_set <- "Monitoring data from the Netherlands"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
season_var = 1:3

dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

dag2 <- prep2(dag)
daf2 <- prep2(daf)

## Compute Jaccard distance matrices 
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist1[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.gf, type = "both", level = "genus to family")
        ))

final_results[[7]] <- mantel_results
# Monitoring data from Germany ----------------------------------------------------------------------   
data_set <- "Monitoring data from Germany"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
season_var = 1:3

das <- prep1(data = da, taxon = "species")
dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 8))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 8))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 8))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 8))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 8))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 8))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 8))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 8))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 8))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 8))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))


## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg, type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf, type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss, type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, type = "both", level = "species to family")
                
        ))
final_results[[8]] <- mantel_results

# Monitoring data from Portugal ---------------------------------------------------------------------- 
data_set <- "Monitoring data from Portugal"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
da <- da[c(1,2)]

season_var = 1:2

daf <- prep1(data = da, taxon = "family", seasons = season_var)

daf2 <- prep2(daf)

daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
## Compute Mantel tests ... 
## ... for numeric resolution 
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))


## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family")

        ))
final_results[[9]] <- mantel_results
# Monitoring data from Slovakia ----------------------------------------------------------------------  
data_set <- "Monitoring data from Slovakia"
da <- lapply(data, function(x) x[data.set == data_set])

season_var = 1

lapply(da, nrow)

das <- prep1(data = da, taxon = "species", seasons = season_var)
dag <- prep1(data = da, taxon = "genus"  , seasons = season_var)
daf <- prep1(data = da, taxon = "family" , seasons = season_var)

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist1[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg, type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf, type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss, type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, type = "both", level = "genus to family")
        ))
final_results[[10]] <- mantel_results
# Monitorig data from Spain ----------------------------------------------------------------------         
data_set <- "Monitorig data from Spain"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
season_var = 1:3

daf <- prep1(data = da, taxon = "family")
daf2 <- prep2(daf)
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))


## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family")
        ))
final_results[[11]] <- mantel_results
# Monitoring data from the UK ----------------------------------------------------------------------   
data_set <- "Monitoring data from the UK"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
season_var = 1:3

das <- prep1(data = da, taxon = "species")
dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist2[[x]], daf.dist2[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist2[[x]], dag.dist4[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist2[[x]], daf.dist4[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist4[[x]], daf.dist2[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg, type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf, type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss, type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, type = "both", level = "genus to family")
        ))
final_results[[12]] <- mantel_results
# Monitoring France (Naiades) ----------------------------------------------------------------------
data_set <- "Monitoring France (Naiades)"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
season_var = 1:3

das <- prep1(data = da, taxon = "species")
dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()
pre.dist.daf2 <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}
for (i in season_var){
        pre.dist.daf2[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% dag[[i]]$gr_sample_id), ]
}


## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
daf.dist5 <- lapply(season_var, function(x) parallelDist(pre.dist.daf2[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))
daf.dist6 <- lapply(season_var, function(x) parallelDist(pre.dist.daf2[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist5[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg, type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf, type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss, type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, type = "both", level = "genus to family")
        ))
final_results[[13]] <- mantel_results
# Monitoring France (RCS Network) ----------------------------------------------------------------------  
data_set <- "Monitoring France (RCS Network)"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
season_var = 1:3

das <- prep1(data = da, taxon = "species")
dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist1[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg, type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf, type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss, type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, type = "both", level = "genus to family")
        ))
final_results[[14]] <- mantel_results
# Project STAR ----------------------------------------------------------------------
data_set <- "Project STAR"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
season_var = 1

das <- prep1(data = da, taxon = "species", seasons = season_var)
dag <- prep1(data = da, taxon = "genus"  , seasons = season_var)
daf <- prep1(data = da, taxon = "family" , seasons = season_var)

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}

## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))

## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist1[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg,     season = "spring", type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf,     season = "spring", type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf,     season = "spring", type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss,     season = "spring", type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg,     season = "spring", type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff,     season = "spring", type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, season = "spring", type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, season = "spring", type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, season = "spring", type = "both", level = "genus to family")
        ))
final_results[[15]] <- mantel_results
# Project WISER ----------------------------------------------------------------------      
data_set <- "Project WISER"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
season_var = 1:3

das <- prep1(data = da, taxon = "species")
dag <- prep1(data = da, taxon = "genus")
daf <- prep1(data = da, taxon = "family")

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}
for (i in season_var){
        pre.dist.daf2[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% dag[[i]]$gr_sample_id), ]
}
## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))
daf.dist5 <- lapply(season_var, function(x) parallelDist(pre.dist.daf2[[x]], method = "binary", threads = 4))
## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist5[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg, type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf, type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf, type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss, type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg, type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff, type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, type = "both", level = "genus to family")
        ))
final_results[[16]] <- mantel_results

                              
# Monitoring data from Finland ------------------------------------------------------
data_set <- "Monitoring data from Finland"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
da <- da[c(2,3)]
season_var = 1:2

das <- prep1(data = da, taxon = "species", seasons = season_var)
dag <- prep1(data = da, taxon = "genus"  , seasons = season_var)
daf <- prep1(data = da, taxon = "family" , seasons = season_var)

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}
for (i in season_var){
        pre.dist.daf2[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% dag[[i]]$gr_sample_id), ]
}
## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))
daf.dist5 <- lapply(season_var, function(x) parallelDist(pre.dist.daf2[[x]], method = "binary", threads = 4))
## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist5[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg,     seasons = c("summer", "autumn"), type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf,     seasons = c("summer", "autumn"), type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf,     seasons = c("summer", "autumn"), type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss,     seasons = c("summer", "autumn"), type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg,     seasons = c("summer", "autumn"), type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff,     seasons = c("summer", "autumn"), type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, seasons = c("summer", "autumn"), type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, seasons = c("summer", "autumn"), type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, seasons = c("summer", "autumn"), type = "both", level = "genus to family")
        ))
final_results[[17]] <- mantel_results

# Cantabria -------------------------------------------------------------------------
data_set <- "Cantabria"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
da <- da[c(2)]
season_var = 1

das <- prep1(data = da, taxon = "species", seasons = season_var)
dag <- prep1(data = da, taxon = "genus"  , seasons = season_var)
daf <- prep1(data = da, taxon = "family" , seasons = season_var)

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}
for (i in season_var){
        pre.dist.daf2[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% dag[[i]]$gr_sample_id), ]
}
## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))
daf.dist5 <- lapply(season_var, function(x) parallelDist(pre.dist.daf2[[x]], method = "binary", threads = 4))
## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist5[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg,     seasons = c("summer"), type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf,     seasons = c("summer"), type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf,     seasons = c("summer"), type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss,     seasons = c("summer"), type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg,     seasons = c("summer"), type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff,     seasons = c("summer"), type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, seasons = c("summer"), type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, seasons = c("summer"), type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, seasons = c("summer"), type = "both", level = "genus to family")
        ))
final_results[[18]] <- mantel_results
# Koutajoki -------------------------------------------------------------------------
data_set <- "Koutajoki"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
da <- da[c(3)]
season_var = 1

das <- prep1(data = da, taxon = "species", seasons = season_var)
dag <- prep1(data = da, taxon = "genus"  , seasons = season_var)
daf <- prep1(data = da, taxon = "family" , seasons = season_var)

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}
for (i in season_var){
        pre.dist.daf2[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% dag[[i]]$gr_sample_id), ]
}
## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))
daf.dist5 <- lapply(season_var, function(x) parallelDist(pre.dist.daf2[[x]], method = "binary", threads = 4))
## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist5[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg,     seasons = c("autumn"), type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf,     seasons = c("autumn"), type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf,     seasons = c("autumn"), type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss,     seasons = c("autumn"), type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg,     seasons = c("autumn"), type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff,     seasons = c("autumn"), type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, seasons = c("autumn"), type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, seasons = c("autumn"), type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, seasons = c("autumn"), type = "both", level = "genus to family")
        ))
final_results[[19]] <- mantel_results

# Monitoring data from Norway -------------------------------------------------------
data_set <- "Monitoring data from Norway"
da <- lapply(data, function(x) x[data.set == data_set])

lapply(da, nrow)
da <- da[c(3)]
season_var = 1

das <- prep1(data = da, taxon = "species", seasons = season_var)
dag <- prep1(data = da, taxon = "genus"  , seasons = season_var)
daf <- prep1(data = da, taxon = "family" , seasons = season_var)

## evaluate taxonomic resolution 
sapply(das, function(x) uniqueN(x$gr_sample_id))
sapply(dag, function(x) uniqueN(x$gr_sample_id))
sapply(daf, function(x) uniqueN(x$gr_sample_id))

das2 <- prep2(das)
dag2 <- prep2(dag)
daf2 <- prep2(daf)

#- Mantel for taxonomy -#
## To compare the taxonomic levels, the three distance matrices (species, genus, family)
## must have the same dimensions. This is not the case if some sites did not have species 
## or genus level observations. There for I reduce each data set to the observations 
## included in the smallest data set.

## This are the updated list with reduced numbers of sites 
pre.dist.dag <- list()
pre.dist.daf <- list()

## Loop over seasons. In each season only keep the sites which are also in the respective 
## smallest data set. I use dag to subset dag2 (or other taxonomic level) because dag2
## does not contain the gr_sample_id column anymore
for (i in season_var){
        
        pre.dist.dag[[i]] <- dag2[[i]][which(dag[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
        pre.dist.daf[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% das[[i]]$gr_sample_id), ]
}
for (i in season_var){
        pre.dist.daf2[[i]] <- daf2[[i]][which(daf[[i]]$gr_sample_id %in% dag[[i]]$gr_sample_id), ]
}
## Compute Jaccard distance matrices 
das.dist  <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "binary", threads = 4))
dag.dist1 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "binary", threads = 4))
dag.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "binary", threads = 4))
daf.dist1 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "binary", threads = 4))
daf.dist2 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "binary", threads = 4))
## Compute Bray Curtis distance matrices
das.dist2 <- lapply(season_var, function(x) parallelDist(das2[[x]], method = "bray", threads = 4))
dag.dist3 <- lapply(season_var, function(x) parallelDist(dag2[[x]], method = "bray", threads = 4))
dag.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.dag[[x]], method = "bray", threads = 4))
daf.dist3 <- lapply(season_var, function(x) parallelDist(daf2[[x]], method = "bray", threads = 4))
daf.dist4 <- lapply(season_var, function(x) parallelDist(pre.dist.daf[[x]], method = "bray", threads = 4))
daf.dist5 <- lapply(season_var, function(x) parallelDist(pre.dist.daf2[[x]], method = "binary", threads = 4))
## Compute Mantel tests ... 
## ... for taxonomic level   
mantel.tax.sg <- lapply(season_var, function(x) mantel(das.dist[[x]], dag.dist2[[x]]))
mantel.tax.sf <- lapply(season_var, function(x) mantel(das.dist[[x]], daf.dist2[[x]]))
mantel.tax.gf <- lapply(season_var, function(x) mantel(dag.dist[[x]], daf.dist[[x]]))

## ... for numeric resolution 
mantel.res.ss <- lapply(season_var, function(x) mantel(das.dist[[x]], das.dist2[[x]]))
mantel.res.gg <- lapply(season_var, function(x) mantel(dag.dist1[[x]], dag.dist3[[x]]))
mantel.res.ff <- lapply(season_var, function(x) mantel(daf.dist1[[x]], daf.dist3[[x]]))

## --- for both 
mantel.res_tax.sg <- lapply(season_var, function(x) mantel(das.dist2[[x]], dag.dist2[[x]]))
mantel.res_tax.sf <- lapply(season_var, function(x) mantel(das.dist2[[x]], daf.dist2[[x]]))
mantel.res_tax.gf <- lapply(season_var, function(x) mantel(dag.dist3[[x]], daf.dist5[[x]]))

## reshape results 
mantel_results <- 
        rbindlist(list(
                compile_mantel(mantel.tax.sg,     seasons = c("autumn"), type = "taxonomic level", level = "species to genus"),
                compile_mantel(mantel.tax.sf,     seasons = c("autumn"), type = "taxonomic level", level = "species to family"),
                compile_mantel(mantel.tax.gf,     seasons = c("autumn"), type = "taxonomic level", level = "genus to family"),
                compile_mantel(mantel.res.ss,     seasons = c("autumn"), type = "numerical resolution", level = "species"),
                compile_mantel(mantel.res.gg,     seasons = c("autumn"), type = "numerical resolution", level = "genus"),
                compile_mantel(mantel.res.ff,     seasons = c("autumn"), type = "numerical resolution", level = "family"),
                compile_mantel(mantel.res_tax.sg, seasons = c("autumn"), type = "both", level = "species to genus"),
                compile_mantel(mantel.res_tax.sf, seasons = c("autumn"), type = "both", level = "species to family"),
                compile_mantel(mantel.res_tax.gf, seasons = c("autumn"), type = "both", level = "genus to family")
        ))
final_results[[20]] <- mantel_results


# save to file ----------------------------------------------------------------------
names(final_results[[1]])[1] <- "seasons"
names(final_results[[2]])[1] <- "seasons"
final_results2 <- rbindlist(final_results)
              
saveRDS(final_results2, paste0("data/02_combined_data/", Sys.Date(),"_results_mantel.rds"))                   
