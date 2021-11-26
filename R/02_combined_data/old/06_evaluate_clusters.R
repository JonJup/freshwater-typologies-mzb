### ————————————————————————————————————————————— ###
### ——— Evaluate bio-clusters from typologies ——— ### 
### ————————————————————————————————————————————— ###

# ————————————————
# date:
#       (27,28).07.21
# files in: 
#TODO       ->  WHAT IS IT 09_sxs_genus_typology_with_bio.rds
# files out:
#TODO       
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#TODO       Evaluate clusters
# ————————————————

#TODO TEST 
#TODO Dont Repeat Yourself  
#TODO AUTOMATE 
#TODO DOCUMENT 
#TODO NO BORKEN WINDOWS 
#TODO Design by Contract 
#TODO Fail fast/ defensive programming 

# setup ---------------------------------------------------------------------------------------------------------------------------------------------------
pacman::p_load(
        beepr,
        data.table, 
        dplyr, 
        indicspecies, 
        fpc,
        future.apply, 
        ggplot2,
        magrittr,
        purrr,
        sf,
        stringr,
        tidyr
)

#- functions  —> all for generlized silhouette width 
source("~/my documents/R/functions/call_gensil.R")
source("~/my documents/R/functions/mdist.R")
source("~/my documents/R/functions/genmean.R")
source("~/my documents/R/functions/silgen.R")

# LOAD DATA -------------------------------------------------------------------------
#- distance matrices 
dist1 <- readRDS("data/combined_data/05_2021-07-28_distance_jaccard.rds")
dist2 <- readRDS("data/combined_data/05_2021-07-28_distance_dice.rds")
dist3 <- readRDS("data/combined_data/05_2021-07-28_distance_ochiai.rds")
#- connect gr_sample_id to types 
lookup    <- readRDS("data/combined_data/2021-07-28_lookup_w_bio.rds")
#- connect rows to gr_sample_ids 
subsets   <- readRDS("data/combined_data/04_2021-07-28_subsets.rds")

#- biological typology 
# prepare data  ----------------------------------------------------------------

lookup %<>% unique(by = "gr_sample_id")
subsets2 <- copy(subsets)
lapply(subsets2, function(x) x[,gr_sample_id := NULL])

#- list of classifications - those that are strings are transformed to numbers 
genus = list(
        brt12        = as.numeric(as.factor(left_join(subsets$genus[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12")])$brt12)),
        brt20        = as.numeric(as.factor(left_join(subsets$genus[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt20")])$brt20)),
        illies       = as.numeric(as.factor(left_join(subsets$genus[,"gr_sample_id"], lookup[,c("gr_sample_id", "illies")])$illies)),
        brt12_illies = as.numeric(as.factor(left_join(subsets$genus[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12_illies")])$brt12_illies)),
        bio4         = left_join(subsets$genus[,"gr_sample_id"], lookup[,c("gr_sample_id", "ge.bio.4")])$ge.bio.4,
        bio11        = left_join(subsets$genus[,"gr_sample_id"], lookup[,c("gr_sample_id", "ge.bio.11")])$ge.bio.11,
        data.set     = as.numeric(as.factor(left_join(subsets$genus[,"gr_sample_id"], lookup[,c("gr_sample_id", "data.set")])$data.set ))
)
family = list(
        brt12        = as.numeric(as.factor(left_join(subsets$family[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12")])$brt12)),
        brt20        = as.numeric(as.factor(left_join(subsets$family[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt20")])$brt20)),
        illies       = as.numeric(as.factor(left_join(subsets$family[,"gr_sample_id"], lookup[,c("gr_sample_id", "illies")])$illies)),
        brt12_illies = as.numeric(as.factor(left_join(subsets$family[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12_illies")])$brt12_illies)),
        data.set     = as.numeric(as.factor(left_join(subsets$family[,"gr_sample_id"], lookup[,c("gr_sample_id", "data.set")])$data.set)),
        bio          = left_join(subsets$family[,"gr_sample_id"], lookup[,c("gr_sample_id", "fa.bio.9")])$fa.bio.9

)
genus.german = list(
        brt12        = as.numeric(as.factor(left_join(subsets$genus.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12")])$brt12)),
        brt20        = as.numeric(as.factor(left_join(subsets$genus.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt20")])$brt20)),
        illies       = as.numeric(as.factor(left_join(subsets$genus.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "illies")])$illies)),
        brt12_illies = as.numeric(as.factor(left_join(subsets$genus.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12_illies")])$brt12_illies)),
        german_type  = as.numeric(as.factor(left_join(subsets$genus.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "german_type")])$german_type)),
        data.set     = as.numeric(as.factor(left_join(subsets$genus.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "data.set")])$data.set)),
        bio.3        = left_join(subsets$genus.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "gege.bio.3")])$gege.bio.3,
        bio.6        = left_join(subsets$genus.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "gege.bio.6")])$gege.bio.6
)
family.german = list(
        brt12        = as.numeric(as.factor(left_join(subsets$family.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12")])$brt12)),
        brt20        = as.numeric(as.factor(left_join(subsets$family.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt20")])$brt20)),
        illies       = as.numeric(as.factor(left_join(subsets$family.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "illies")])$illies)),
        brt12_illies = as.numeric(as.factor(left_join(subsets$family.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12_illies")])$brt12_illies)),
        german_type  = as.numeric(as.factor(left_join(subsets$family.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "german_type")])$german_type)),
        data.set     = as.numeric(as.factor(left_join(subsets$family.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "data.set")])$data.set)),
        bio          = left_join(subsets$family.german[,"gr_sample_id"], lookup[,c("gr_sample_id", "fage.bio.4")])$fage.bio.4
)
genus.french = list(
        brt12        = as.numeric(as.factor(left_join(subsets$genus.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12")])$brt12)),
        brt20        = as.numeric(as.factor(left_join(subsets$genus.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt20")])$brt20)),
        illies       = as.numeric(as.factor(left_join(subsets$genus.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "illies")])$illies)),
        brt12_illies = as.numeric(as.factor(left_join(subsets$genus.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12_illies")])$brt12_illies)),
        french_type  = as.numeric(as.factor(left_join(subsets$genus.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "her")])$her)),
        data.set     = as.numeric(as.factor(left_join(subsets$genus.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "data.set")])$data.set)),
        bio          = left_join(subsets$genus.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "gefr.bio.4")])$gefr.bio.4
)
family.french = list(
        brt12        = as.numeric(as.factor(left_join(subsets$family.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12")])$brt12)),
        brt20        = as.numeric(as.factor(left_join(subsets$family.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt20")])$brt20)),
        illies       = as.numeric(as.factor(left_join(subsets$family.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "illies")])$illies)),
        brt12_illies = as.numeric(as.factor(left_join(subsets$family.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "brt12_illies")])$brt12_illies)),
        french_type  = as.numeric(as.factor(left_join(subsets$family.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "her")])$her)),
        data.set     = as.numeric(as.factor(left_join(subsets$family.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "data.set")])$data.set)),
        bio          = left_join(subsets$family.french[,"gr_sample_id"], lookup[,c("gr_sample_id", "fafr.bio.4")])$fafr.bio.4
)

lapply(genus, anyNA)
lapply(family, anyNA)
lapply(genus.german, anyNA)
lapply(family.german, anyNA)
lapply(genus.french, anyNA)
lapply(family.french, anyNA)


# ————————————————————————————— #
# ——— Random Classification ——— # 
# ————————————————————————————— #

#- random number generation seed 
set.seed(123)
#- number of random types 
n.random.type <- 25
# find the maximum number of groups: 
group.sizes.all <- sapply(append(genus,family), uniqueN)
group.sizes.germany <- sapply(append(genus.german, family.german), uniqueN)
group.sizes.france  <- sapply(append(genus.french, family.french), uniqueN)

n.types.all     <- sample(min(group.sizes.all):max(group.sizes.all), n.random.type, replace = TRUE)
n.types.germany <- sample(min(group.sizes.germany):max(group.sizes.germany), n.random.type, replace = TRUE)
n.types.france  <- sample(min(group.sizes.france):max(group.sizes.france), n.random.type, replace = TRUE)


null.types.genus      <- lapply(1:n.random.type, function(x) sample(1:n.types.all[x], size = length(genus$brt12), replace = TRUE))
null.types.family     <- lapply(1:n.random.type, function(x) sample(1:n.types.all[x], size = length(family$brt12), replace = TRUE))
null.types.gege       <- lapply(1:n.random.type, function(x) sample(1:n.types.germany[x], size = length(genus.german$brt12), replace = TRUE))
null.types.fage       <- lapply(1:n.random.type, function(x) sample(1:n.types.germany[x], size = length(family.german$brt12), replace = TRUE))
null.types.gefr       <- lapply(1:n.random.type, function(x) sample(1:n.types.france[x], size = length(genus.french$brt12), replace = TRUE))
null.types.fafr       <- lapply(1:n.random.type, function(x) sample(1:n.types.france[x], size = length(family.french$brt12), replace = TRUE))

names(null.types.genus) <- names(null.types.family) <- names(null.types.gege) <- names(null.types.fage) <-  names(null.types.gefr) <-  names(null.types.fafr) <-  paste0("null", 1:n.random.type)

genus  %<>% append(null.types.genus)
family %<>% append(null.types.family)
genus.german  %<>% append(null.types.gege)
family.german %<>% append(null.types.fage)
genus.french  %<>% append(null.types.gefr)
family.french %<>% append(null.types.fafr)
#- clean up
rm(n.random.type, group.sizes.all,group.sizes.germany,null.types.genus, null.types.family, null.types.gege,null.types.fage,n.types.all, n.types.germany)
gc()

# compute cluster metrics ---------------------------------------------------------------------------------------------------------------------------------



txn.slct  <- rep(c("genus", "family", "genus.german", "family.german", "genus.french", "family.french"), each = 3)
dist.slct <- rep(c("jacc", "dice", "ochi"), times = 6)

eval.fun <- function(x,y,t){
        out <-  data.table(ch = y[[x]]$ch, sw = y[[x]]$avg.silwidth, typology = t)
        return(out)
}


# ——————————————————————————— #
# ——— cluster statistics  ——— # 
# ——————————————————————————— #

for (i in seq_along(txn.slct)){
        
        #- In first round, initiate an object to save the results of the loop. 
        if (i == 1) ce.list <- list()
        
        i.txn = txn.slct[i]
        i.txn.code = switch(i.txn, "genus" = 1, "family" = 2, "genus.german" = 3, "family.german" = 4, "genus.french" = 5, "family.french" = 6)
        i.txn = get(i.txn)
        
        i.dist = dist.slct[i]
        
        
        # if (! i %in% 1:12){
        #         print("i is outside the defined range")
        #         break()
        # }
        
        i.dist <- switch(i.dist,
                         "jacc" = dist1,
                         "dice" = dist2, 
                         "ochi" = dist3
                         )
        
        if (is.null(i.dist)){
                print("the distance matrix is empty")
                break()
        }
        
        #anyNA( i.dist[[i.txn.code]])
        #lapply(i.txn, anyNA)
        #- apply the cluster stats function to each clustering in i.txn using the distance matrix i.dist[[i.txn.code]]
        i.e  <- lapply(1:length(i.txn), function(x) cluster.stats(d= i.dist[[i.txn.code]], clustering = i.txn[[x]]))
        i.e2 <- lapply(1:length(i.e), function(x) eval.fun(x, y = i.e, t = names(i.txn)[x])) |> rbindlist()
        i.e2[, distance := dist.slct[i]]
        i.e2[, data := txn.slct[i]]
        ce.list[[i]] <- i.e2 
        
        if (i == length(txn.slct)) ce.list <- rbindlist(ce.list)
        
        #- clean up
        print(paste(i, Sys.time()))
        rm(i)
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
}

ce.plot <- pivot_longer(ce.list, cols = c("ch", "sw"))
setDT(ce.plot)
ce.plot[str_detect(typology, "null"), typology := "null"]
ce.plot[data == "genus"] |> 
        ggplot(aes(x = typology, y = value)) + 
        geom_point(aes(col = distance)) + 
        facet_wrap(.~name, scale = "free")
ce.plot[data == "family"] |> 
        ggplot(aes(x = typology, y = value)) + 
        geom_point(aes(col = distance)) + 
        facet_wrap(.~name, scale = "free")
ce.plot[data == "genus.german"] |> 
        ggplot(aes(x = typology, y = value)) + 
        geom_point(aes(col = distance)) + 
        facet_wrap(.~name, scale = "free")
ce.plot[data == "family.german"] |> 
        ggplot(aes(x = typology, y = value)) + 
        geom_point(aes(col = distance)) + 
        facet_wrap(.~name, scale = "free")
ce.plot[data == "genus.french"] |> 
        ggplot(aes(x = typology, y = value)) + 
        geom_point(aes(col = distance)) + 
        facet_wrap(.~name, scale = "free")
ce.plot[data == "family.french"] |> 
        ggplot(aes(x = typology, y = value)) + 
        geom_point(aes(col = distance)) + 
        facet_wrap(.~name, scale = "free")


# ———————————————————————— #
# ——— Indicator Value  ——— # 
# ———————————————————————— #

for (i in 1:length(txn.slct)){
        
        #- In first round, initiate an object to save the results of the loop. 
        if (i == 1) indval.list <- list()
        
        #- determine loop variables 
        i.txn  <- txn.slct[i]
        i.txn.n = switch(i.txn, "genus" = 1, "family" = 2, "genus.german" = 3, "family.german" = 4, "genus.french" = 5, "family.french" = 6)
        i.clus.obct <- get(i.txn)
        
        #- compute indval scores 
        i.eval.obj <- lapply(1:length(i.clus.obct), function(x) strassoc(X = subsets2[[i.txn.n]],
                                                                 cluster = i.clus.obct[[x]], 
                                                                 func = "r.ind.g"))        
        i.eval.obj <- lapply(i.eval.obj, function(x) apply(x, 1, function(y) y[which.max(y)]))
        i.eval.obj <- sapply(i.eval.obj, mean)
        i.eval.obj <- data.table(typology = names(i.clus.obct), 
                                 indval   = i.eval.obj)
        i.eval.obj[,`:=` (data = i.txn, 
                          distance = dist.slct[i])]
        
        #- save to storage object 
        indval.list[[i]] <- i.eval.obj
        
        #- In the last round, bind all elements of storage object 
        if (i == length(txn.slct)) indval.list <- rbindlist(indval.list)
        
        #- end 
        print(i)
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
        rm(i)
}


# ——————————————————————————————— #
# ——— Classification Strength ——— # 
# ——————————————————————————————— #

for (i in seq_along(txn.slct)){
        #- In first round, initiate an object to save the results of the loop. 
        if (i == 1) cs.list <- list()
        
        # ———— determine loop variables ———— #
        #- data set 
        i.txn  <- txn.slct[i]
        #- distance metric 
        i.dist <- dist.slct[i]
        #- distance number 
        i.dist.num  <-  ifelse(i.dist == "jacc", 1, ifelse(i.dist == "dice",2, ifelse(i.dist == "ochi",3,stop())))
        #- data set number
        i.taxo.num  <-  switch(i.txn, "genus" = 1, "family" = 2, "genus.german" = 3, "family.german" = 4, "genus.french" = 5, "family.french" = 6)
        #- get clustering for data set 
        i.clus.obct <- get(i.txn)
        #- get distance matrices 
        i.dist2 <- get(paste0("dist",i.dist.num))
        #- extract focal distance matrix 
        i.dist2 <- i.dist2[[i.taxo.num]] 
        #- transform to matrix 
        i.dist2 <- as.matrix(i.dist2)
        
        # ———— compute cluster strength ———— #
        for (j in seq_along(i.clus.obct)){
                #- initialize storage object (i.csj) for ... 
                if (j == 1) i.csj <- c()
                #- extract types from clusterings 
                j.classes <- i.clus.obct[[j]]
                #- reduce to unique types 
                j.classes.u <- unique(j.classes)
                
                #- for every type: how similar are observations within type compared to between types 
                for (k in seq_along(j.classes.u)) {
                        
                        if (k == 1) j.csk <- c()
                        k.id1    <- which(j.classes == j.classes.u[k])
                        k.id.n1  <- which(j.classes != j.classes.u[k])
                        k.sim1   <- i.dist2[k.id1, k.id1]
                        k.sim.n1 <- i.dist2[k.id1, k.id.n1]
                        k.ut     <- k.sim1[upper.tri(k.sim1)]
                        k.lt     <- k.sim1[lower.tri(k.sim1)]
                        k.ut.n   <- k.sim.n1[upper.tri(k.sim.n1)]
                        k.lt.n   <- k.sim.n1[lower.tri(k.sim.n1)]
                        j.csk[k] <- mean(append(k.ut, k.lt)) - mean(append(k.ut.n, k.lt.n))
                        rm(list = ls()[grepl(x = ls(), pattern = "^k\\.")])
                        rm(k)
                        
                }
                i.csj[j] <- mean(j.csk)
                rm(list = ls()[grepl(x = ls(), pattern = "^j\\.")])
                rm(j)
        }
        
        # ———— reshape and store results  ———— #
        i.cs.eval <- data.table(cs = i.csj, 
                              typology = names(i.clus.obct), 
                              distance = i.dist,
                              data = i.txn)
        cs.list[[i]] <- i.cs.eval
        
        
        #- In the last round, bind all elements of storage object 
        if (i == length(txn.slct)) cs.list <- rbindlist(cs.list)
        
        #- end 
        print(i)
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
        rm(i)
        gc()
}

# ———————————————————————————————————— #
# ——— Generalized Silhouette Width ——— # 
# ———————————————————————————————————— #


for (i in seq_along(txn.slct)){
        
        # temp
        if(i %in% 1:12) next()
        
        #- In first round, initiate an object to save the results of the loop. 
        if (i == 1) gs.list <- list()
        
        #- determine loop variables 
        i.txn  <- txn.slct[i]
        i.dist <- dist.slct[i]
        if (!i.txn %in% c("genus", "family", "genus.german", "family.german", "genus.french", "family.french")){
                print("The correct data set could not be identified")
                break()
        }
        if (!i.dist %in% c("jacc", "dice", "ochi")){
                print("The correct distance metric could not be identified.")
                break()        
        }
        i.dist.num  <-  ifelse(i.dist == "jacc", 1, ifelse(i.dist == "dice",2, ifelse(i.dist == "ochi",3,stop())))
        i.taxo.num  <-  switch(i.txn, "genus" = 1, "family" = 2, "genus.german" = 3, "family.german" = 4, "genus.french" = 5, "family.french" = 6)
        i.clus.obct <- get(i.txn)
        i.loop.dist <- get(paste0("dist",i.dist.num))
        i.loop.dist <- i.loop.dist[[i.taxo.num]] 
        
        #- compute generalized silhouette widths 
        i.gs <- lapply(X = 1:length(i.clus.obct), FUN = function(X) call_gensil(type = i.clus.obct[[X]], distance = i.loop.dist))

        #- reshape results 
        i.gs %<>% rbindlist()
        i.gs[, typology := rep(names(i.clus.obct), each = 2)]
        i.gs %<>% mutate(p = case_when(p == -Inf ~ "min",
                                      p == 1 ~ "arithmetic",
                                      p == Inf ~ "max"))  %>% 
                pivot_wider(id_cols = typology, names_from = p, values_from = silhouette) 
        setDT(i.gs)
        i.gs[, `:=` (data = i.txn, distance = i.dist)]
        
        #- save results to storage object 
        gs.list[[i]] <- i.gs
        
        #- In the last round, bind all elements of storage object 
        if (i == length(txn.slct)) gs.list <- rbindlist(gs.list)
        
        #- clean up 
        print(paste(i, Sys.time()))
        rm(list = ls()[grepl(x = ls(), pattern = "î\\.")])
        rm(i)
}



# ———————————————————— #
# ——— Combine Data ——— # 
# ———————————————————— #
combine <- gs.list[cs.list, on = c("typology", "data", "distance")]
combine <- indval.list[combine, on =  c("typology", "data", "distance")]

duplicated(indval.list[, c("typology", "data", "distance")])

combine <- ce.list[combine, on =  c("typology", "data", "distance")]

combine[str_detect(typology , "null"), typology  := "null"]
combine2 <- pivot_longer(combine, !c("typology", "distance", "data"))
setDT(combine2)

combine2[data == "genus"] |> 
ggplot(aes(x = typology, y = value, col = distance)) + geom_point() + facet_wrap(.~name, scale = "free")
combine2[data == "family"] |> 
ggplot(aes(x = typology, y = value, col = distance)) + geom_point() + facet_wrap(.~name, scale = "free")
combine2[data == "genus.german"] |> 
ggplot(aes(x = typology, y = value, col = distance)) + geom_point() + facet_wrap(.~name, scale = "free")
combine2[data == "genus"] |> 
ggplot(aes(x = typology, y = value, col = distance)) + geom_point() + facet_wrap(.~name, scale = "free")


# save to file ----------------------------------------------------------------------
saveRDS(join1, "data/10_class_eval_mzb.rds")




