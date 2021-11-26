# - f helper function


library(viridis)



#  ————— OPTIONS —————— # 
plot.alpha = 0.9


setNAcols <- function(x){
        #- How many columns are in x that are not in append("gr_sample_id", typologies)
        x1 <- length(setdiff(names(x), append("gr_sample_id", typologies)))
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

prep1 <- function(data, taxon, abundance.to.pa = TRUE) {
        
        if("list" %in% class(data)) {
                id.da <-  1:length(data)
        } else {
                id.da <- 1
        }
                           
        
        #- Character vector with column names to keep
        col.vec <- append(c("gr_sample_id", taxon, "abundance"), typologies)
        #- Subset data to columns from col.vec
        x1 <- lapply(id.da, function(x) data[[x]][, .SD, .SDcols = col.vec])
        #- remove any NA in taxon column 
        if ("species" %in% names(x1[[1]])) x1 <- lapply(id.da, function(t) x1[[t]][!is.na(species)])
        if ("genus"   %in% names(x1[[1]])) x1 <- lapply(id.da, function(t) x1[[t]][!is.na(genus)])
        if ("family"  %in% names(x1[[1]])) x1 <- lapply(id.da, function(t) x1[[t]][!is.na(family)])
        if (abundance.to.pa){
                #- Set all abundance to 1.
                x1 <- lapply(id.da, function(x) data[[x]][, abundance := 1])
        }
        #- Make sure that each taxon only occurs once per sample. This might be violated
        #- when a coarse taxonomic resolution is used and several taxa from this group were
        #- observed.
        if ("species" %in% names(x1[[1]])) x1 <- lapply(id.da, function(x) x1[[x]][, abundance := sum(abundance),by = c("gr_sample_id", "species")])
        if ("genus"   %in% names(x1[[1]])) x1 <- lapply(id.da, function(x) x1[[x]][, abundance := sum(abundance),by = c("gr_sample_id", "genus")])
        if ("family"  %in% names(x1[[1]])) x1 <- lapply(id.da, function(x) x1[[x]][, abundance := sum(abundance),by = c("gr_sample_id", "family")])
        x1 <- lapply(id.da, function(x) unique(x1[[x]], by = c("gr_sample_id", taxon)))
        #- Drop rows where the taxonomic level is coarser then what is required by the
        #- taxon argument.
        x1 <- lapply(id.da, function(x) x1[[x]][!is.na(get(taxon))]) 
        #- Turn to wide format with one column for each taxon and one row per site. 
        x1 <- lapply(x1, pivot_wider, id_cols = append(c("gr_sample_id"), typologies), 
                     names_from = all_of(taxon), 
                     values_from = abundance, 
                     values_fill = 0)
        #- Turn to data.table. 
        x1 <- lapply(x1, setDT)
        #- Turn NAs to 0 i.e. absence.  
        x2 <- lapply(x1, setNAcols)
        x1 <- lapply(id.da, function(z) setnafill(x = x1[[z]], type = "const", cols = x2[[z]], fill = 0))
        #- remove columns of taxa that are always absent.
        rm.col <- lapply(id.da, function(x) names(which(colSums(x1[[x]][,x2[[x]],with=F]) == 0))) 
        for (i in seq_along(id.da)){
                if(length(rm.col[[i]]) > 0){
                        x1[[i]][, (rm.col[[i]]) := NULL]
                }
        }
        x2 <- lapply(x1, setNAcols)
        #- Remove sites without taxa 
        rm.rows <- lapply(id.da, function(x) which(rowSums(x1[[x]][,x2[[x]],with=F]) == 0))
        for (i in id.da){
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
                                select(x[[y]], !append(c("gr_sample_id"), 
                                                       typologies)
                                )
                        )
        )
}


prep3 <- function(x) lapply(x, vegdist,method = "bray")

PMI <- function(a, b, c, dropReq8 = TRUE) {
        stopifnot(all(c(a, b, c) >= 0)) # Negative values are invalid. 
        if (!dropReq8) stopifnot((a + b >0) && (a + c > 0)) # Stop if both lists show no positive entry 
        if (a == 0) return(0) # No positive match occurs. 
        # Trivial case can be evaluated # if Requirement 8 would be dropped out.
        if (b == c) return(a/(a + b)) 
        # Equation (1). 
        return(a/abs(b, c)* log((a + max(b, c))/(a + min(b, c)))) # Equation (2). 
        }

prep3.parallel <- function(x) lapply(x, parallelDist, method = "bray")


ivs <- function(data2, data3, add = NULL, season.data, typologies) {
        
        seasons <- season.data
        
        for (i in seq_along(typologies)) {
                #- In first round, initiate an object to save the results of the loop.
                if (i == 1)
                        indval.list <- list()
                
                # #- determine loop variables
                i.data <-
                        lapply(data2, function(x)
                                x[[typologies[i]]])
                
                lapply(i.data, table)
                
                #- compute indval scores
                i.eval.obj <- lapply(seasons,
                                     function(x)
                                             strassoc(
                                                     X = data3[[x]],
                                                     cluster = i.data[[x]],
                                                     func = "r.ind.g"
                                             ))
                i.eval.obj %<>%
                        lapply(function(x)
                                apply(x, 1, function(y)
                                        y[which.max(y)])) %>%
                        lapply(unlist) %>%
                        sapply(mean, na.rm = T)
                
                i.eval.obj <-
                        data.table(
                                season   = all_seasons[seasons],
                                typology = typologies[i],
                                indval   = i.eval.obj
                        )
                if(!is.null(add)) i.eval.obj$type = add
                
                #- save to storage object
                indval.list[[i]] <- i.eval.obj
                
                #- In the last round, bind all elements of storage object
                if (i == length(typologies))
                        indval.list <- rbindlist(indval.list)
                
                #- end
                rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
                rm(i)
        }
        indval.list
}
#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 
ivs.plot1 <- function(x, taxon){
        p <- 
                ggplot(data = x, 
                       aes(x = type, 
                           y = indval, 
                           fill = season, 
                           group = typology)
                ) + 
                geom_point(
                        size = 6, 
                        pch = 23, 
                        col = "white", 
                        alpha = plot.alpha
                ) + 
                stat_summary(
                        geom = "point", 
                        fun = mean, 
                        pch = 151,
                        size = 7,
                        fill = "black") + 
                theme_bw() + 
                theme(axis.text.x = element_text(size = 6)) + 
                #ylim(0,1.2) + 
                facet_grid(.~typology) + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                ggtitle(paste(data.set,"-", taxon))
        p
}
#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 
ivs.plot2 <- function(x, taxon){
        
        seasons <- get("out1", envir = globalenv())
        seasons <- seasons$seasons
        x$season %<>% factor(levels = all_seasons[seasons])
        
        p <- ggplot(
                data = x, 
                aes(
                        x = season, 
                        y = indval, 
                        fill = typology, 
                        group = typology)
        ) + 
                geom_line(aes(col = typology)) + 
                geom_jitter(
                        size = 5,
                        pch = 21,
                        col = "white",
                        alpha = plot.alpha,
                        width = 0.01,
                        height = 0.01
                ) + 
                theme_bw() +
                theme(axis.text.x = element_text(size = 7))+ 
                #ylim(0, 1.2) +
                facet_wrap(. ~ type) + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                ggtitle(paste(data.set,"-", taxon))
        
        p 
}

#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 

ivs.plot3 <- function(x, taxon){
        
        p <- ggplot(data = x, 
                    aes(x = typology, y = indval, fill = season, group = typology)) + 
                geom_jitter(
                        size = 4, 
                        pch = 21, 
                        col = "white",
                        width = 0.1, 
                        alpha = plot.alpha) + 
                stat_summary(geom = "point", fun = mean, pch = 151, size = 7, fill = "black") + 
                theme_bw() + 
                theme(axis.text.x = element_text(size = 6)) +
                #ylim(0,1.2) + 
                facet_grid(.~type) + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") +
                ggtitle(paste(data.set,"-", taxon))
        p
}
class.strength <- function(data2, data4, add=NULL, season.data){
        
        seasons <- season.data
        
        for (i in seq_along(typologies)){
                
                #- In first round, initiate an object to save the results of the loop. 
                if (i == 1) cs.list <- list()
                
                # ———— determine loop variables ———— #
                #- data set 
                i.data <- lapply(data2, function(x) x[[typologies[i]]])
                
                # ———— compute cluster strength ———— #
                # ———> loop over seasons   
                for (j in seasons){
                        #- initialize storage object (i.csj) for ... 
                        if (j == seasons[1]) i.csj <- c()
                        #- extract types from clusterings 
                        j.classes <- i.data[[j]]
                        #- reduce to unique types 
                        j.classes.u <- unique(j.classes)
                        #- distance metric 
                        j.dist <- data4[[j]]
                        #- transform to matrix 
                        j.dist2 <- as.matrix(j.dist)
                        j.dist2 <- 1 - j.dist2
                        # ———> for every type: how similar are observations within type compared to between types 
                        for (k in seq_along(j.classes.u)) {
                                
                                if (k == 1) j.csk <- c()
                                k.id1    <- which(j.classes == j.classes.u[k])
                                k.id.n1  <- which(j.classes != j.classes.u[k])
                                k.sim1   <- j.dist2[k.id1, k.id1]
                                k.sim.n1 <- j.dist2[k.id1, k.id.n1]
                                k.ut     <- k.sim1[upper.tri(k.sim1)]
                                k.lt     <- k.sim1[lower.tri(k.sim1)]
                                k.ut.n   <- k.sim.n1[upper.tri(k.sim.n1)]
                                k.lt.n   <- k.sim.n1[lower.tri(k.sim.n1)]
                                j.csk[k] <- mean(append(k.ut, k.lt), na.rm = T) - mean(append(k.ut.n, k.lt.n), na.rm = T)
                                rm(list = ls()[grepl(x = ls(), pattern = "^k\\.")])
                                rm(k)
                                
                        }
                        i.csj[j] <- mean(j.csk, na.rm = T)
                        rm(list = ls()[grepl(x = ls(), pattern = "^j\\.")])
                        rm(j)
                }
                
                # ———— reshape and store results  ———— #
                i.cs.eval <- data.table(cs = i.csj, 
                                        typology = typologies[i], 
                                        season = all_seasons[seasons])
                if(!is.null(add)) i.cs.eval$type = add
                cs.list[[i]] <- i.cs.eval
                
                
                #- In the last round, bind all elements of storage object 
                if (i == length(typologies)) cs.list <- rbindlist(cs.list)
                
                #- end 
                #print(i)
                rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
                rm(i)
                gc()
        }
        cs.list
}
class.strength.plot1 <- function(x,taxon){
        p <- ggplot(
                data = x, 
                aes(
                        x = typology, 
                        y = cs, 
                        fill = season, 
                        group = typology)
        ) + 
                stat_summary(
                        geom = "point",
                        fun = mean, 
                        pch = 151, 
                        size = 7, 
                        fill = "black",
                        alpha = plot.alpha
                ) + 
                geom_point(
                        size = 6, 
                        pch = 23, 
                        col = "white", 
                        alpha = plot.alpha
                ) + 
                theme_bw() + 
                theme(legend.position = "bottom", 
                      axis.text.x = element_text(size = 8))+ 
                #ylim(0,.33) + 
                facet_wrap(.~type) + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                ggtitle(paste(data.set, "-", taxon))
        p 
}


class.strength.plot2 <- function(x, taxon){
        
        x$season %<>% factor(levels = all_seasons[seasons])
        
        p <- 
                ggplot(data = x,
                       aes(
                               x = season,
                               y = cs,
                               fill = typology,
                               group = typology
                       )) +
                geom_line(aes(col = typology)) +
                geom_point(
                        size = 6, 
                        pch = 23, 
                        col = "white", 
                        alpha = plot.alpha
                ) + 
                theme_bw() +
                #ylim(0, .33) +
                facet_wrap(. ~ type) +
                scale_color_viridis_d(option = "cividis") +
                scale_fill_viridis_d(option = "cividis") +
                ggtitle(paste(data.set, "-", taxon))
        p 
}

class.strength.plot3 <- function(x, taxon){
        
        p <-
                ggplot(data = x,
                       aes(
                               x = type,
                               y = cs,
                               fill = season,
                               group = typology
                       )) +
                stat_summary(
                        geom = "point",
                        fun = mean,
                        pch = 151,
                        size = 7,
                        fill = "black",
                        alpha = 0.5
                ) +
                geom_point(
                        size = 6, 
                        pch = 23, 
                        col = "white", 
                        alpha = plot.alpha
                ) + 
                theme_bw() +
                # ylim(0, .33) +
                facet_wrap(. ~ typology) +
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                ggtitle(paste(data.set, "-", taxon))
        p 
}
wraper_silhouette <- function(data2, distance, add = NULL){
        
        # —— Errors —— 
        if (! "typologies" %in% ls(name = .GlobalEnv)) stop("typologies is not in global environment")
        if (! "seasons" %in% ls(name = .GlobalEnv))    stop("seasons is not in global environment")
        
        #- loop over seasons (object must exist in global environment)
        for (i in seasons) {
                #- setup storage in first iteration 
                if (i == 1) sil.ls <- list()
                
                #- define objects for loop over typologies (k)
                i.distance   <- distance[[i]]
                i.season     <- all_seasons[seasons[i]]
                #- which observations belongs to which group? All typologies 
                i.grouping   <- data2[[i]]
                #- subset i.grouping to tested typologies 
                #- typologies must be available as object in the global environment 
                i.typologies <- i.grouping[, (typologies), with = F]
                
                #- loop over typologies 
                for (k in seq_along(i.typologies)){
                        
                        #- setup storage in first iteration 
                        if (k == 1) i.ls <- list()
                        k.typ <- i.typologies[[k]]
                        k.dictionary <- data.table(v = k.typ,
                                                   cluster = as.numeric(as.factor(k.typ)))
                        
                        k.sil <- silhouette(x = k.dictionary$cluster, 
                                            dist = i.distance)
                        k.sil2 <- as.data.table(as.data.frame(as.matrix(k.sil)[1:nrow(k.sil), 1:ncol(k.sil)]))
                        k.sil2[, typology :=  names(i.typologies)[k]]
                        k.sil2[, typ      := add]
                        k.sil2[, season   := i.season]
                        
                        k.dictionary <- unique(k.dictionary, by = "v")
                        k.neighbor <- data.table(v = k.typ,
                                                 neighbor = as.numeric(as.factor(k.typ)))
                        k.neighbor <- unique(k.neighbor, by = "v")
                        names(k.dictionary)[1] <- "true_type"
                        names(k.neighbor)[1] <- "neighbor_type"
                        k.sil2 <- k.dictionary[k.sil2, on = "cluster"]
                        k.sil2 <- k.neighbor[k.sil2, on = "neighbor"]
                        k.sil2[, c("neighbor", "cluster") := NULL]
                        i.ls[[k]] <- k.sil2 
                        if (k == ncol(i.typologies)) i.ls.b <- rbindlist(i.ls)
                        
                        rm(list = ls()[grepl(pattern = "^k\\.", x = ls())])
                }# END LOOP k 
                
                sil.ls[[i]] <- i.ls.b
                rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                
        }# END LOOP i 
        sil.out <- rbindlist(sil.ls)
        sil.out
}
silhouette.plot1 <- function(x, taxon = "family"){
        
        p <- ggplot(x, aes(x = true_type, y = sil_width)) + 
                geom_hline(yintercept = 0, lty = 2) +
                #geom_violin(aes(fill = season), draw_quantiles = 0.5) + 
                stat_summary(geom = "point", fun = mean, aes(fill = season), pch = 21, size = 4) + 
                stat_summary(fun = mean, aes(x = 1, yintercept = ..y.., group = 1), geom = "hline") + 
                facet_grid(typology~typ, scales = "free") + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                theme_bw() + 
                theme(axis.text.x = element_text(size = 6))+ 
                ggtitle(paste(data.set,"-", taxon)) + 
                ylab("Mean Silhouette width") + xlab("")
        p
}
wraper_anosim <- function(distance, grouping, parallel = 1, permutations = 999, add = NULL){
        
        
        # —— Errors —— 
        if (! "typologies" %in% ls(name = .GlobalEnv)) stop("typologies is not in global environment")
        if (! "seasons" %in% ls(name = .GlobalEnv))    stop("seasons is not in global environment")
        
        #- initialize with list to store the outputs of the for loop 
        out.ls <- list()
        
        #- loop over seasons 
        for (i in seq_along(seasons)){
                i.distance <- distance[[i]]
                i.grouping <- grouping[[i]]
                i.season   <- all_seasons[seasons[i]]
                #- create subset of spe2 with only typology variables 
                setDT(i.grouping)
                i.anosim.typologies <- i.grouping[, (typologies), with = F]
                
                #apply(i.anosim.typologies,2,uniqueN)
                
                #- for each typologies compute ANOSIM
                i.out <- lapply(seq_along(i.anosim.typologies),
                                function(k)
                                        anosim(
                                                x = i.distance,
                                                # distance matrix
                                                grouping = i.anosim.typologies[[k]],
                                                # Factor for grouping observations
                                                permutations = permutations,
                                                # number of permutations
                                                parallel = parallel  # Number of parallel processes
                                        ))
                
                #- extract R statistic and p-value 
                i.out2 <- 
                        i.out |> 
                        lapply(function(x) data.table(R = x$statistic, p.value = x$signif)) |> 
                        rbindlist()
                i.out2$typology <- names(i.anosim.typologies)
                if (!is.null(add)) i.out2$type = add 
                i.out2$season <- i.season
                out.ls[[i]] <- i.out2
                #- clean up 
                rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                gc()
        }# END OF LOOP i over seasons 
        out <- rbindlist(out.ls)
        out
}
anosim.plot1 <- function(x, taxon){
        
        x[, significant := p.value <= 0.05]
        
        p <- 
                ggplot(data = x, 
                       aes(x = type, 
                           y = R, 
                           col = season, 
                           group = typology)
                ) + 
                geom_jitter(
                        size = 4, 
                        aes(shape = significant), 
                        #col = "white", 
                        alpha = plot.alpha,
                        width = 0.1
                ) + 
                stat_summary(
                        geom = "point", 
                        fun = mean, 
                        pch = 151,
                        size = 7,
                        fill = "black") + 
                theme_bw() + 
                theme(axis.text.x = element_text(size = 6)) + 
                #ylim(0,1.2) + 
                facet_grid(.~typology) + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                ggtitle(paste(data.set,"-", taxon))
        p
}

indicator_analysis <- function(data3, data2, B.lvl, add){
        
        # —— Errors —— 
        if (! "typologies" %in% ls(name = .GlobalEnv)) stop("typologies is not in global environment")
        if (! "seasons" %in% ls(name = .GlobalEnv)   ) stop("seasons is not in global environment")
        if (missing (B.lvl)                          ) stop("You must supply a value to B.lvl")
        if (missing (add)                            ) stop("You must supply a value to add")
        
        
        out.ls <- vector(mode = "list", length = length(seasons)+1)
        names(out.ls) <- append(c("spring", "summer", "autumn")[seasons], "add")
        out.ls$add = add
        #- loop over seasons 
        for (seas in seasons){
                s.out <- vector(mode = "list", length = length(typologies))
                names(s.out) <- typologies
                for (typo in typologies){
                        
                        t.out <- list()
                        #- multipatt analysis 
                        t.multpatt.out <- multipatt(x       = data3[[seas]], 
                                          cluster = data2[[seas]][[typo]], 
                                          duleg = TRUE, 
                                          func = "IndVal.g")
                        #- save to object 
                        t.out$multipatt <- t.multpatt.out
                        # ———— TYPICAL TAXA ———— 
                        #- extract B object from multipatt 
                        t.B <- as.data.frame(t.multpatt.out$B)
                        #- subset to typical taxa (i.e. B > B.lvl argument)
                        #- and extract taxon names 
                        t.B.list <- 
                                lapply(t.B, function(x) which(x >= B.lvl)) |> 
                                lapply(function(x) rownames(t.B)[x]) |>  
                                lapply(sort)
                        #- reshape to data.table and bind 
                        t.B.table <- 
                                lapply(seq_along(t.B.list), 
                                       function(x) data.table(taxon = t.B.list[[x]], 
                                                              type  = names(t.B.list)[x])) |> 
                                rbindlist()
                        t.out$typical_taxa <- t.B.table
                        
                        #- how many types and their names 
                        t.n.types <- uniqueN(t.B.table$type)
                        t.f.types <- unique(t.B.table$type)
                        
                        #- how often do taxa occur
                        # t.B.table2 <- t.B.table$taxon |> table() |> data.table()
                        # t.B.table2[, percent := N/n.types * 100]
                        # t.B.table2 %<>% rename(taxon = V1)
                        # t.B.table <- t.B.table2[t.B.table, on = "taxon"]
                        # t.B.table[, mean(percent), by = "type"]
                        
                        #- Jaccard Similarity between typical communities 
                        t.simi_typical <- matrix(0, t.n.types, t.n.types)
                        diag(t.simi_typical) <- 666
                        rownames(t.simi_typical) <- t.f.types
                        colnames(t.simi_typical) <- t.f.types
                        
                        for (i in 1:t.n.types){
                                
                                i.names <- t.B.table[type == t.f.types[i], taxon]
                                
                                for (k in 1:t.n.types){
                                        
                                        if(i == k) next()
                                        
                                        k.names <- t.B.table[type == t.f.types[k], taxon]
                                        k.difference <- length(intersect(i.names, k.names))
                                        k.union      <- length(union(i.names, k.names))
                                        t.simi_typical[i,k] <- k.difference/k.union
                                        #- clean up k 
                                        rm(list = ls()[grepl(pattern = "^k//.", ls())])
                                }
                                #- clean up i 
                                rm(list = ls()[grepl(pattern = "^i//.", ls())])
                        }
                        
                        t.out$jaccard <- t.simi_typical
                        
                        #- indicator taxa 
                        t.iv <- t.multpatt.out$sign
                        setDT(t.iv)
                        t.iv[, taxon := rownames(t.multpatt.out$B)]
                        for (i in 1:t.n.types){
                                if (i == 1){
                                        t.mean_iv <- c()
                                        t.mean_iv_sig <- c()
                                        t.indicator_taxa <- list()
                                }
                                t.data                <- t.iv[index == i]
                                t.mean_iv[i]          <- t.data[, mean(stat)]
                                t.mean_iv_sig[i]      <-  t.data[p.value <= 0.05, mean(stat)]
                                t.indicator_taxa[[i]] <- t.data[p.value <= 0.05, taxon]
                        }
                        t.out$mean_iv <- t.mean_iv
                        t.out$t.mean_iv_sig <- t.mean_iv_sig
                        t.out$t.indicator_taxa <- t.indicator_taxa
                        
                        s.out[[typo]] <- t.out 
                        #- clean up t 
                        rm(list = ls()[grepl(pattern = "^t//.", ls())])
                }
                out.ls[[seas]] <- s.out
                #- clean up s 
                rm(list = ls()[grepl("^s\\.", ls())])
                gc()
        }
        #- return output 
        out.ls
}
jaccard_extract <- function(x){
        
        #- create storage
        storage <- list()
        
        add <- x$add
        add.id <- which(names(x) == "add")
        x <- x[-add.id]
        #- how many seasons ? 
        n.seasons <- length(x)
        #- loop over seasons
        for (i in 1:n.seasons){
                
                i.n.typo <- length(x[[i]])
                
                for (k in 1:i.n.typo){
                        
                        k.simi <- x[[i]][[k]]$jaccard |>
                                c() |>
                                unique()
                        
                        k.dt <- data.table( similarity = k.simi, season = names(x)[i], typology = names(x[[i]])[k])
                        k.dt <- k.dt[similarity != 666]
                        
                        storage[[length(storage) + 1]] <- k.dt
                        
                        rm(list = ls()[grepl("^k\\.", ls())])
                }# END loop k over typologies 
                
                rm(list = ls()[grepl("^i\\.", ls())])
        }# END loop i over seasons 
        
        res <- rbindlist(storage)
        res$type = add
        res
}

jaccard_plot <- function(x, taxon){
        x[, season := factor(season, levels = c("spring", "summer", "autumn", "winter"))]
        out <- ggplot(x, aes(x = typology, y = similarity)) + 
                geom_violin(aes(fill = season), alpha = 0.5, draw_quantiles = 0.5) + 
                geom_point(aes(fill = season), pch = 21) + 
                facet_grid(season~type) + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                ggtitle(paste(data.set,"-", taxon)) + 
                ylim(0,1)
        out
}

mid_extract <- function(x){
        
        #- create storage
        storage <- list()
        
        add <- x$add
        add.id <- which(names(x) == "add")
        x <- x[-add.id]
        #- how many seasons ? 
        n.seasons <- length(x)
        #- loop over seasons
        for (i in 1:n.seasons){
                
                i.n.typo <- length(x[[i]])
                
                for (k in 1:i.n.typo){
                        
                        k.simi <- x[[i]][[k]]$mean_iv |>
                                c() |>
                                unique()
                        
                        k.dt <- data.table(mean_indicator = k.simi, season = names(x)[i], typology = names(x[[i]])[k])
                        storage[[length(storage) + 1]] <- k.dt
                        
                        rm(list = ls()[grepl("^k\\.", ls())])
                }# END loop k over typologies 
                
                rm(list = ls()[grepl("^i\\.", ls())])
        }# END loop i over seasons 
        
        res <- rbindlist(storage)
        res$type = add
        res
}

mid_plot <- function(x, taxon){
        x[, season := factor(season, levels = c("spring", "summer", "autumn", "winter"))]
        out <- ggplot(x, aes(x = typology, y = mean_indicator)) + 
                geom_violin(aes(fill = season), alpha = 0.5, draw_quantiles = 0.5) + 
                geom_point(aes(fill = season), pch = 21) + 
                facet_grid(season~type) + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                ggtitle(paste(data.set,"-", taxon))
                #ylim(0,1)
        out
}

add.beta.clust <- function(x) {
        f.data.set     <- substr(x,1,2)
        f.taxon.lvl    <- substr(x,3,3)
        f.distance <- get(paste0(x, ".dist"))
        typologies_new <- paste0(typologies, "_", "beta")
        f.data <- get(paste0(x, "2"))
        f.n.season <- length(f.data)
        types <- rbindlist(lapply(1:f.n.season, function(x) f.data[[x]][, lapply(.SD, uniqueN), .SDcols = typologies]))
        for (i in seq_along(typologies_new)) lapply(1:f.n.season, function(x) f.data[[x]][,typologies_new[i] := 0])
        for (i in f.n.season){
                f.beta <- agnes(f.distance[[i]], method = "flexible", par.method = 0.625)
                for (g in 1:length(typologies_new)){
                        g.type <- typologies_new[g]
                        f.data[[i]][[g.type]] <- cutree(tree = f.beta, k = types[i,g,with = F])
                        f.data[[i]][[g.type]] <- cutree(tree = f.beta, k = types[i,g,with = F])    
                }

        }
        f.data
}

