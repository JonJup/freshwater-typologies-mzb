### ———————————————————————————————————— ###
### ——— individual data sets helpter ——— ### 
### ———————————————————————————————————— ###

library(viridis)



#  ————— OPTIONS —————— # 
plot.alpha = 0.9


#. Function to return column range of taxa columns.     
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

#. prep1 
#  —— reshape to sites x taxon table. Using the taxonomic level provided to the taxon argument. 

prep1 <- function(data, taxon, seasons) {
        
        #- Character vector with column names to keep
        col.vec <- append(c("gr_sample_id", taxon, "abundance"), typologies)
        #- Subset data to columns from col.vec
        x1 <- lapply(seasons, function(x) data[[x]][, .SD, .SDcols = col.vec])
        #- Drop rows where the taxonomic level is coarser then what is required by the
        #- taxon argument.
        
        #- Make sure that each taxon only occurs once per sample. This might be violated
        #- when a coarse taxonomic resolution is used and several taxa from this group were
        #- observed.
        if (taxon == "species"){
                x1 <- lapply(seasons, function(x) x1[[x]][!is.na(species)]) 
                x1 <- lapply(seasons, function(x) x1[[x]][, abundance := sum(abundance), by = c("species", "gr_sample_id")])
        } else if (taxon == "genus") {
                x1 <- lapply(seasons, function(x) x1[[x]][!is.na(genus)]) 
                x1 <- lapply(seasons, function(x) x1[[x]][, abundance := sum(abundance), by = c("genus", "gr_sample_id")])
        } else if (taxon == "family"){
                x1 <- lapply(seasons, function(x) x1[[x]][!is.na(family)]) 
                x1 <- lapply(seasons, function(x) x1[[x]][, abundance := sum(abundance), by = c("family", "gr_sample_id")])
        }
       
        x1 <- lapply(seasons, function(x) unique(x1[[x]], by = c("gr_sample_id", taxon)))

        #- Turn to wide format with one column for each taxon and one row per site. 
        x1 <- lapply(x1, pivot_wider, id_cols = append(c("gr_sample_id"), typologies), names_from = all_of(taxon), values_from = abundance, values_fill = 0)
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
                                select(x[[y]], !append(c("gr_sample_id"), 
                                                       typologies)
                                       )
                                )
                )
        }


prep3 <- function(x) lapply(x, vegdist,method = "bray")

plot_nmds_outliers <- 
        function(x,data,mode = "point"){
                #- check that x is an NMDS object 
                if(!"metaMDS" %in% class(x)){ print("x must be a NMDS object"); break()}
                
                #-
                y <- data.table(x = x$points[,1], 
                                y = x$points[,2], 
                                brt12 = data$brt12, 
                                brt12_illies = data$brt12_illies, 
                                illies = data$illies, 
                                german = data$german_type, 
                                gr_sample_id = data$gr_sample_id)
                if (mode == "text"){
                        plot.out <- plot_ly(data = y, x = ~x, y = ~y, mode = "text", text = ~gr_sample_id, type = "scatter")        
                } else if (mode == "point") {
                        plot.out <- plot_ly(data = y, x = ~x, y = ~y, type = "scatter", mode = "markers")   
                }
                
                plot.out
        }


plot_nmds_eval <- 
        function(x, data, typ, poly = FALSE, wrap = FALSE, hull = FALSE){
                if(!"metaMDS" %in% class(x)){ print("x must be a NMDS object"); break()}
                
                #-
                y <- data.table(x = x$points[,1], 
                                y = x$points[,2],
                                gr_sample_id = data$gr_sample_id)
                
                if ("brt12" %in% names(data)) y$brt12 = data$brt12
                if ("brt12_illies" %in% names(data)) y$brt12_illies = data$brt12_illies
                if ("illies" %in% names(data)) y$illies = data$illies
                if ("german_type" %in% names(data)) y$german_type = data$german_type
                if ("brt12_bgr" %in% names(data)) y$brt12_bgr = data$brt12_bgr
                if ("bgr" %in% names(data)) y$bgr = data$bgr
                
                plot.columns <- c("x","y", typ)
                y2 <- y[, c(plot.columns), with = F]
                names(y2)[3] = "types"
                # Find the convex hull of the points being plotted
                convex.hull <- y2 %>% group_by(types) %>% slice(chull(x, y))

                p <- ggplot(y2, aes(x, y)) +
                        geom_point(aes(col = types), alpha = 1) +
                        geom_hline(yintercept = 0, lty = 2) +
                        geom_vline(xintercept = 0, lty = 2) +
                        theme(panel.grid = element_blank(), 
                              legend.position = "none") + 
                        ggtitle(typ) 
                        
                if (wrap) p <- p + facet_wrap(. ~ types)    
                if (poly) p <- p + stat_density_2d(aes(fill = types), geom = "polygon", colour="white")
                if (hull) p <- p + geom_polygon(data = convex.hull, aes(fill = types), alpha = 0.5)
                p 
                
        }



prep4 <- function(x){
        lapply(x, metaMDS, parallel = 8, trymax = 8)      
} 

prep5 <- function(x, x2, mode = "point") {
        for (i in seasons) {
                if (i == 1)
                        out.ls <- list()
                out.ls[[i]] <-
                        plot_nmds_outliers(x = x[[i]],
                                           data = x2[[i]],
                                           mode = mode)
                
                
        }
        out.ls
}


prep6 <- function(x,
                  x2,
                  poly = FALSE,
                  wrap = FALSE,
                  hull = FALSE) {
        for (i in seasons) {
                if (i == 1)
                        out.ls <- list()
                for (k in 1:length(typologies)) {
                        if (k == 1)
                                out.ls2 <- list()
                        out.ls2[[k]] <-
                                plot_nmds_eval(
                                        x = x[[i]],
                                        data = x2[[i]],
                                        typ = typologies[k],
                                        poly = poly,
                                        wrap = wrap,
                                        hull = hull
                                )
                }
                names(out.ls2) <- typologies
                out.ls[[i]] <- out.ls2
                
        }
        names(out.ls) <- all_seasons[seasons]
        out.ls
}


prep7 <- function(x, x2){
        x3 <- lapply(typologies, function(a) lapply(seasons, function(y) cluster.stats(x[[y]], clustering = as.numeric(as.factor(x2[[y]][[a]])))))
        x3
}




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
                geom_jitter(
                        size = 4, 
                        pch = 21, 
                        col = "white", 
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
                geom_jitter(
                        size = 3,
                        pch = 21,
                        col = "white",
                        width = 0.1,
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
        
        seasons <- get("out1", envir = globalenv())
        seasons <- seasons$seasons
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
                        size   = 4,
                        pch    = 21,
                        col    = "white",
                        alpha  = plot.alpha,
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
                geom_jitter(size = 5,
                           pch = 21,
                           col = "white",
                           width = 0.1,
                           alpha = plot.alpha) +
                theme_bw() +
               # ylim(0, .33) +
                facet_wrap(. ~ typology) +
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                ggtitle(paste(data.set, "-", taxon))
        p 
}

save.nmds.plot <- function(x, data.set, taxon){
        fail <- readline(prompt = paste("Is", data.set, "correct?"))
        if (fail == "no") break()
        
        for (i in seq_along(x)){
                for (k in seq_along(x[[i]])){
                        loop.name <- names(x)[i]
                        loop.names2 <- names(x[[i]])[k]
                        ggsave(filename = paste0("fig/indiviual_data_sets/nmds/",Sys.Date(),"_",data.set,"_",taxon,"_",loop.name,"_",loop.names2,".pdf"),
                               plot = x[[i]][[k]])
                }
        }
}

wraper_anosim <- function(x, grouping, parallel = 1, permutations = 999, add = NULL, season.data, typologies){
        
        
        seasons <- get("out1", envir = globalenv())
        seasons <- seasons$seasons
        x$season %<>% factor(levels = all_seasons[seasons])
        
        #- initialize with list to store the outputs of the for loop 
        out.ls <- list()
        #- loop over seasons 
        for (i in seq_along(seasons)){
                i.x <- x[[i]]
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
                                                x = i.x,
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
                rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        }
        out <- rbindlist(out.ls)
       out
}

anosim_plot1 <- function(x, taxon){
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


anosim_plot2 <- function(x, taxon){
        
        x$season %<>% factor(levels = all_seasons[seasons])
        
        p <- ggplot(
                data = x, 
                aes(
                        x = season, 
                        y = R, 
                        col = typology, 
                        group = typology)
        ) + 
                geom_line(aes(col = typology)) + 
                geom_jitter(
                        size = 5,
                        aes(shape = significant),
                        #col = "white",
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


anosim_plot3 <- function(x, taxon){
        
        p <- ggplot(data = x, 
                    aes(x = typology,
                        y = R, 
                        col = season, 
                        group = typology)) + 
                geom_jitter(
                        size = 4, 
                        aes(shape = significant),
                        width = 0.1, 
                        alpha = plot.alpha) + 
                stat_summary(geom = "point", fun = mean, pch = 151, size = 7, fill = "black") + 
                theme_bw() + 
                theme(axis.text.x = element_text(size = 6)) +
                #ylim(0,1.2) + 
                facet_grid(.~type) + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") 
        p
}
#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 
wraper_simper <- function(x, grouping, permutations, add = NULL){
        
        seasons <- which(all_seasons %in% unique(rbindlist(x)$season))
        
        #- initialize with list to store the outputs of the for loop
        out.ls <- list()
        #- loop over seasons
        for (i in seq_along(seasons)) {
                i.x <- x[[i]]
                i.grouping <- grouping[[i]]
                i.season   <- all_seasons[seasons[i]]
                #- create subset of spe2 with only typology variables
                setDT(i.grouping)
                i.anosim.typologies <-
                        i.grouping[, (typologies), with = F]
                i.anosim.typologies <-
                        as.data.frame(apply(i.anosim.typologies, 2, function(x)
                                str_replace_all(x, "_", "\\.")))
                
                #- for each typologies compute ANOSIM
                i.out <-
                        lapply(seq_along(i.anosim.typologies),
                                #1:2,
                                function(k)
                                        simper(
                                                comm = i.x,
                                                # distance matrix
                                                group = i.anosim.typologies[[k]],
                                                # Factor for grouping observations
                                                permutations = permutations,
                                                # number of permutations
                                                parallel = 1  # Number of parallel processes
                                        ))
                
                i.test <- list()
                for (j in seq_along(i.out)) {
                        j.out2 <- transpose(i.out[[j]])
                        j.simp1 <-
                                cbind(unlist(j.out2$species),
                                      as.data.frame(j.out2$p))
                        names(j.simp1)[1] <- "taxon"
                        j.simp2 <-
                                j.simp1 |>
                                pivot_longer(cols = !taxon) |>
                                mutate(name2 = name) |>
                                mutate(name = str_remove(name, "_.*")) |>
                                mutate(name2 = str_remove(name2, ".*_"))
                        
                        j.simp3 <-
                                bind_rows(select(j.simp2, !name2),
                                          select(j.simp2, !name))
                        setDT(j.simp3)
                        j.simp3[is.na(name), name := name2]
                        j.simp3[, name2 := NULL]
                        j.simp3[, significant := value <= 0.05]
                        j.simp4 <-
                                j.simp3[, .N, by = c("taxon", "name")]
                        j.simp5 <-
                                j.simp3[, sum(significant), by = c("taxon", "name")]
                        j.simp5[, proportion_significant := V1 / unique(j.simp4$N)]
                        j.simp6 <-
                                j.simp5[proportion_significant == 1]
                        stat <- nrow(j.simp6) / uniqueN(j.simp3$name)
                        i.test[[j]] <- stat
                        rm(list = ls()[grepl("^j\\.", ls())])
                        gc()
                }
                
                i.test <- data.table(simper_statistic = unlist(i.test))
                i.test$typology <- names(i.anosim.typologies)
                i.test$season <- i.season
                if(!is.null(add)) i.test$type = add
                out.ls[[i]] <- i.test
        }
        out <- rbindlist(out.ls)
        out
}
#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 
wraper_silhouette <- function(tax2, tax4, add = NULL, typologies){
        
        seasons <- get("out1", envir = globalenv())
        seasons <- seasons$seasons

        #- loop over seasons 
        for (i in seasons) {
                if (i == 1) sil.ls <- list()
                i.x <- tax4[[i]]
                i.season <- all_seasons[seasons[i]]
                i.grouping <- tax2[[i]]
                i.typologies <- i.grouping[, (typologies), with = F]
                
                for (k in seq_along(i.typologies)){
                        if (k == 1) i.ls <- list()
                        k.typ <- i.typologies[[k]]
                        k.dictionary <- data.table(v = k.typ,
                                                   cluster = as.numeric(as.factor(k.typ)))
 
                        k.sil <- silhouette(x = k.dictionary$cluster, 
                                            dist = i.x)
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
                }
                
                sil.ls[[i]] <- i.ls.b
                rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        }
        sil.out <- rbindlist(sil.ls)
        sil.out
}
#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 
silhouette.plot1 <- function(x, taxon = "family"){
        
        p <- ggplot(x, aes(x = true_type, y = sil_width)) + 
                geom_hline(yintercept = 0, lty = 2) +
                #geom_violin(aes(fill = season), draw_quantiles = 0.5) + 
                stat_summary(geom = "point", fun = mean, aes(fill = season), pch = 21, size = 4, position = position_jitter(width = 0.01)) + 
                facet_wrap(typ~typology, scales = "free") + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                theme_bw() + 
                theme(axis.text.x = element_text(size = 6))+ 
                ggtitle(paste(data.set,"-", taxon)) + 
                ylab("Mean Silhouette width") + xlab("")
        p
}
#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 
silhouette.plot3 <- function(x, taxon = "family"){
        
        p <- ggplot(x, aes(x = true_type, y = sil_width)) + 
                geom_hline(yintercept = 0, lty = 2) +
                #geom_violin(aes(fill = season), draw_quantiles = 0.5) + 
                stat_summary(geom = "point", fun = mean, aes(fill = season), pch = 21, size = 4, position = position_jitter(width = 0.01)) + 
                facet_wrap(typ~typology, scales = "free") + 
                scale_color_viridis_d(option = "cividis") + 
                scale_fill_viridis_d(option = "cividis") + 
                theme_bw() + 
                theme(axis.text.x = element_text(size = 6))+ 
                ggtitle(paste(data.set,"-", taxon)) + 
                ylab("Mean Silhouette width") + xlab("")
        p
}

#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 
call_all1 <- function(subset.name, german.type = FALSE,her = FALSE, taxon.level = "family"){
        #  —————————————————————————————— #
        #  ————— SUBSET DATA ———————————— # 
        #  —————————————————————————————— #
        #- subset to focal data set - skip for argument value "all"  
        if (! "all" %in% subset.name){
                data2.al <- lapply(1:4, function(x) data[[x]][data.set %in% subset.name])
        } else {
                data2.al <- data
        }
        #- what seasons are included? 
        seasons <- which(all_seasons %in% unique(rbindlist(data2.al)$season))

        #- remove remaining NA in national types if national types are used. 
        if (german.type){
                data2.al <- lapply(seasons, function(x) data2.al[[x]][!is.na(german_type)])
        }
        if (her){
                data2.al <- lapply(seasons, function(x) data2.al[[x]][!is.na(her)])
        }
        #- create subset of least impacted sites  
        data2.li <- lapply(seasons, function(x) data2.al[[x]][fec.least.impacted == TRUE])
        #- create subset of sites that are least impacted and removed from regional borders 
        data2.pu <- lapply(seasons, function(x) data2.li[[x]][illies_distance >= 25000 & bgr_distance >= 25000])

        sites.al <- lapply(seasons, function(x) st_as_sf(unique(data2.al[[x]]   , by = "gr_sample_id"), crs = 3035))
        sites.li <- lapply(seasons, function(x) st_as_sf(unique(data2.li[[x]], by = "gr_sample_id"), crs = 3035))
        sites.pu <- lapply(seasons, function(x) st_as_sf(unique(data2.pu[[x]], by = "gr_sample_id"), crs = 3035))
        #- check number of sites

        if (any(sapply(sites.pu, nrow) == 0)){
                id <- which(sapply(sites.pu, nrow) ==4)
                seasons <- setdiff(seasons, seasons[id])
        }

        seasons <- which(all_seasons %in% unique(rbindlist(data2.pu)$season))
        

        
        
        #  —————————————————————————————— #
        #  ————— PREPARE DATA ——————————— # 
        #  —————————————————————————————— #
        tax2.al <- prep1(data = data2.al, taxon = taxon.level)
        tax2.li <- prep1(data = data2.li, taxon = taxon.level)
        tax2.pu <- prep1(data = data2.pu, taxon = taxon.level)
        
        #- check typologies 
        check.typo <- 
                lapply(tax2.pu, function(x) x[, (typologies), with = F]) |> 
                sapply(function(x) apply(x,2,uniqueN)) |> 
                as.data.frame()
        
        if (any(check.typo == 1)){
                check.typo.id <- apply(check.typo,1, function(x) any(x==1))
                rm.types <- names(check.typo.id)[which(check.typo.id)]
                typologies <- setdiff(typologies, rm.types)
        }
        #-  prep2: drop gr_sample_id column and transform to class matrix 
        tax3.al <- prep2(x = tax2.al, data2 = data2.al)
        tax3.li <- prep2(x = tax2.li, data2 = data2.li)
        tax3.pu <- prep2(x = tax2.pu, data2 = data2.pu)
        
        #- remove taxa that occur in less than one percent of samples
        op.al <- sapply(sites.al, function(x) round(nrow(x)/100,0))
        op.li <- sapply(sites.li, function(x) round(nrow(x)/100,0))
        op.pu <- sapply(sites.pu, function(x) round(nrow(x)/100,0))
        
        colsum.al <- lapply(tax3.al, colSums)     
        colsum.li <- lapply(tax3.li, colSums)     
        colsum.pu <- lapply(tax3.pu, colSums)     
        for (i in seasons){
                if (any(colsum.al[[i]] <= min(op.al[1],1))){
                        rare.names <- which(colsum.al[[i]] <= min(op.al[1],1))
                        tax3.al[[i]] <- tax3.al[[i]][, -rare.names]
                        print(paste("Dropped", paste(sort(names(rare.names)), collapse = ", ")))
                }
        }
        for (i in seasons){
                if (any(colsum.li[[i]] <= min(op.li[1],1))){
                        rare.names <- which(colsum.li[[i]] <= min(op.li[1],1))
                        tax3.li[[i]] <- tax3.li[[i]][, -rare.names]
                        print(paste("Dropped", paste(sort(names(rare.names)), collapse = ", ")))
                }
        }
        for (i in seasons){
                if (any(colsum.pu[[i]] <= min(op.pu[1],1))){
                        rare.names <- which(colsum.pu[[i]] <= min(op.pu[1],1))
                        tax3.pu[[i]] <- tax3.pu[[i]][, -rare.names]
                        print(paste("Dropped", paste(sort(names(rare.names)), collapse = ", ")))
                }
        }
        
        #- prep3: compute Sorensen distance matrix using the vegan function vegdist 
        tax4.al <- tax3.al |>  prep3()
        tax4.li <- tax3.li |>  prep3()
        tax4.pu <- tax3.pu |>  prep3()
        
        out <- list(
                data2   = list(data2.al, data2.li, data2.pu),
                seasons = seasons, 
                tax2    = list(tax2.al, tax2.li, tax2.pu),
                tax3    = list(tax3.al, tax3.li, tax3.pu),
                tax4    = list(tax4.al, tax4.li, tax4.pu),
                typologies = typologies
        )
        out
}
#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 

call_indval <- function(call1.obj,
                        taxon.level = "family",
                        plot.results = FALSE) {
        tax8.al <-
                ivs(
                        data2 = call1.obj$tax2[[1]],
                        data3 = call1.obj$tax3[[1]],
                        season.data = call1.obj$seasons,
                        add = "all",
                        typologies = call1.obj$typologies
                )
        tax8.li <-
                ivs(
                        call1.obj$tax2[[2]],
                        call1.obj$tax3[[2]],
                        season.data = call1.obj$seasons,
                        add = "least impacted",
                        typologies = call1.obj$typologies
                )
        tax8.pu <-
                ivs(
                        call1.obj$tax2[[3]],
                        call1.obj$tax3[[3]],
                        season.data = call1.obj$seasons,
                        add = "purifed ecoregions",
                        typologies = call1.obj$typologies
                )
        tax8.all <- rbindlist(list(tax8.al, tax8.li, tax8.pu))
        names <- list(
                paste0(
                        "fig/indiviual_data_sets/ivs/",
                        Sys.Date(),
                        "_1_",
                        str_replace_all(data.set, "\\ ", "_"),
                        "_",
                        taxon.level,
                        ".jpeg"
                ),
                paste0(
                        "fig/indiviual_data_sets/ivs/",
                        Sys.Date(),
                        "_2_",
                        str_replace_all(data.set, "\\ ", "_"),
                        "_",
                        taxon.level,
                        ".jpeg"
                ),
                paste0(
                        "fig/indiviual_data_sets/ivs/",
                        Sys.Date(),
                        "_3_",
                        str_replace_all(data.set, "\\ ", "_"),
                        "_",
                        taxon.level,
                        ".jpeg"
                )
        )
        if (plot.results) {
                ggsave(
                        filename =  names[[1]],
                        ivs.plot1(tax8.all, taxon = taxon.level),
                        dpi = 600
                )
                ggsave(
                        filename =  names[[2]],
                        ivs.plot2(tax8.all, taxon = taxon.level),
                        dpi = 600
                )
                ggsave(
                        filename =  names[[3]],
                        ivs.plot3(tax8.all, taxon = taxon.level),
                        dpi = 600
                )
        }
        tax8.all
}


#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 
call_calssification.strength <-
        function(call1.obj,
                 taxon.level = "family",
                 plot.results = FALSE) {
                
        tax9.al  <-
                class.strength(call1.obj$tax2[[1]], call1.obj$tax4[[1]], season.data = call1.obj$seasons, add = "all")
        tax9.li  <-
                class.strength(call1.obj$tax2[[2]], call1.obj$tax4[[2]], season.data = call1.obj$seasons, add = "least impacted")
        tax9.pu  <-
                class.strength(call1.obj$tax2[[3]], call1.obj$tax4[[3]], season.data = call1.obj$seasons, add = "purifed ecoregions")
        
        
        tax9.all <- rbindlist(list(tax9.al, tax9.li,tax9.pu))
        
        
        if (plot.results) {
                names <- list(
                        paste0(
                                "fig/indiviual_data_sets/cs/",
                                Sys.Date(),
                                "_1_",
                                str_replace_all(data.set, "\\ ", "_"),
                                "_",
                                taxon.level,
                                ".jpeg"
                        ),
                        paste0(
                                "fig/indiviual_data_sets/cs/",
                                Sys.Date(),
                                "_2_",
                                str_replace_all(data.set, "\\ ", "_"),
                                "_",
                                taxon.level,
                                ".jpeg"
                        ),
                        paste0(
                                "fig/indiviual_data_sets/cs/",
                                Sys.Date(),
                                "_3_",
                                str_replace_all(data.set, "\\ ", "_"),
                                "_",
                                taxon.level,
                                ".jpeg"
                        )
                )
                ggsave(
                        filename = names[[1]],
                        class.strength.plot1(tax9.all, taxon = taxon.level),
                        dpi = 600
                )
                ggsave(
                        filename = names[[2]],
                        class.strength.plot2(tax9.all, taxon = taxon.level),
                        dpi = 600
                )
                ggsave(
                        filename = names[[3]],
                        class.strength.plot3(tax9.all, taxon = taxon.level),
                        dpi = 600
                )
        }
        tax9.all
}
#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 
call_anosim <- function(call1.obj, taxon.level = "family", plot.results = FALSE){

        tax10.al   <- wraper_anosim(x = call1.obj$tax4[[1]], grouping = call1.obj$tax2[[1]], season.data = call1.obj$season, typologies = call1.obj$typologies ,add = "all")
        tax10.li   <- wraper_anosim(x = call1.obj$tax4[[2]], grouping = call1.obj$tax2[[2]], season.data = call1.obj$season, typologies = call1.obj$typologies ,add = "least impacted")
        tax10.pu   <- wraper_anosim(x = call1.obj$tax4[[3]], grouping = call1.obj$tax2[[3]], season.data = call1.obj$season, typologies = call1.obj$typologies ,add = "purifed")
        tax10.all  <- rbindlist(list(tax10.al, tax10.li, tax10.pu))
        tax10.all[, significant := p.value <= 0.05]
       
        
         if (plot.results){
                names <- list(
                        paste0(
                                "fig/indiviual_data_sets/anosim/",
                                Sys.Date(),
                                "_1_",
                                str_replace_all(data.set, "\\ ", "_"),
                                "_",
                                taxon.level,
                                ".jpeg"
                        ),
                        paste0(
                                "fig/indiviual_data_sets/anosim/",
                                Sys.Date(),
                                "_2_",
                                str_replace_all(data.set, "\\ ", "_"),
                                "_",
                                taxon.level,
                                ".jpeg"
                        ),
                        paste0(
                                "fig/indiviual_data_sets/anosim/",
                                Sys.Date(),
                                "_3_",
                                str_replace_all(data.set, "\\ ", "_"),
                                "_",
                                taxon.level,
                                ".jpeg"
                        ))
                        ggsave(filename = names[[1]], anosim_plot1(tax10.all, taxon = taxon.level), dpi = 600 )
                        ggsave(filename = names[[2]], anosim_plot2(tax10.all, taxon = taxon.level), dpi = 600 )
                        ggsave(filename = names[[3]], anosim_plot3(tax10.all, taxon = taxon.level), dpi = 600 )
        }
        
        #- return output 
        tax10.all
}
call_silhouette <- function(call1.obj, taxon.level = "family", plot.results = FALSE){
        
        tax11.al  <- wraper_silhouette(tax = call1.obj$tax2[[1]], tax4 = call1.obj$tax4[[1]], typologies = call1.obj$typologies, add = "all")
        tax11.li  <- wraper_silhouette(tax = call1.obj$tax2[[2]], tax4 = call1.obj$tax4[[2]], typologies = call1.obj$typologies, add = "least impact")
        tax11.pu  <- wraper_silhouette(tax = call1.obj$tax2[[3]], tax4 = call1.obj$tax4[[3]], typologies = call1.obj$typologies, add = "purified regions")
        tax11.all <- rbindlist(list(tax11.al, tax11.li, tax11.pu))
        if (plot.results){
                
                names <- list(
                        paste0(
                                "fig/indiviual_data_sets/silhouette/",
                                Sys.Date(),
                                "_1_",
                                str_replace_all(data.set, "\\ ", "_"),
                                "_",
                                taxon.level,
                                ".jpeg"
                        ))
                ggsave(filename = names[[1]], silhouette.plot1(tax11.all, taxon = taxon.level), dpi = 600 )
        }
        tax11.all
}
#  ——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— # 

        # ——————————— BETACLUSTER ——————————— # 
        # ——————————————————————————————————— #
        
        
        # —————————————————————————————————————— #
        # ——————————— INDICATOR TAXA ——————————— # 
        # —————————————————————————————————————— #
        
        # —————————————————————————————— #
        # ——————————— SIMPER ——————————— # 
        # —————————————————————————————— #
        # spe11    <- spe3    |> wraper_simper(spe2   , 20, add = "all")
        # spe11.li <- spe3.li |> wraper_simper(spe2.li, 20, add = "least impacted")
        # spe11.pu <- spe3.pu |> wraper_simper(spe2.pu, 20,  add = "purifed ecoregions")
        # gen11    <- gen3    |> wraper_simper(gen2   , 999, 10, add = "all")
        # gen11.li <- gen3.li |> wraper_simper(gen2.li, 999, 10, add = "least impacted")
        # gen11.pu <- gen3.pu |> wraper_simper(gen2.pu, 999, 10, add = "purifed ecoregions")
        # fam11    <- fam3    |> wraper_simper(fam2   , 999, 10, add = "all")
        # fam11.li <- fam3.li |> wraper_simper(fam2.li, 999, 10, add = "least impacted")
        # fam11.pu <- fam3.pu |> wraper_simper(fam2.pu, 999, 1, add = "purifed ecoregions")
        # —————————————————————————————— #
        # —————————————————————————————— #
        # ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— #
#}
