# --------------------------------------- #
# -- Test clusters with simulated data -- # 
# --------------------------------------- #


pacman::p_load(data.table, dplyr, tidyr)

# ----------------- #
# --- FUNCTIONS --- #
# ----------------- #
source("~/my documents/R/functions/genmean.R")
source("~/my documents/R/functions/mdist.R")
source("~/my documents/R/functions/silgen.R")


# --------------- #
# --- Options --- #
# --------------- #

n.sites   <- 200
n.taxa    <- 20
n.cluster <- 4 

#- assign sites to clusters randomly 
cl = sample(1:n.cluster, 
            size = n.sites, 
            replace = TRUE)
#- create "enviromnetal data" 
cl2 <- data.table(cl1 = 0, cl2 =0 , cl3 = 0, cl4 = 0, cl = cl)
cl2[cl == 1, cl1 := 1]
cl2[cl == 1, cl1 := 1]
cl2[cl == 2, cl2 := 1]
cl2[cl == 3, cl3 := 1]
cl2[cl == 4, cl4 := 1]

#- occurrence probability outside cluster 
low.val <-  .1
#- occurrence probability inside cluster
high.val <- .9

beta.cl1 <- c(high.val,low.val,low.val,low.val)
beta.cl2 <- c(low.val,high.val,low.val,low.val)
beta.cl3 <- c(low.val,low.val,high.val,low.val)
beta.cl4 <- c(low.val,low.val,low.val,high.val)
#beta.cl5 <- c(low.val,low.val,low.val,low.val)
beta <- rbind(beta.cl1, beta.cl2, beta.cl3, beta.cl4)#, beta.cl5)
sp.cl.id <- sample(1:n.cluster, size = n.taxa, replace = TRUE)
sp <- matrix(0, nrow = n.sites, ncol = n.taxa)

test.ls <- list()

for (ä in 1:3){
        
        # ———————————————————— #
        # — DEFINE VARIABLES — # 
        # ———————————————————— #
        
        # indval = list()
        # indval2 = list()
        # min.sil = max.sil = 
        #         m1.sil = m2.sil = zero.sil = p2.sil = 
        #sa.A = sa.A.g = sa.r = sa.r.g = indval.overall.mean = indval.best.mean = 
        #         indval.g.overall.mean = indval.g.best.mean = sa.b.overall = sa.b.best = sa.cog.g = sa.cog.g = 
        #         sa.ind.g = sa.ind = sa.cos 
        # c()
        
        
        
        set.seed(ä)
        for (i in 1:nrow(sp)) {
                for (j in 1:ncol(sp)) {
                        p <- sum(cl2[i, 1:4, with = F] * beta[sp.cl.id[j], ])
                        p <- rbinom(n = 1,
                                    size = 1,
                                    prob = p)
                        sp[i, j] <- p
                }
        }
        all(unique(cl) %in% cl[rowSums(sp) != 0])
        sp <- sp[rowSums(sp) != 0, ]
        
        
        
        test.dist <- parallelDist(x = sp, method = "dice")
        test.sim  <- 1 - test.dist
        cluster.obj <- agnes(x = test.dist,
                             method = "flexible",
                             par.method = 0.625)
        # cluster.obj.sim <- agnes(x = test.sim,
        #                      method = "flexible",
        #                      par.method = 0.625)
        


        cluster.obj = as.hclust(cluster.obj)
        #cluster.obj.sim = as.hclust(cluster.obj.sim)
        
        lp.cut      <-
                lapply(2:10, function(x)
                        cutree(cluster.obj, k = x))
        lp.cut.sim      <-
                lapply(2:10, function(x)
                        cutree(cluster.obj.sim, k = x))
        
        dt.cs <- cs
        
        for (cut in 1:length(lp.cut)){
                if (cut == 1) cs = c()
                lpü.clust <- lp.cut[[cut]]
        
        for (ü in 1:uniqueN(lpü.clust)){
                if (ü == 1) csü <- c()
                id1   <- which(lpü.clust == ü)  
                id.n1 <- which(lpü.clust != ü)
                sim1   <-  as.matrix(test.sim)[id1, id1]
                sim.n1 <-  as.matrix(test.sim)[id1, id.n1]
                ut <- sim1[upper.tri(sim1)]
                lt <- sim1[lower.tri(sim1)]
                ut.n <- sim.n1[upper.tri(sim.n1)]
                lt.n <- sim.n1[lower.tri(sim.n1)]
                csü[ü] <- mean(append(ut,lt)) - mean(append(ut.n,lt.n))
        }
                cs[cut] <- mean(csü)
                
        }
        
        
        
        lp.cs       <-
                lapply(lp.cut, function(x)
                        cluster.stats(d = test.dist, clustering = x))
        lp.cs.sim       <-
                lapply(lp.cut.sim, function(x)
                        cluster.stats(d = test.dist, clustering = x))
        lp.sw <-
                sapply(lp.cs, function(x)
                        x$avg.silwidth)
        lp.sw.sim <-
                sapply(lp.cs.sim, function(x)
                        x$avg.silwidth)
        lp.cs.1 <-
                sapply(lp.cs, function(x)
                        x$average.between - x$average.within)
        # lp.cs.2 <-
        #         sapply(lp.cs, function(x)
        #                 x$average.between/x$average.within) 
        # lp.cs.1.sim <-
        #         sapply(lp.cs.sim, function(x)
        #                 x$average.within - x$average.between)
        # lp.cs.2.sim0 <-
        #         sapply(lp.cs.sim, function(x)
        #                 x$average.within/x$average.between) 
        
        lp.ch     <- sapply(lp.cs, function(x) x$ch )
        lp.ch.sim <- sapply(lp.cs.sim, function(x) x$ch )

        

        # —————————————————— #
        # — COMPUTE INDVAL — # 
        # —————————————————— #
        
        # for (ö in 1:9) {
        #         indval[[ö]] <-
        #                 multipatt(
        #                         sp,
        #                         cluster = lp.cut[[ö]],
        #                         control = how(nperm = 999),
        #                         max.order = 1,
        #                         print.perm = T
        #                 )
        #         
        #         #- strassoc
        #         sa.A[ö]   <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "A")   |>
        #                 apply(1, function(x)
        #                         x[which.max(x)]) |>
        #                 mean()
        #         sa.A.g[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "A.g") |>
        #                 apply(1, function(x)
        #                         x[which.max(x)]) |>
        #                 mean()
        #         sa.r[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "r") |>
        #                 apply(2, function(x)
        #                         ifelse(x < 0, NA, x)) |>
        #                 rowSums(na.rm = T) |>
        #                 mean()
        #         sa.r.g[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "r.g") |>
        #                 apply(2, function(x)
        #                         ifelse(x < 0, NA, x)) |>
        #                 rowSums(na.rm = T) |>
        #                 mean()
        #         indval.overall.mean[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "IndVal") |>
        #                 apply(2, mean) |>
        #                 mean()
        #         indval.best.mean[ö] <-
        #                 strassoc(sp, lp.cut[[ö]], "IndVal") |>
        #                 apply(1, function(x)
        #                         x[which.max(x)]) |>
        #                 mean()
        #         indval.g.overall.mean[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "IndVal.g") |>
        #                 apply(2, mean) |>
        #                 mean()
        #         indval.g.best.mean[ö] <-
        #                 strassoc(sp, lp.cut[[ö]], "IndVal.g") |>
        #                 apply(1, function(x)
        #                         x[which.max(x)]) |>
        #                 mean()
        #         sa.b.overall[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "B") |>
        #                 apply(2, mean) |>
        #                 mean()
        #         sa.b.best[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "B") |>
        #                 apply(1, function(x)
        #                         x[which.max(x)]) |>
        #                 mean()
        #         sa.cos[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "cos") |>
        #                 apply(1, function(x)
        #                         x[which.max(x)]) |>
        #                 mean()
        #         sa.cog.g[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "cos") |>
        #                 apply(1, function(x)
        #                         x[which.max(x)]) |>
        #                 mean()
        #         sa.ind[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "r.ind") |>
        #                 apply(1, function(x)
        #                         x[which.max(x)]) |>
        #                 mean()
        #         sa.ind.g[ö] <-
        #                 strassoc(sp, cluster = lp.cut[[ö]], func = "r.ind.g") |>
        #                 apply(1, function(x)
        #                         x[which.max(x)]) |>
        #                 mean()
        #         
        #         min.sil[ö] <-
        #                 mean(silgen(
        #                         d = test.dist,
        #                         p = -Inf,
        #                         gr = lp.cut[[ö]]
        #                 )[, 1])
        #         max.sil[ö] <-
        #                 mean(silgen(d = test.dist,
        #                             p = Inf,
        #                             gr = lp.cut[[ö]])[, 1])
        #         m1.sil[ö] <-
        #                 mean(silgen(
        #                         d = test.dist,
        #                         p = -1,
        #                         gr = lp.cut[[ö]]
        #                 )[, 1])
        #         zero.sil[ö] <-
        #                 mean(silgen(d = test.dist,
        #                             p = 0,
        #                             gr = lp.cut[[ö]])[, 1])
        #         m2.sil[ö] <-
        #                 mean(silgen(
        #                         d = test.dist,
        #                         p = -2,
        #                         gr = lp.cut[[ö]]
        #                 )[, 1])
        #         p2.sil[ö] <-
        #                 mean(silgen(d = test.dist,
        #                             p = 2,
        #                             gr = lp.cut[[ö]])[, 1])
        #         
        # }
        # indval <- lapply(indval,
        #                  function(x)
        #                          data.table(
        #                                  taxon  = rownames(x$A),
        #                                  pvalue = x$sign$p.value,
        #                                  indval = x$sign$stat
        #                          ))
        # 
        # indval |>
        #         lapply(function(x)
        #                 x[, sig05 := sum(pvalue <= 0.05)]) |>
        #         lapply(function(x)
        #                 x[, sig01 := sum(pvalue <= 0.01)]) |>
        #         lapply(function(x)
        #                 x[, mean_indval := mean(indval)]) |>
        #         lapply(function(x)
        #                 x[pvalue <= 0.05, mean_indval_p := mean(indval)]) ->
        #         indval2
        # 
        # indval2 <- data.table(
        #         n.clust = 2:10,
        #         sig05  = unlist(lapply(indval, function(x)
        #                 nrow(x[pvalue <= 0.5]))),
        #         sig01  = unlist(lapply(indval, function(x)
        #                 nrow(x[pvalue <= 0.1]))),
        #         mean = unlist(lapply(indval, function(x)
        #                 mean(x$indval))),
        #         mean05  = unlist(lapply(indval, function(x)
        #                 mean(x[pvalue <= 0.5, indval]))),
        #         mean01  = unlist(lapply(indval, function(x)
        #                 mean(x[pvalue <= 0.1, indval]))),
        #         cs = lp.classtre ,
        #         sil_width = lp.silwidth, 
        #         ch = lp.ch,
        #         run = ä,
        #         min.sil = min.sil,
        #         max.sil = max.sil,
        #         m1.sil = m1.sil, 
        #         m2.sil = m2.sil, 
        #         p2.sil = p2.sil, 
        #         zero.sil = zero.sil
                # sa.A  = sa.A, 
                # sa.A.g  = sa.A.g, 
                # sa.r  = sa.r, 
                # sa.r.g  = sa.r.g, 
                # indval.overall.mean  = indval.overall.mean, 
                # indval.best.mean  = indval.best.mean,
                # indval.g.overall.mean  = indval.g.overall.mean, 
                # indval.g.best.mean  = indval.g.best.mean, 
                # sa.b.overall  = sa.b.overall, 
                # sa.b.best  = sa.b.best, 
                # sa.cog.g  = sa.cog.g, 
                # sa.ind.g  = sa.ind.g, 
                # sa.ind  = sa.ind, 
                # sa.cos = sa.cos
        # )
        
        out <- data.table(run = ä, 
                          n.clust = 2:10,
                          lp.sw ,
                          lp.sw.sim ,
                          lp.cs.1 ,
                          cs,
                          lp.ch     ,
                          lp.ch.sim 
                          ) 
        
        out %<>% pivot_longer(cols = !c("n.clust", "run"))
        test.ls[[ä]] <- out
        print(paste("Finished run", ä))
}
# ————————————————

test.ls |> 
        rbindlist() |> 
        group_by(run) |> 
        ggplot(aes(n.clust, value,group = run)) +
        geom_line(alpha = 0.2) + 
        geom_smooth(se = FALSE, aes(group = 1)) + 
        #geom_point() +
        facet_wrap(. ~ name, scale = "free") + 
        geom_vline(xintercept = 4, lty = 2)
