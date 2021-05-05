### -------------------------- ###
### --- NMDS of typologies --- ### 
### -------------------------- ###

# --------------- #
# date:   
#               04.05.21
# files in:  
#               -> 11_sxs_genus_W_bio_typology.rds
#               -> 12_sxs_genus_typology_wo_bio.rds
# files out:  
#               <- 
# Project: 
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose: 
#               Create a ordination of the sites to visualize groups    
# --------------- #


# setup -----------------------------------------------------------------------------
source("R/setup.R")

# load data  ----------------------------------------------------------------------------------------------------------------------------------------------
data = readRDS("data/12_sxs_genus_typology_wo_bio.rds")
data2 = readRDS("data/11_sxs_genus_W_bio_typology.rds")

# join typology with bioclusters  ---------------------------------------------------
data2 %<>% dplyr::select(gr_sample_id, bio)
data = left_join(x = data, 
                 y = data2, 
                 by = "gr_sample_id"
)
rm(data2);gc()

## -- remove rare types 
data %<>%
        filter(ls20 != "RT17") %>% 
        filter(!gloric %in%  c(17, 22)) %>% 
        filter(!illies %in%  c("Fenno-scandian shield"))  


## -- sites x species table as matrix and without typology columns
setDT(data)
data[, eea := str_remove(eea, "Bio-geographical")]
data[, eea := str_replace(eea, "\\ \\ ", "\\ ")]

ma_data = 
        copy(data) %>% 
        .[, c("gr_sample_id", "ls20", "ls12", "illies", "eea", "gloric", "bio") := NULL] %>% 
        as.matrix

## -- create distance matrix 
dt_distance = parallelDist(ma_data, method = "binary")

## -- NMDS 
# nmds_res <- metaMDS(comm = dt_distance)
# saveRDS(nmds_res, "data/xx_nmds_sampling_sites.rds")


## -- reshape NMDS 
nmds_dt <-
        data.table(
                NMDS1 = scores(nmds_res)[, 1],
                NMDS2 = scores(nmds_res)[, 2],
                gr_sample_id = data$gr_sample_id,
                brt20 = factor(data$ls20,
                               levels = paste0(
                                       "RT",
                                       c(1, 2, 3, 4, 5, 8, 9, 10, 11, 14, 15, 16, 18)
                               )),
                brt12 = factor(data$ls12,
                               levels = paste0(
                                       "RT",
                                       c(1, 2, 3, 4, 5,6,7, 8, 9, 10, 11)
                               )),
                gloric = factor(data$gloric),
                illies = factor(data$illies),
                bgr = factor(data$eea)
        )



# plot brt20 ------------------------------------------------------------------------
plot_brt20 <- 
        ggplot(nmds_dt, aes(x = NMDS1, y = NMDS2)) +
        geom_point(alpha = 0.6, pch = 21, aes(fill = brt20)) +
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2) +
        facet_wrap(. ~ brt20) +
        theme(
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "none",
                axis.text = element_blank()
        ) + 
        ggtitle("NMDS BRT20")  
        

# brt12 -----------------------------------------------------------------------------
plot_brt12 <- 
        ggplot(nmds_dt, aes(x = NMDS1, y = NMDS2)) +
        geom_point(alpha = 0.6, pch = 21, aes(fill = brt12)) +
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2) +
        facet_wrap(. ~ brt12) +
        theme(
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "none",
                axis.text = element_blank()
        ) +  
        ggtitle("NMDS BRT12")
# plot gloric -----------------------------------------------------------------------
plot_gloric <- 
        ggplot(nmds_dt, aes(x = NMDS1, y = NMDS2)) +
        geom_point(alpha = 0.6, pch = 21, aes(fill = gloric)) +
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2) +
        facet_wrap(. ~ gloric) +
        theme(
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "none",
                axis.text = element_blank()
        )+  
        ggtitle("NMDS GloRiC")
# plot illies -----------------------------------------------------------------------
plot_illies <- 
        ggplot(nmds_dt, aes(x = NMDS1, y = NMDS2)) +
        geom_point(alpha = 0.6, pch = 21, aes(fill = illies)) +
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2) +
        facet_wrap(. ~ illies) +
        theme(
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "none",
                axis.text = element_blank(),
                strip.text.x = element_text(size = 5)
        ) +  
        ggtitle("NMDS Illies' Ecoregions")
# plot bgr -----------------------------------------------------------------------
plot_bgr <-
        ggplot(nmds_dt, aes(x = NMDS1, y = NMDS2)) +
        geom_point(alpha = 0.6, aes(fill = bgr), pch = 21) +
        geom_hline(yintercept = 0, lty = 2) +
        geom_vline(xintercept = 0, lty = 2) +
        facet_wrap(. ~ bgr) +
        theme(
                panel.grid = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "none",
                axis.text = element_blank()
        ) +  
        ggtitle("NMDS Biogeographical Regions")

# save plotrs to file  --------------------------------------------------------------
ggsave(plot = plot_brt20, filename = "figures/nmds/brt20.eps")
ggsave(plot = plot_brt12, filename = "figures/nmds/brt12.eps")
ggsave(plot = plot_gloric, filename = "figures/nmds/gloric.eps")
ggsave(plot = plot_illies, filename = "figures/nmds/illies.eps")
ggsave(plot = plot_bgr, filename = "figures/nmds/bgr.eps")
ggsave(plot = plot_brt20, filename = "figures/nmds/brt20.png")
ggsave(plot = plot_brt12, filename = "figures/nmds/brt12.png")
ggsave(plot = plot_gloric, filename = "figures/nmds/gloric.png")
ggsave(plot = plot_illies, filename = "figures/nmds/illies.png")
ggsave(plot = plot_bgr, filename = "figures/nmds/bgr.png")



