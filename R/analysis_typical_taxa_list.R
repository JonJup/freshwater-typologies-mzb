# —————————————————————————————— #
# ——— Analysis: Typical 2——————— # 
# —————————————————————————————— #

# ———————————————————————————————————
#  date created: 18-10-21
# last modified: 18-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute Typical communities 
# ———————————————————————————————————


# setup -----------------------------------------------------------------------------
library(jjmisc)
library(corrplot)
library(data.table)
library(dplyr)
library(ggplot2)
# load data -------------------------------------------------------------------------
sp.i <- readRDS("data/02_combined_data/2021-10-14_community_data_spring_impacted.rds")
su.i <- readRDS("data/02_combined_data/2021-10-14_community_data_summer_impacted.rds")
au.i <- readRDS("data/02_combined_data/2021-10-14_community_data_autumn_impacted.rds")
sp.l <- readRDS("data/02_combined_data/2021-10-14_community_data_spring_least_impacted.rds")
su.l <- readRDS("data/02_combined_data/2021-10-14_community_data_summer_least_impacted.rds")
au.l <- readRDS("data/02_combined_data/2021-10-14_community_data_autumn_least_impacted.rds")
id   <- readRDS("data/02_combined_data/2021-10-14_distance_ids.rds")


# typical plot function -------------------------------------------------------------
typical_corrplot <- function(data1, data2, data3){
        all_taxa1 <- 
                data1 |> 
                unlist() |> 
                unique() |> 
                sort() 
        all_taxa2 <- 
                data2 |> 
                unlist() |> 
                unique() |> 
                sort() 
        all_taxa3 <- 
                data3 |> 
                unlist() |> 
                unique() |> 
                sort() 
        all_types1 <-  names(data1)
        all_types2 <-  names(data2)
        all_types3 <-  names(data3)
        data1.2 <- as.data.frame(
                matrix(0, 
                       ncol = length(all_types1), 
                       nrow = length(all_taxa1)
                       )
                )
        data2.2 <- as.data.frame(
                matrix(0, 
                       ncol = length(all_types2), 
                       nrow = length(all_taxa2)
                       )
                )
        data3.2 <- as.data.frame(
                matrix(0, 
                       ncol = length(all_types3), 
                       nrow = length(all_taxa3)
                       )
                )
        
        colnames(data1.2) <- all_types1
        rownames(data1.2) <- all_taxa1
        colnames(data2.2) <- all_types2
        rownames(data2.2) <- all_taxa2
        colnames(data3.2) <- all_types3
        rownames(data3.2) <- all_taxa3
        
        for (i in seq_along(data1)){
                i.typical <- data1[[i]]  
                i.id      <- which(rownames(data1.2) %in% i.typical)
                data1.2[i.id, i] <- 1 
        }
        for (i in seq_along(data2)){
                i.typical <- data2[[i]]  
                i.id      <- which(rownames(data2.2) %in% i.typical)
                data2.2[i.id, i] <- 1 
        }
        for (i in seq_along(data3)){
                i.typical <- data3[[i]]  
                i.id      <- which(rownames(data3.2) %in% i.typical)
                data3.2[i.id, i] <- 1 
        }
        data1.3 <-
                data1.2 |>
                mutate(taxon = rownames(data1.2)) |>
                tidyr::pivot_longer(cols = !taxon,
                                    names_to = "type",
                                    values_to = "spring")
        
        
        data2.3 <-
                data2.2 |>
                mutate(taxon = rownames(data2.2)) |>
                tidyr::pivot_longer(cols = !taxon,
                                    names_to = "type",
                                    values_to = "summer")
        
        
        data3.3 <-
                data3.2 |>
                mutate(taxon = rownames(data3.2)) |>
                tidyr::pivot_longer(cols = !taxon,
                                    names_to = "type",
                                    values_to = "autumn")
        
        data_all <-     
            full_join(data1.3,
                                  data2.3,
                                  by = c("taxon", "type")) |>
            full_join(data3.3,
                      by = c("taxon", "type"))
        
    data_all <- mutate(data_all, spring = ifelse(is.na(spring), 0, spring))
    data_all <- mutate(data_all, summer = ifelse(is.na(summer), 0, summer))
    data_all <- mutate(data_all, autumn = ifelse(is.na(autumn), 0, autumn))
    data_all <- mutate(data_all, total = spring + summer +autumn)
    data_all <- select(data_all, c(taxon, type, total))    
    data_all$total <- factor(data_all$total)
    ## Illies freshwater ecoregions need a fix
    if ("Central plains" %in% data_all$type){
        data_all2 <- data.table(
            taxon = c("Gerridae", "Gerridae", "Calopterygidae", "Calopterygidae"), 
            type = c("Dinaric western Balkan", "Eastern Balkan", "Dinaric western Balkan", "Eastern Balkan"),
            total = 0
        )
        data_all <- rbindlist(list(data_all,data_all2))
    }
    plot.obj <- 
        ggplot(data_all, 
               aes(
                   x = type, y = taxon, fill = total
               )) + 
        geom_tile() + 
        # scale_fill_gradient(
        #     low = "#FFFFFF",
        #     high = "#4855FB"
        #         ) + 
        scale_fill_brewer(palette = "Blues", type = "qual") + 
        theme(
            panel.background = element_rect(fill = "white"), 
            axis.ticks.length = unit(0, "line"),
            axis.text.x = element_text(angle = 90),
            plot.margin = unit(c(1,1,1,1), "cm")
        ) + 
        labs(fill = "seasons",
             x = NULL,
             y = NULL) 
    plot.obj
}



# typical ---------------------------------------------------------------------------
sp.l.brt <- compute_typical_comm(com = sp.l[,-1], grouping = id$sp.l$brt, out = "typical_communities")
sp.l.bgr <- compute_typical_comm(com = sp.l[,-1], grouping = id$sp.l$bgr, out = "typical_communities")
sp.l.ife <- compute_typical_comm(com = sp.l[,-1], grouping = id$sp.l$ife, out = "typical_communities")
su.l.brt <- compute_typical_comm(com = su.l[,-1], grouping = id$su.l$brt, out = "typical_communities")
su.l.bgr <- compute_typical_comm(com = su.l[,-1], grouping = id$su.l$bgr, out = "typical_communities")
su.l.ife <- compute_typical_comm(com = su.l[,-1], grouping = id$su.l$ife, out = "typical_communities")
au.l.brt <- compute_typical_comm(com = au.l[,-1], grouping = id$au.l$brt, out = "typical_communities")
au.l.bgr <- compute_typical_comm(com = au.l[,-1], grouping = id$au.l$bgr, out = "typical_communities")
au.l.ife <- compute_typical_comm(com = au.l[,-1], grouping = id$au.l$ife, out = "typical_communities")



corrplot.brt <- typical_corrplot(sp.l.brt, su.l.brt, au.l.brt)
corrplot.ife <- typical_corrplot(sp.l.ife, su.l.ife, au.l.ife)
corrplot.bgr <- typical_corrplot(sp.l.bgr, su.l.bgr, au.l.bgr)


# save to file ----------------------------------------------------------------------

ggsave(plot = corrplot.brt, filename = paste0("fig/results_typical/", Sys.Date(), "_typical_taxa_brt.png"), dpi = 320, height = 4.5)
ggsave(plot = corrplot.ife, filename = paste0("fig/results_typical/", Sys.Date(), "_typical_taxa_ife.png"), dpi = 320, height = 6)
ggsave(plot = corrplot.bgr, filename = paste0("fig/results_typical/", Sys.Date(), "_typical_taxa_bgr.png"), dpi = 320, height = 4.5)





data1 <- sp.l.ife
data2 <- su.l.ife
data3 <- au.l.ife
