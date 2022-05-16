# ------------------------------ #
# ---Analysis: Typical 2 ------- # 
# ------------------------------ #

#  -----------------------------------
#  date created: 18-10-21
# last modified: 04-05-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute Typical communities 
#  -----------------------------------


# setup -----------------------------------------------------------------------------
library(jjmisc)
library(corrplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(rstudioapi)
library(tidyr)

x<-getActiveDocumentContext()
sink(file = paste0("R/02_combined_data/log_files/typical_taxa_lists","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)


# load data -------------------------------------------------------------------------
sp <-readRDS("data/02_combined_data/04_2022-05-04_community_data_spring.rds")
su <-readRDS("data/02_combined_data/04_2022-05-04_community_data_summer.rds")
au <-readRDS("data/02_combined_data/04_2022-05-04_community_data_autumn.rds")
id   <-readRDS("data/02_combined_data/04_2022-05-04_distance_ids.rds")

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
        all_types1 <- names(data1)
        all_types2 <- names(data2)
        all_types3 <- names(data3)
        data1.2    <- 
                as.data.frame(matrix(0,
                                     ncol = length(all_types1),
                                     nrow = length(all_taxa1)))
        data2.2    <- 
                as.data.frame(matrix(0,
                                     ncol = length(all_types2),
                                     nrow = length(all_taxa2)))
        data3.2    <- 
                as.data.frame(matrix(0,
                                     ncol = length(all_types3),
                                     nrow = length(all_taxa3)))
        
        colnames(data1.2) <- all_types1
        rownames(data1.2) <- all_taxa1
        colnames(data2.2) <- all_types2
        rownames(data2.2) <- all_taxa2
        colnames(data3.2) <- all_types3
        rownames(data3.2) <- all_taxa3
        
        for (i in seq_along(data1)){
                i.typical <-data1[[i]]  
                i.id      <-which(rownames(data1.2) %in% i.typical)
                data1.2[i.id, i] <-1 
        }
        for (i in seq_along(data2)){
                i.typical <-data2[[i]]  
                i.id      <-which(rownames(data2.2) %in% i.typical)
                data2.2[i.id, i] <-1 
        }
        for (i in seq_along(data3)){
                i.typical <-data3[[i]]  
                i.id      <-which(rownames(data3.2) %in% i.typical)
                data3.2[i.id, i] <-1 
        }
        data1.3 <-
                data1.2 |>
                mutate(taxon = rownames(data1.2)) |>
                pivot_longer(cols = !taxon,
                                    names_to = "type",
                                    values_to = "spring")
        
        
        data2.3 <-
                data2.2 |>
                mutate(taxon = rownames(data2.2)) |>
                pivot_longer(cols = !taxon,
                                    names_to = "type",
                                    values_to = "summer")
        
        
        data3.3 <-
                data3.2 |>
                mutate(taxon = rownames(data3.2)) |>
                pivot_longer(cols = !taxon,
                                    names_to = "type",
                                    values_to = "autumn")
        
        data_all <-    
            full_join(data1.3,
                                  data2.3,
                                  by = c("taxon", "type")) |>
            full_join(data3.3,
                      by = c("taxon", "type"))
        
    data_all <-mutate(data_all, spring = ifelse(is.na(spring), 0, spring))
    data_all <-mutate(data_all, summer = ifelse(is.na(summer), 0, summer))
    data_all <-mutate(data_all, autumn = ifelse(is.na(autumn), 0, autumn))
    data_all <-mutate(data_all, total = spring + summer +autumn)
    data_all <-select(data_all, c(taxon, type, total))    
    data_all$total <-factor(data_all$total)
    
    # - FIX Perlidae in BRT
    if ("RT12" %in% data_all$type){
            data_all2 <-data.table(
                            taxon = c("Perlidae", "Hydroptilidae", "Empididae"),
                            type = c("RT12"),
                            total = 0
                        )
             data_all <-rbindlist(list(data_all,data_all2))
    }
   
    if ("Alps" %in% data_all$type){
            # - which possible combinations of taxa and types are missing from data_all?
            # - First, we create a table with all possible combinations
            all_taxa <- unique(data_all$taxon)
            all_types <- unique(data_all$type)
            grid <- expand.grid(all_taxa, all_types)
            setDT(grid)
            setDT(data_all)
            grid[,combined := paste0(Var1, "_", Var2)]
            data_all[,combined := paste0(taxon, "_", type)]
            # - which combinations do not occur in data_all? 
            missing_id <- which(!grid$combined %in% data_all$combined)
            grid2 <- grid[missing_id, c("Var1", "Var2")]
            names(grid2) <- c("taxon", "type")
            grid2[, total := 0]
            data_all[, combined := NULL]
            data_all <- rbindlist(list(data_all, grid2))
    } 
    
    data_all$taxon <- factor(data_all$taxon)
    
    plot.obj <-
        ggplot(data_all, 
               aes(
                   x = type, y = taxon, fill = total
               )) + 
        geom_tile() + 
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
sp.l.brt <-compute_typical_comm(com = sp[,-1], grouping = id$sp.i$brt, out = "typical_communities")
sp.l.bgr <-compute_typical_comm(com = sp[,-1], grouping = id$sp.i$bgr, out = "typical_communities")
sp.l.ife <-compute_typical_comm(com = sp[,-1], grouping = id$sp.i$ife, out = "typical_communities")
su.l.brt <-compute_typical_comm(com = su[,-1], grouping = id$su.i$brt, out = "typical_communities")
su.l.bgr <-compute_typical_comm(com = su[,-1], grouping = id$su.i$bgr, out = "typical_communities")
su.l.ife <-compute_typical_comm(com = su[,-1], grouping = id$su.i$ife, out = "typical_communities")
au.l.brt <-compute_typical_comm(com = au[,-1], grouping = id$au.i$brt, out = "typical_communities")
au.l.bgr <-compute_typical_comm(com = au[,-1], grouping = id$au.i$bgr, out = "typical_communities")
au.l.ife <-compute_typical_comm(com = au[,-1], grouping = id$au.i$ife, out = "typical_communities")

(corrplot.brt <-typical_corrplot(sp.l.brt, su.l.brt, au.l.brt))
(corrplot.ife <-typical_corrplot(sp.l.ife, su.l.ife, au.l.ife))
(corrplot.bgr <-typical_corrplot(sp.l.bgr, su.l.bgr, au.l.bgr))


# save to file ----------------------------------------------------------------------

ggsave(plot = corrplot.brt, filename = paste0("fig/results_typical/", Sys.Date(), "_typical_taxa_brt.png"), dpi = 320, height = 4.5, width = 4.5)
ggsave(plot = corrplot.ife, filename = paste0("fig/results_typical/", Sys.Date(), "_typical_taxa_ife.png"), dpi = 320, height = 6, width = 4.5)
ggsave(plot = corrplot.bgr, filename = paste0("fig/results_typical/", Sys.Date(), "_typical_taxa_bgr.png"), dpi = 320, height = 4.5, width = 4.5)


# create lists ----------------------------------------------------------------------

lapply(au.l.ife, paste, collapse  = ", ")
lapply(su.l.ife, paste, collapse  = ", ")
lapply(sp.l.ife, paste, collapse  = ", ")

lapply(au.l.brt, paste, collapse  = ", ")
lapply(su.l.brt, paste, collapse  = ", ")
lapply(sp.l.brt, paste, collapse  = ", ")

lapply(au.l.bgr, paste, collapse  = ", ")
lapply(su.l.bgr, paste, collapse  = ", ")
lapply(sp.l.bgr, paste, collapse  = ", ")



