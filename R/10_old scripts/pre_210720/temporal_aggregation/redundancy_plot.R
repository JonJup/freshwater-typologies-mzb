### ----------------------- ###
### --- REDUNDANCY PLOT --- ###
### ----------------------- ###


# setup -----------------------------------------------------------------------------

pacman::p_load(ggplot2)

# load data  ------------------------------------------------------------------------
red1 <- readRDS("data/temporal_aggregation/redundancies_bgr.rds")
red2 <- readRDS("data/temporal_aggregation/redundancies_brt12.rds")
red3 <- readRDS("data/temporal_aggregation/redundancies_brt20.rds")
red4 <- readRDS("data/temporal_aggregation/redundancies_gloric.rds")
red5 <- readRDS("data/temporal_aggregation/redundancies_illies.rds")

ta_1 <- readRDS("data/temporal_aggregation/ta_bgr_no-redundant.rds")
ta_2 <- readRDS("data/temporal_aggregation/ta_brt12_with-redundant.rds")
ta_3 <- readRDS("data/temporal_aggregation/ta_brt20_with-redundant.rds")
ta_4 <- readRDS("data/temporal_aggregation/ta_gloric_with-redundant.rds")
ta_5 <- readRDS("data/temporal_aggregation/ta_illies_non-redundant.rds")

# create table  ---------------------------------------------------------------------
red = data.table(
        similarity = c(red1, red2, red3, red4, red5),
        typology   = rep(
                c("BGR", "BRT12", "BRT20", "GloRiC", "Illies"),
                times = c(
                        length(red1),
                        length(red2),
                        length(red3),
                        length(red4),
                        length(red5)
                )
        )
)
red <- red[similarity != 2]

red %>% 
        mutate(
                typology = factor(x = typology,
                                  levels = c("BRT12", "BRT20", "GloRiC", "Illies", "BGR"))
                ) %>% 
        ggplot(aes(x = typology, y = similarity, fill = typology)) + 
        geom_violin(alpha = 0.5) + 
        geom_jitter(shape = 21, width = .05) + 
        stat_summary(geom="point",
                     fun=mean, 
                     size = 5) + 
        scale_fill_brewer(palette = "Dark2") + 
        theme(legend.position = "none", 
              panel.grid = element_blank())  + 
        ylab("Similarity of typical assemblages")-> 
        redundancy_plot 

ggsave(plot = redundancy_plot, "fig/tempagg/redundancy.png")
# size of typical assemblages  -----------------------------------------------------------------------------
for (i in 1:5) {
        if (i == 1)
                out_list <- vector(mode = "list", length = 5)
        loop_data <- get(paste0("ta_", i))
        loop_typology <-
                case_when(i == 1 ~ "BRT12",
                          i == 2 ~ "BRT20",
                          i == 3 ~ "GloRiC",
                          i == 4 ~ "Illies",
                          i == 5 ~ "BGR")
        
        loop_mean <-
                loop_data %>% lapply(nrow) %>% unlist 
        # %>% unlist %>% mean %>% round(1)
        # loop_sd   <-
        #         loop_data %>% lapply(nrow) %>% unlist %>% sd %>% round(1)
        # loop_min  <-
        #         loop_data %>% lapply(nrow) %>% unlist %>% min %>% round(1)
        # loop_max  <-
        #         loop_data %>% lapply(nrow) %>% unlist %>% max %>% round(1)
        
        
        out_list[[i]] <-
                tibble (
                        #metric = c("mean", "sd", "max", "min"),
                        typology = loop_typology,
                        value = loop_mean
                )
}
out_list <- rbindlist(out_list)

out_list %>% 
        ggplot(aes(x = typology, y = value, fill = typology)) + 
        geom_hline(yintercept = mean(out_list$value), lty = 2) + 
        geom_violin(alpha = 0.5) + 
        geom_point(shape = 21, position=position_jitter(width = 0.05)) + 
        stat_summary(geom = "point", 
                     fun = mean, 
                     group = "typology",
                     size = 3,
                     shape = 22, 
                     fill = "black") + 
        theme(legend.position = "none") -> 
        ta_size_plot 
        
        
## -- smallest overall mean 
ta_small_overall_mean <- out_list %>% filter(typology == "BGR") %>% pull(value) %>% mean() %>% round(1)

## -- largest overall mean 
ta_large_overall_mean <- out_list %>% filter(typology == "Illies") %>% pull(value) %>% mean() %>% round(1)
