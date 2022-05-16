# ---------------------- #
# --- Plots: IndVal ---- # 
# ---------------------- #

#  date created: 13-10-21
# last modified: 05-05-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Create visualizations for IndVal statistics



# setup -----------------------------------------------------------------------------
library(colorspace)
library(cowplot)
library(dplyr)
library(ggdist)
library(ggplot2)
library(magrittr)
library(tidyr)
library(rstudioapi)
library(wesanderson)
library(stringr)

x<-getActiveDocumentContext()
sink(file = paste0("R/05_plot/log_files/indval","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/2022-05-04_results_indval_data.rds")
pval <- readRDS("data/02_combined_data/2022-05-04_results_indval_pvalue.rds")

# prepare data ----------------------------------------------------------------------
# - define color palette 
pal <- wes_palette("Darjeeling1")[c(3,4,5)]

# - reshape data
data %<>% 
        mutate(typology = str_to_upper(typology)) %>%
        mutate(typology = factor(typology, levels = c("IFE", "BGR", "BRT"))) %>%
        ## make a factor out of season to fix the order 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn"))) %>% 
        ## create a numeric version typology 
        mutate(typology_num = as.numeric(as.factor(typology))) 

# - create summary data set 
data2 <- 
        data |> 
        group_by(id, season, typology) |> 
        summarise(sum_indval = sum(stat))

mean_stat <- 
        data |> 
        group_by(season, typology) |> 
        summarise(mean_stat = mean(stat))

n.data <- 
        data |> 
        group_by(season, typology, id) |> 
        count() |> 
        left_join(mean_stat, 
                  by = c("season", "typology")) |> 
        mutate(typology_num = as.numeric(as.factor(typology)))

seasonal_mean <- 
        data |> 
        group_by(season) |> 
        summarize(mean_stat = mean(stat))
        
train3 <- train3 %>%
        group_by(Sex) %>%
        mutate(Int = mean(Age))

# plots -----------------------------------------------------------------------------
#- wrap plot in function 
indval_plot <- function(var, label){
        
        f.data <- data 
        f.data %<>% rename(var = all_of(var))
        out <- 
                f.data |>
                ## exclude permutations
                filter(id == 0) |>
                ## —— start of plot —— ##
                ggplot(aes(x = var,
                           y = typology)
                       ) +
                ## facet
                facet_grid(.~season) +
                
                ## halfeye for data
                stat_halfeye(
                        aes(
                                x = var,
                                y = typology,
                                col = typology,
                                fill = after_scale(lighten(color, 0.6)),
                                shape = season
                        ),
                        point_size = 6,
                        scale = 0.5,
                        .width = 0
                ) +
                ## add median values as text 
                geom_text(data = 
                                  f.data |> 
                                  filter(id == 0) |> 
                                  group_by(season, typology_num, typology) |> 
                                  summarise(m = median(var)), 
                        aes(
                                x = m,
                                y = typology_num - .2,
                                label = format(round(m, 2), nsmall = 2),
                        ),
                        inherit.aes = FALSE,
                        fontface = "bold",
                        size = 3.5
                ) +
                ## prettying
                scale_fill_manual(values = pal , guide = "none") +
                scale_color_manual(values = pal, guide = "none") +
                scale_shape_manual(values = c(16,15,18)) + 
                theme(
                        legend.key = element_blank(),
                        panel.background = element_rect(fill = "white"),
                        panel.grid.major.x = element_line(colour = "grey89"),
                        axis.text.y = element_text(
                                color = pal,
                                size = 14,
                                lineheight = .9
                        ),
                        axis.ticks.length = unit(0, "lines") ,
                        strip.text.x = element_blank()        
                        #panel.spacing.y = unit(0, "lines")
                        ) +
                labs(x = label, y = NULL) + 
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = NA, color = "black")
       return(out) 
}
## ------ END OF FUNCTION ------ ### 

(meanstat <- indval_plot(var = "stat", label = "Indicator Value"))

# Number of indicators --------------------------------------------------------------
(
        n_indi_plot <-
                n.data |>
                ## no permutations
                filter(id == 0) |>
                ggplot(aes(x = n, y = typology)) +
                geom_point(aes(fill = typology, shape = season), size = 6) +
                theme(
                        legend.key = element_blank(),
                        panel.background = element_rect(fill = "white"),
                        panel.grid.major.x = element_line(colour = "grey89"),
                        axis.text.y = element_text(
                                color = pal,
                                size = 14,
                                lineheight = .9
                        ),
                        axis.ticks.length = unit(0, "lines"),
                        panel.border = element_blank(),
                        panel.spacing.y = unit(0, "line")
                ) +
                scale_fill_manual(values = pal, guide = "none") +
                scale_color_manual(values = pal, guide = "none") +
                scale_shape_manual(values = c(21,22,23)) +
                labs(x = "Number of indicator families",
                     y = NULL,
                     shape = NULL)
)


# combine plots ---------------------------------------------------------------------

legend <- get_legend(n_indi_plot)
n_indi_plot2 <- n_indi_plot + theme(legend.position = "none", axis.text.y = element_blank())
meanstat2 <- meanstat + theme(legend.position = "none")
combined_plot <- plot_grid(meanstat2, legend, n_indi_plot2, nrow = 1, rel_widths = c(1,.25,.75), labels = c("A", "","B"))

# save to file ----------------------------------------------------------------------
ggsave(plot = meanstat,         filename = paste0("fig/results_indval/",Sys.Date(),"_meanstat.png"), height = 5, width = 5)
ggsave(plot = n_indi_plot,      filename = paste0("fig/results_indval/",Sys.Date(),"_nind.png"), height = 5, width = 5)
ggsave(plot = combined_plot,    filename = paste0("fig/results_indval/",Sys.Date(),"_combined.png"), width = 8, heigh = 3.5)

# summary statistics ----------------------------------------------------------------
data |> filter(id == 0, typology == "BGR") |> summarise(mean(stat))
data |> filter(id == 0, typology == "IFE") |> summarise(mean(stat))
data |> filter(id == 0, typology == "BRT") |> summarise(mean(stat))


##  ———————————————————————— END OF SCRIPT —————————————————————————— ###

