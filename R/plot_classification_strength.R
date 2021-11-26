# ————————————————————————————————————————— #
# ——— Plot: Classification Strength ——————— # 
# ————————————————————————————————————————— #

# ————————————————————————————————————————— #
#  date created: 27-09-21
# last modified: 18-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute Classification strength
# ————————————————————————————————————————— #


# setup -----------------------------------------------------------------------------
library(ggplot2)
library(ggdist)
library(colorspace)
library(dplyr)
library(magrittr)
library(data.table)
library(GGally)
#- plot function
source("R/function_scherer_raincloud_plot.R")

# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/2021-10-14_cs_results.rds")

# prepare data  ---------------------------------------------------------------------

#- fix typo
data %<>% rename(typology = typlogy)

#  —— color palette —— # 
pal <- wesanderson::wes_palette("Zissou1", n = 5, type = "discrete")[c(1,3,5)]
pal <- wesanderson::wes_palette("Rushmore1",n=5)[c(3,4,5)]
pal <- wesanderson::wes_palette("Darjeeling1")[c(3,4,5)]
# plot  -----------------------------------------------------------------------------

rainclod_within_similartiy  <- scherer_raincloud_plot(filter(data, impaired == FALSE), pal = pal, var = "within_type", label = "within type similarity")
rainclod_between_similartiy <- scherer_raincloud_plot(filter(data, impaired == FALSE), pal = pal, var = "between_type", label = "between type similartiy")


data2 <- filter(data,impaired == FALSE) |> 
        mutate(typology = stringr::str_to_upper(typology)) |> 
        mutate(typology = factor(typology, c("IFE", "BRT","BGR"))) |> 
        mutate(typology_num = as.numeric(typology)) 

data2 %<>% tidyr::pivot_longer(cols = c(within_type, between_type), names_to = "wb", values_to = "similarity")

mean_var <- 
        group_by(data2, typology_num, wb) |> 
        summarise(mean = mean(similarity)) |> 
        mutate(mean = round(mean,2)) 

wtb <- ggplot(data2, aes(x = typology,
                  y = similarity)) +
        facet_grid(. ~ wb) +
        stat_halfeye(scale = 0.5,
                     aes(fill = typology),
                     point_interval = mean_qi) +
        geom_jitter(
                aes(x = typology_num - 0.2, fill = typology),
                shape = 21,
                width = 0.1,
                height = 0
        ) + 
        scale_fill_manual(values = pal) +
        geom_text(data = mean_var,
                  aes(y = mean,
                      x = typology_num + .3,
                      label = mean),
                  fontface = "bold") +
        labs(x = NULL,
             y = "similarity",
             fill = NULL) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major.y = element_line(colour = "grey89"),
                axis.ticks = element_blank(),
                axis.text.x = element_text(
                        color = pal,
                        size = 14,
                        lineheight = .9
                ),
                legend.key = element_blank(),
                legend.position = "none",
                panel.border = element_blank(),
                panel.spacing.y = unit(0, "line")
        )


cs_plot <- 
        data |> 
        unique(by = c("typology", "season", "impaired")) |> 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn"))) |>
        mutate(impaired = case_when(impaired == FALSE ~ "reference sites",
                                    impaired == TRUE ~ "all sites")) |> 
        mutate(typology = stringr::str_to_upper(typology)) |> 
        ggplot(aes(x = classification_strength, y = typology)) + 
        geom_jitter(
                aes(
                        shape = impaired, 
                        color = typology,
                        fill = after_scale(lighten(color, .1))
                        ), 
                #shape = 21, 
                size = 4, 
                width = 0, 
                height = 0.1, 
                alpha = 0.66
                ) + 
        geom_hline(yintercept = 3.5, alpha =  c(0,1,1), color = "grey85") + 
        facet_grid(season ~ .) + 
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.major.x = element_line(colour = "grey89"), 
              axis.ticks = element_blank(), 
              axis.text.y = element_text(color = pal, size = 14, lineheight = .9),
              legend.key = element_blank(),
              panel.border = element_blank(), 
              panel.spacing.y = unit(0,"line")) + 
        labs(x = "classification strength", 
             y = NULL, 
             shape = NULL) + 
        scale_color_manual(values = pal, guide = "none") + 
        scale_fill_manual(values = pal , guide = "none")
        
within_type_plot_brt <- 
        data |> filter(typology == "brt" & impaired == FALSE) |> 
        ggplot(aes(x = within_type, y = type)) + 
        geom_point(aes(fill = season), size = 4, shape = 21) + 
        stat_summary(geom = "point", fun = mean, col = "black", shape = "|", size = 6) + 
        scale_fill_manual(values = pal) + 
        theme(panel.background = element_blank(),
              panel.grid.major.x = element_line(colour = "grey89"), 
              panel.grid.major.y = element_line(colour = "grey89"), 
              axis.ticks.length = unit(0, "line")) + 
                labs(y = NULL, 
                     x = "within-type Similarity")
within_type_plot_brt
# save to file  ---------------------------------------------------------------------
ggsave(plot = rainclod_within_similartiy,  filename = paste0("fig/results_classification_strength/",Sys.Date(),"_within_similartiy.png"),  dpi = 320) 
ggsave(plot = rainclod_between_similartiy, filename = paste0("fig/results_classification_strength/",Sys.Date(),"_between_similartiy.png"), dpi = 320) 
ggsave(plot = cs_plot,                     filename = paste0("fig/results_classification_strength/",Sys.Date(),"_classification_strength.png"), height = 5, width = 5)
ggsave(plot = within_type_plot_brt,        filename = paste0("fig/results_classification_strength/",Sys.Date(),"_within_similarity_brt.png"))
ggsave(plot = within_simple,               filename = paste0("fig/results_classification_strength/",Sys.Date(),"_within_simple.png") , width = 6, height = 3)
ggsave(plot = wtb,                         filename = paste0("fig/results_classification_strength/",Sys.Date(),"_wtb.png"), width = 8, height = 5)


# plots for interpretation not for final paper --------------------------------------

## Parallel Plot across seasons 
data |> 
        mutate(id = paste0(type, impaired)) |> 
        ggplot(aes(x = season, y = within_type, fill = id, col = id, group = id)) + 
        geom_point() +
        stat_summary(geom = "line", fun = mean) +  
        theme(legend.position = "none")
        


data |> filter(typology == "brt" & impaired == FALSE) |> 
        ggplot(aes(x = between_type, y = type)) + 
        geom_point(aes(col = season)) + 
        stat_summary(geom = "point", fun = mean, col = "black", shape = "|", size = 5) + 
        scale_color_manual(values = pal)


# summaries --------------------------------------------------------------------------
## mean values per typology 

data |> filter(typlogy  == "ife" & impaired == FALSE) |> pull(classification_strength) |> unique() |> mean()
data |> filter(typlogy == "bgr" & impaired == FALSE) |> pull(classification_strength) |> unique() |> mean()
data |> filter(typlogy == "brt" & impaired == FALSE) |> pull(classification_strength) |> unique() |> mean()

## mean values per season 

data |> filter(typlogy  == "ife" & impaired == FALSE) |> pull(classification_strength) |> unique() |> mean()
data |> filter(typlogy == "bgr" & impaired == FALSE) |> pull(classification_strength) |> unique() |> mean()
data |> filter(typlogy == "brt" & impaired == FALSE) |> pull(classification_strength) |> unique() |> mean()