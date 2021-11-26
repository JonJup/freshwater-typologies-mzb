# ———————————————————————— #
# ——— Plot: ANOSIM ——— # 
# ———————————————————————— #

# ———————————————————————————————————
#  date created: 18-10-21
# last modified: 15-11-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Plot ANOSIM
# ———————————————————————————————————


# setup -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(colorspace)
# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/2021-10-18_results_anosim.rds")
pal <- wesanderson::wes_palette("Darjeeling1")[c(3,4,5)]
# plots -----------------------------------------------------------------------------
anosim_plot <- 
        data |> 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn"))) |> 
        mutate(impaired = case_when(impaired == "least impaired" ~ "reference sites",
                                    impaired == "impaired" ~ "all sites")) |> 
        mutate(typology = stringr::str_to_upper(typology)) |> 
        ggplot(aes(x = R, y = typology)) + 
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
        geom_hline(yintercept = 3.5, alpha = c(0,1,1), color = "grey89") + 
        facet_grid(season~.) + 
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.major.x = element_line(colour = "grey89"), 
              axis.ticks = element_blank(), 
              axis.text.y = element_text(color = pal, size = 14, lineheight = .9),
              legend.key = element_blank(),
              panel.border = element_blank(), 
              panel.spacing.y = unit(0,"line")) + 
        
        labs(x = "ANOSIM R-Statistic", 
             y = NULL, 
             shape = NULL) + 
        scale_color_manual(values = pal, guide = "none") + 
        scale_fill_manual(values = pal , guide = "none") + 
        scale_x_continuous(
                limits = c(0,0.45),
                breaks = c(0,0.1,0.2,0.3,0.4),
                expand = c(.0,0)
        )  



ggsave(plot = anosim_plot, 
       filename = paste0("fig/results_anosim/",Sys.Date(),"_anosim_plot.png"), width = 5, height = 5)


# computations ----------------------------------------------------------------------

## mean values per typology 

data |> filter(typology == "ife" & impaired == "least impaired") |> summarize(R = mean(R))
data |> filter(typology == "bgr" & impaired == "least impaired") |> summarize(R = mean(R))
data |> filter(typology == "brt" & impaired == "least impaired") |> summarize(R = mean(R))

sum_ref <- filter(data, season == "summer" & impaired == "least impaired" & typology == "ife") |> select(R) 
sum_nre <- filter(data, season == "summer" & impaired == "impaired" & typology == "ife") |> select(R)  
aut_ref <- filter(data, season == "autumn" & impaired == "least impaired" & typology == "ife") |> select(R)  
aut_nre <- filter(data, season == "autumn" & impaired == "impaired" & typology == "ife") |> select(R)

sum_ref - sum_nre
aut_ref - aut_nre

