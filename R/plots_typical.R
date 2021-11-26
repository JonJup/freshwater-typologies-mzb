# ———————————————————————— #
# ——— Plots: Typical ——————— # 
# ———————————————————————— #

# ———————————————————————————————————
#  date created: 18-10-21
# last modified: 18-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Create visualizations for Typical taxa statistics
# ———————————————————————————————————


# setup -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(data.table)
library(ggdist)
library(colorspace)
# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/2021-10-14_results_typical.rds")

# prep data -------------------------------------------------------------------------
pal <- wesanderson::wes_palette("Darjeeling1")[c(3,4,5)]
data <- mutate(data, 
               impaired = ifelse(least_impaired, "least impaired", "impaired"), 
               typology_num = as.numeric(as.factor(typology)),
               season = factor(season, levels = c("spring", "summer", "autumn")),
               typology = stringr::str_to_upper(typology))

data_least <- data[impaired == "least impaired"]
data_impai <- data[impaired == "impaired"]

# plots -----------------------------------------------------------------------------
typical_plot_fun <- function(data){
        out <- ggplot(
                data,
                aes(
                y = value.similarity,
                x = season,
                col = typology
                ), 
        ) +
        stat_eye(
                aes(
                        y = value.similarity,
                        x = typology, 
                        col = typology,
                        fill = after_scale(lighten(col, 0.5))
                ),
                point_size = 5,
                point_interval = mean_qi,
                interval_linetype = "dashed", 
                interval_colour = "black",
                interval_alpha = 0,
                .width = c(0.25,.75)
        ) + 
                scale_color_manual(values = pal, guide = "none")  + 
                facet_grid(.~season) + 
                geom_text(
                        aes(
                                x = typology_num,
                                y = value.mean - .05,
                                label = format(round(value.mean, 2), nsmall = 2)
                        ),
                        color = "black",
                        inherit.aes = FALSE,
                        fontface = "bold",
                        size = 3.5
                )+ 
                coord_cartesian(clip = "off") + 
                theme(panel.grid.major.y = element_line(color = "grey89", size = 0.5),
                      axis.text.x = element_text(color = pal, size = 14, lineheight = .9),
                      axis.ticks.length = unit(0, "lines"), 
                      plot.subtitle = element_text(margin = c(0,0,-10,0)),
                      panel.background = element_rect(fill = "white"),
                      panel.spacing.x = unit(0, "lines")
                ) + 
                labs(y = "Similarity between typical communities", 
                     x = NULL) 
        return(out)
}
(typical_plot_fun(data_least))
        
        

}
# save to file  ---------------------------------------------------------------------
(typical_plot_l <- typical_plot_fun(data_least))
typical_plot_i <- typical_plot_fun(data_impai)
ggsave(
        filename = paste0(
                "fig/results_typical/",
                Sys.Date(),
                "_typical_communities_least_impaired.png"
        ),
        plot = typical_plot_l,
        # dpi = 320,
         width = 5, 
         height = 5
        # units = "cm"
)
ggsave(
        filename = paste0(
                "fig/results_typical/",
                Sys.Date(),
                "_typical_communities_impaired.png"
        ),
        plot = typical_plot_i,
        # #dpi = 320,
        width = 5,
        height = 5
        # units = "cm"
)

