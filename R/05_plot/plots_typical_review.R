# ---------------------------------- #
# --- Plots: Typical --------------- #
# --- Genus + Relative Abundance --- # 
# ---------------------------------- #

#  date created: 29.04.22
# last modified: 29.04.22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Create visualizations for Typical taxa statistics


# setup -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(data.table)
library(ggdist)
library(colorspace)
library(magrittr)
library(rstudioapi)
library(wesanderson)
library(stringr)

x<-getActiveDocumentContext()
sink(file = paste0("R/05_plot/log_files/typical_review","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

# load data -------------------------------------------------------------------------
data <- readRDS("data/review/2022-04-29_results_typical_review.rds")

# prep data -------------------------------------------------------------------------
pal <- wes_palette("Darjeeling1")[c(3,4,5)]
pal <- rev(pal)
data[, value.median := median(value.similarity), by = c("typology", "season")]
data <- mutate(data, 
               typology_num = as.numeric(as.factor(typology)),
               season = factor(season, levels = c("spring", "summer", "autumn")),
               typology = str_to_upper(typology))

data2 <- copy(data)
data2 <- mutate(data2, typology = factor(typology, levels = c("BRT", "BGR", "IFE")))
data2[, value.median := median(value.similarity),  by = c("typology", "season")]

n_obs <- function(x, s){
        out <- 
                filter(x, season == s)  |> 
                unique(by = "value.type1") %$% 
                table(typology) |> 
                as_tibble() |> 
                mutate(season = s) |> 
                setDT()
        return(out)
}

n_obs_out <- rbindlist(list(
        n_obs(data2, "spring"), 
        n_obs(data2, "summer"), 
        n_obs(data2, "autumn") 
))


data2 <- left_join(data2,n_obs_out, by = c("typology", "season") )
data2 %<>% mutate(season = factor(season, levels = c("spring", "summer", "autumn")))

# plots -----------------------------------------------------------------------------
typical_plot_fun <- function(data){
        out <- ggplot(
                data,
                aes(
                        y = value.similarity,
                        x = typology,
                        fill = typology
                ), 
        ) +
                geom_boxplot() + 
                scale_fill_manual(values = pal, guide = "none")  + 
                facet_grid(.~season) + 
                geom_label(
                        aes(
                                x = typology_num,
                                y = .05,
                                label = paste("n =", n+1)
                                #format(round(value.median, 2), nsmall = 2)
                        ),
                        color = "black",
                        inherit.aes = FALSE,
                        fontface = "bold",
                        size = 3.5
                ) +
                geom_vline(xintercept = 3.5, color = "grey89", size = 0.5) + 
                coord_cartesian(clip = "off") + 
                theme(panel.grid.major.y = element_line(color = "grey89", size = 0.5),
                      #panel.grid.major.x = element_line(color = "grey89", size = 0.5),
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
(typical_plot_l <- typical_plot_fun(data2))
        
# save to file  ---------------------------------------------------------------------

ggsave(
        filename = paste0(
                "fig/results_typical/",
                Sys.Date(),
                "_typical_communities_review.png"
        ),
        plot = typical_plot_l,
        # dpi = 320,
         width = 6, 
         height = 6
        # units = "cm"
)
# ggsave(
#         filename = paste0(
#                 "fig/results_typical/",
#                 Sys.Date(),
#                 "_typical_communities_impaired.png"
#         ),
#         plot = typical_plot_i,
#         # #dpi = 320,
#         width = 5,
#         height = 5
#         # units = "cm"
# )

