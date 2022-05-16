# —————————————————————————————— #
# ——— Plots: Mantel test ——————— # 
# —————————————————————————————— #

# ———————————————————————————————————
#  date created: 25-10-21
# last modified: 25-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Create visualizations for Mantel statistics
# ———————————————————————————————————


# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, ggplot2, ggdist, colorspace)

# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/2021-10-22_results_mantel.rds")


# prepare data ----------------------------------------------------------------------
data[, stat_sig := p_value < 0.05]

# plot ------------------------------------------------------------------------------

mantel_plot <- 
        data |> 
        ggplot(aes(x = statistic, y = levels)) + 
        geom_jitter(
                aes(
                        col = levels
                    ), 
                size = 3,
                height = 0.2,
                width = 0
        ) + 
        geom_hline(yintercept = 3.5, alpha =  c(0,1,1), color = "grey85") +
        stat_summary(geom = "point", size = 10, col = "black", shape = "|", fun = mean) + 
        facet_grid(type ~ ., scales = "free_y") + 
        labs(x = "Mantel statistic", 
             y = NULL) + 
        theme(legend.position = "none", 
              panel.background = element_rect(fill = "white"),
              panel.grid.major.x = element_line(colour = "grey89"),
              axis.ticks.length = unit(0, "lines"),
              panel.spacing = unit(0, "lines"))
ggsave(filename = paste0("fig/results_mantel/", Sys.Date(), "_results_mantel.png"), dpi = 320, height = 5)
                   