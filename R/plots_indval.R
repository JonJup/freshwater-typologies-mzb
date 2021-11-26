# ———————————————————————— #
# ——— Plots: IndVal ——————— # 
# ———————————————————————— #

# ———————————————————————————————————
#  date created: 13-10-21
# last modified: 20-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Create visualizations for IndVal statistics
# ———————————————————————————————————


# setup -----------------------------------------------------------------------------
library(colorspace)
library(dplyr)
library(ggdist)
library(ggplot2)
library(magrittr)
library(tidyr)

# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/2021_11_19_results_indval_data.rds")
pval <- readRDS("data/02_combined_data/2021_11_19_results_indval_pvalue.rds")


# prepare data ----------------------------------------------------------------------
pal <- wesanderson::wes_palette("Darjeeling1")[c(3,4,5)]

data %<>% 
        mutate(impaired = ifelse(least.impaired, FALSE, TRUE)) %>%
        mutate(impaired2 = case_when(impaired == TRUE ~ "all sites",
                                     impaired == FALSE ~ "reference sites")) %>%
        mutate(impaired2 = factor(impaired2, levels = c("all sites", "reference sites"))) %>%
        mutate(typology = stringr::str_to_upper(typology)) %>%
        ## make a factor out of season to fix the order 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn"))) %>% 
        ## create a numeric version typology 
        mutate(typology_num = as.numeric(as.factor(typology))) 

data2 <- 
        data |> 
        group_by(id, season, typology, impaired2) |> 
        summarise(sum_indval = sum(stat))

n.data <- 
        data |> 
        #filter(id == 0) |> 
        group_by(season, typology, impaired2,id) |> 
        count()
mean_stat <- 
        data |> 
        #filter(id == 0) |> 
        group_by(season, typology, impaired2) |> 
        summarise(mean_stat = mean(stat))
n.data <- 
        left_join(n.data, 
          mean_stat, 
          by = c("season", "typology", "impaired2"))

n.data %<>% mutate(typology_num = as.numeric(as.factor(typology)))
# plots -----------------------------------------------------------------------------


## wrap plot in function 
indval_plot <- function(var, label){
        
        f.data <- data 
        f.data %<>% rename(var = all_of(var))
        out <- 
                f.data |>
                ## exclude permutations
                filter(id == 0) |>
                ## exclude impaired samples
                filter(least.impaired == TRUE) |>
                ## —— start of plot —— ##
                ggplot(aes(x = var,
                           y = typology)
                       ) +
                ## halfeye for data
                stat_halfeye(
                        aes(
                                x = var,
                                y = typology,
                                col = typology,
                                fill = after_scale(lighten(color, 0.5))
                        ),
                        #alpha = 0.9,
                        point_size = 4,
                        scale = 0.5,
                        .width = c(0, 1)
                ) +
                # ## jitter for data
                # geom_jitter(
                #         aes(y = typology_num - .2,
                #             color = typology),
                #         size = 1,
                #         width = 0,
                #         height = 0.1
                # ) +
                ## background halfeye for permutations
                stat_halfeye(
                        data = filter(f.data, id != 0),
                        aes(x = var,
                            y = typology),
                        col = "black",
                        alpha = 0.4,
                        point_size = 4,
                        .width = c(0, 0),
                        scale = 0.5
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
                # geom_text(
                #         data =
                #                 f.data |>
                #                 filter(id == 0) |>
                #                 group_by(typology, typology_num) |>
                #                 summarize(n = n(), max = max(var, na.rm = T)),
                #         aes(
                #                 x = max,
                #                 y = typology_num + 0.5,
                #                 label = glue::glue("n = {n}"),
                #                 color = typology
                #         )
                # ) +
                ## facet
                facet_grid(season~.) +
                ## prettying
                scale_fill_manual(values = pal , guide = "none") +
                scale_color_manual(values = pal, guide = "none") +
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
                        panel.spacing.y = unit(0, "lines")
                ) +
                labs(x = label,
                     y = NULL)
       return(out) 
}
##  ———————————————————————— END OF FUNCTION —————————————————————————— ### 
options(warn = 0)
(meanstat <- indval_plot(var = "stat", label = "Indicator Value"))
(Aplot    <- indval_plot(var = "A", label = "IndVal A statistic"))
(Bplot    <- indval_plot(var = "B", label = "IndVal B statistic"))


# p Value plot ----------------------------------------------------------------------
ggplot(pval, aes(x = p.value, y = typology, col = season)) + 
        geom_jitter(width = 0, height = 0.1) + 
        facet_wrap(.~variable) + 
        geom_vline(xintercept = 0.05, lty = 2)


# Number of indicators --------------------------------------------------------------

(n_indi_plot <- 
        n.data |> 
        ## no permutations 
        filter(id == 0) |> 
        ggplot(
               aes(
                x = n,
                y = typology,
                shape = impaired2,
                col = typology
        )) +
        geom_point(size = 4) +
        geom_point(data = filter(n.data, id != 0),
                   aes(
                           x = n,
                           y = typology,
                   ),
                   col = "black",
                   alpha = 0.2,
                   shape = "|"
                   ) +
        facet_grid(season ~ .) +
        ## draw lines to seperate facets 
        geom_hline(
                yintercept = 3.5,
                alpha = c(0,1,1),
                colour = "grey89"
                ) + 
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
                panel.spacing.y = unit(0,"line")
        ) +
        scale_color_manual(values = pal, guide = "none") +
        labs(x = "Number of indicator families",
             y = NULL,
             shape = NULL) +
        scale_shape_discrete(labels = c("all sites", "reference sites"))        
)


# Number vs indicator value ---------------------------------------------------------

##  What is the relationship between the mean indicator value and the number of indicator
##  families?

n_vs_stat <- 
        ## omit permutations 
        filter(n.data, id == 0) |> 
        ggplot( 
               aes(
                       x = n, 
                       y = mean_stat, 
                       col = typology)
               ) + 
        geom_point() + 
        geom_smooth(method = "lm", 
                    se = F) + 
        scale_color_manual(values = pal, 
                           guide = "none") + 
        theme(panel.background = element_rect(fill = "white"),
              panel.grid = element_line(color = "grey89"), 
              axis.ticks.length = unit(0, "lines"),
              legend.key = element_blank()) + 
        labs(x = "Number of Indicator taxa", 
             y = "Mean Indicator value",
             col = NULL)  + 
        annotate(geom = "text", 
                 x = c(60, 40, 60), 
                 y = c(0.33, 0.33, 0.25), 
                 label = c("bgr", "ife", "brt"), 
                 color = pal[c(1,3,2)], 
                 size = 6)

# AB Plot  --------------------------------------------------------------------------
data |>
        filter(id == 0) |>
        filter(least.impaired == TRUE) |>
        ggplot(
               aes(x = A,
                   y = B)) +
        geom_text(label = "") +
        geom_smooth(se = FALSE, method = "lm") + 
        facet_grid(typology ~ season)
data |>
        filter(id != 0) |>
        filter(least.impaired == TRUE) |>
        ggplot(
               aes(x = A,
                   y = B)) +
        geom_point() +
        geom_smooth(se = FALSE, method = "lm") + 
        facet_grid(typology ~ season)


# SI- compare imaired and least impaired sites --------------------------------------


indval_impaire <- data |>
        ## exclude permutations
        filter(id == 0) |>
        ## exclude impaired samples
        filter(least.impaired == TRUE) |>
        ## —— start of plot —— ##
        ggplot(aes(x = stat,
                   y = typology)) +
        ## halfeye for data
        stat_halfeye(
                aes(
                        x = stat,
                        y = typology,
                        col = typology,
                        fill = after_scale(lighten(color, 0.5))
                ),
                #alpha = 0.9,
                point_size = 4,
                scale = 0.5
        ) +
        ## jitter for data
        geom_jitter(
                aes(y = typology_num - .2,
                    color = typology),
                size = 1,
                width = 0,
                height = 0.1
        ) +
        ## background halfeye for permutations
        stat_halfeye(
                data = filter(data, id == 0 & least.impaired == FALSE),
                aes(x = stat,
                    y = typology),
                col = "black",
                alpha = 0.4,
                point_size = 4,
                .width = c(0, 0),
                scale = 0.5
        ) +
        ## add median values as text
        geom_text(
                data =
                        data |>
                        filter(id == 0) |>
                        group_by(season, typology_num, typology) |>
                        summarise(m = median(stat)),
                aes(
                        x = m,
                        y = typology_num + .2,
                        label = format(round(m, 2), nsmall = 2),
                ),
                inherit.aes = FALSE,
                fontface = "bold",
                size = 3.5
        ) +
        ## facet
        facet_grid(. ~ season) +
        ## prettying
        scale_fill_manual(values = pal , guide = "none") +
        scale_color_manual(values = pal, guide = "none") +
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
                panel.spacing.x = unit(0,"line")
        ) +
        labs(x = "Indicator Value",
             y = NULL)



# sum of indval plot  ---------------------------------------------------------------

data2 |> 
        ggplot(aes(x = sum_indval, y = typology)) + 
        geom_point(aes(shape = impaired)) + 
        facet_grid(season~.)


# save to file ----------------------------------------------------------------------
ggsave(plot = meanstat,         filename = paste0("fig/results_indval/",Sys.Date(),"_meanstat.png"), height = 5, width = 5)
ggsave(plot = Aplot,            filename = paste0("fig/results_indval/",Sys.Date(),"_Aplot.png"))
ggsave(plot = Bplot,            filename = paste0("fig/results_indval/",Sys.Date(),"_Bplot.png"))
ggsave(plot = n_indi_plot,      filename = paste0("fig/results_indval/",Sys.Date(),"_nind.png"), height = 5, width = 5)
ggsave(plot = n_vs_stat,        filename = paste0("fig/results_indval/",Sys.Date(),"_n_vs_stat.png"))
ggsave(plot = indval_impaire,   filename = paste0("fig/results_indval/",Sys.Date(),"_indvalimpair.png"))


# summary statistics ----------------------------------------------------------------
data |> filter(id == 0, typology == "bgr") |> summarise(mean(stat))
data |> filter(id == 0, typology == "ife") |> summarise(mean(stat))
data |> filter(id == 0, typology == "brt") |> summarise(mean(stat))


##  ———————————————————————— END OF SCRIPT —————————————————————————— ###

