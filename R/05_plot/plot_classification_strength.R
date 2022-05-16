# ----------------------------------------- #
# --- Plot: Classification Strength ------- # 
# ----------------------------------------- #
#
#  date created: 27-09-21
# last modified: 04-05-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Compute Classification strength

# setup -----------------------------------------------------------------------------
pacman::p_load(
        ggplot2,
        ggdist,
        colorspace,
        dplyr,
        magrittr,
        data.table,
        GGally,
        wesanderson,
        rstudioapi,
        stringr,
        tidyr
)

#- plot function
source("R/funcitons/function_scherer_raincloud_plot.R")

x<-getActiveDocumentContext()
sink(file = paste0("R/05_plot/log_files/classification_strength","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)


# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/2022-05-04_cs_results.rds")

# prepare data  ---------------------------------------------------------------------

#- fix typo
data %<>% rename(typology = typlogy)

#  -- color palette -- # 
pal  <- wes_palette("Darjeeling1")[c(3,4,5)]
pal3  <- wes_palette("Darjeeling1")[c(5,4,3)]
pal2 <- wes_palette("Rushmore1",n=5)[c(3,4,5)]
# plot  -----------------------------------------------------------------------------

rainclod_within_similartiy  <- scherer_raincloud_plot(filter(data), pal = pal, var = "within_type", label = "within type similarity")
rainclod_between_similartiy <- scherer_raincloud_plot(filter(data), pal = pal, var = "between_type", label = "between type similartiy")


data2 <- data |> 
        mutate(typology = str_to_upper(typology)) |> 
        mutate(typology = factor(typology, c("BRT", "BGR", "IFE"))) |> 
        mutate(typology_num = as.numeric(typology)) 

data2 %<>% pivot_longer(cols = c(within_type, between_type), names_to = "wb", values_to = "similarity")

mean_var <- 
        group_by(data2, typology_num, wb) |> 
        summarise(mean = mean(similarity)) |> 
        mutate(mean = round(mean,2)) 

# - facet labels
fl <- c("between type", "within type")
names(fl) <- c("between_type", "within_type")

wtb <- ggplot(data2, aes(x = typology,
                  y = similarity)) +
        facet_grid(. ~ wb, labeller = labeller(wb = fl)) +
        stat_halfeye(scale = 0.5,
                     aes(fill = typology),
                     point_interval = mean_qi, .width = 0) +
        geom_jitter(
                aes(x = typology_num - 0.2, fill = typology),
                shape = 21,
                width = 0.1,
                height = 0,
        ) + 
        scale_fill_manual(values = pal3) +
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
                        color = pal3,
                        size = 14,
                        lineheight = .9
                ),
                legend.key = element_blank(),
                legend.position = "none",
                panel.border = element_blank(),
                panel.spacing.y = unit(0, "line")
        )
wtb

shapes <- c(21,22,23)

cs_plot <- 
        data |> 
        unique(by = c("typology", "season")) |> 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn"))) |>
        mutate(typology = str_to_upper(typology)) |> 
        mutate(typology = factor(typology, levels = c("IFE", "BGR", "BRT"))) |> 
        
        ggplot(aes(x = classification_strength, y = typology)) + 
        geom_point(aes(shape = season, fill = typology), size = 6) + 
        stat_summary(geom = "point", fun = mean, shape = "|", size = 16) + 
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
        scale_fill_manual(values = pal , guide = "none") + 
        scale_shape_manual(values=shapes) 
cs_plot

        
within_type_plot_brt <- 
        data |> 
        filter(typology == "brt") |> 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn"))) |> 
        ggplot(aes(x = within_type, y = type)) + 
        geom_boxplot() + 
        # geom_point(aes(col = season), size = 1) + 
        # stat_summary(geom = "point", fun = mean, col = "black", shape = "|", size = 10) + 
        scale_color_manual(values = pal2) + 
        theme(panel.background = element_blank(),
              panel.grid.major.x = element_line(colour = "grey89"), 
              panel.grid.major.y = element_line(colour = "grey89"), 
              axis.ticks.length = unit(0, "line")) + 
                labs(y = NULL, 
                     x = "within-type Similarity")
within_type_plot_brt

cs_plot_graphical_abstract <- 
        data |> 
        unique(by = c("typology", "season", "impaired")) |> 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn"))) |>
        mutate(typology = str_to_upper(typology)) |> 
        mutate(typology = factor(typology, levels = c("IFE", "BGR", "BRT"))) |> 
        filter(impaired == FALSE) |> 
        ggplot(aes(x = classification_strength, y = season)) + 
        geom_jitter(
                aes(fill = typology), 
                width = 0, 
                height = .2, 
                shape = 21, 
                size = 6 
                )  + 
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.major.x = element_line(colour = "grey89"), 
              axis.ticks = element_blank(), 
              axis.text.y = element_text(size = 14, lineheight = .9),
              axis.title.x = element_text(size = 14),
              legend.key = element_blank(),
              panel.border = element_blank(), 
              panel.spacing.y = unit(0,"line")) + 
        labs(x = "classification strength", 
             y = NULL, 
             shape = NULL) + 
        scale_color_manual(values = pal, guide = "none") + 
        scale_fill_manual(values = pal , guide = "none")
        



# save to file  ---------------------------------------------------------------------
ggsave(plot = rainclod_within_similartiy,  filename = paste0("fig/results_classification_strength/",Sys.Date(),"_within_similartiy.png"),  dpi = 320) 
ggsave(plot = rainclod_between_similartiy, filename = paste0("fig/results_classification_strength/",Sys.Date(),"_between_similartiy.png"), dpi = 320) 
ggsave(plot = cs_plot,                     filename = paste0("fig/results_classification_strength/",Sys.Date(),"_classification_strength.png"), height = 5, width = 5)
ggsave(plot = within_type_plot_brt,        filename = paste0("fig/results_classification_strength/",Sys.Date(),"_within_similarity_brt.png"))
ggsave(plot = within_sample,               filename = paste0("fig/results_classification_strength/",Sys.Date(),"_within_simple.png") , width = 6, height = 3)
ggsave(plot = wtb,                         filename = paste0("fig/results_classification_strength/",Sys.Date(),"_wtb.png"), width = 8, height = 6)

# - save cs plot as rds to combine it with ANOSIM plot in another scripts
saveRDS(cs_plot, paste0("fig/results_classification_strength/", Sys.Date(),"_cs_plot.rds"))


# plots for interpretation not for final paper --------------------------------------

## Parallel Plot across seasons 
data |> 
        mutate(id = paste0(type)) |> 
        ggplot(aes(x = season, y = within_type, fill = id, col = id, group = id)) + 
        geom_point() +
        stat_summary(geom = "line", fun = mean) +  
        theme(legend.position = "none")
        


data |> filter(typology == "ife") |> 
        ggplot(aes(x = between_type, y = type)) + 
        geom_point(aes(col = season)) + 
        stat_summary(geom = "point", fun = mean, col = "black", shape = "|", size = 5) + 
        scale_color_manual(values = pal)
data |> filter(typology == "ife") |> 
        ggplot(aes(x = within_type, y = type)) + 
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

data |>  
        group_by(typology) |> 
        summarize(mean = mean(classification_strength))
