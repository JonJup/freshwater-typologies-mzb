# -------------------- #
# --- Plot: ANOSIM --- # 
# -------------------- #

#  date created: 02-05-22
# last modified: 04-05-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Plot ANOSIM

# setup -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(colorspace)
library(wesanderson)
library(stringr)
library(rstudioapi)

x<-getActiveDocumentContext()
sink(file = paste0("R/05_plot/log_files/anosim","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)


# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/2022-05-04_results_anosim.rds")
pal  <- wes_palette("Darjeeling1")[c(3,4,5)]
# plots -----------------------------------------------------------------------------
data2 <- 
        data |> 
        mutate(season = factor(season, levels = c("spring", "summer", "autumn"))) |> 
        mutate(typology = str_to_upper(typology)) |> 
        mutate(typology = factor(typology, levels = c("IFE", "BGR", "BRT")))


shapes = c(21,22,23)
(        
anosim_plot <- 
        ggplot(data2, aes(x = R, y = typology)) + 
                stat_summary(geom = "point", fun = mean, shape = "|", size = 16) + 
                geom_point(aes(shape = season, fill = typology), size = 6) + 
                
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
        scale_shape_manual(values = shapes)
        # scale_x_continuous(
        #         limits = c(0,0.45),
        #         breaks = c(0,0.1,0.2,0.3,0.4),
        #         expand = c(.0,0)
        # )  
)


ggsave(plot = anosim_plot, 
       filename = paste0("fig/results_anosim/",Sys.Date(),"_anosim_plot.png"), width = 5, height = 5)

# - save cs plot as rds to combine it with ANOSIM plot in another scripts
saveRDS(anosim_plot, paste0("fig/results_anosim/", Sys.Date(),"_anosim_plot_review.rds"))

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

