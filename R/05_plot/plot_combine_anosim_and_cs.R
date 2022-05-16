# -------------------- ---------------#
# --- Plot: Combine ANOSIM and CS --- # 
# ----------------------------------- #

#  date created: 04-05-22
# last modified: 04-05-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Combine ANOSIM and CS plots

# setup -----------------------------------------------------------------------------
library(ggplot2)
library(rstudioapi)
library(cowplot)


x<-getActiveDocumentContext()
sink(file = paste0("R/05_plot/log_files/anosim_and_cs","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)

an.plot <- readRDS("fig/results_anosim/2022-05-04_anosim_plot.rds")
cs.plot <- readRDS("fig/results_classification_strength/2022-05-04_cs_plot.rds")

cs.plot2 <- cs.plot + theme(legend.position = "none", axis.text.y = element_blank())
an.legend <- get_legend(an.plot)
an.plot2 <- an.plot + theme(legend.position = "none")
combined_plot <- plot_grid(an.plot2, an.legend, cs.plot2, nrow = 1, rel_widths = c(1,0.5,1), labels = c("A", NA, "B"))
ggsave(plot = combined_plot, filename = "fig/test.png", width = 7, height = 3)
