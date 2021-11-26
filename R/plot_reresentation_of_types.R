# ——————————————————————————————————————————————————— #
# ——— Whicht types do we include in which season? ——— # 
# ——————————————————————————————————————————————————— #

# ———————————————————————————————————
#  date created: 21-10-21
# last modified: 21-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Which types do we include in the analyses? This script creates plots that
#       show the distribution of types across seasons (one plot per typology). 
# ———————————————————————————————————

# setup -----------------------------------------------------------------------------
pacman::p_load(
        data.table, 
        ggplot2,
        sf
        )

# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/02_2021-10-14_core_taxa_data_aggregated.rds")
ife <- st_read("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr    <- st_read("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
brt    <- st_read("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")

# create base table  ----------------------------------------------------------------

## unique types 
types_ife <- unique(ife$NAME) |> sort()
types_bgr <- unique(bgr$short_name) |> sort()
types_brt <- append(paste0("RT0", 1:9), paste0("RT", 10:12))

## bgr contains a bogous type "outside" which I remove. 
types_bgr <- types_bgr[-which(types_bgr == "outside")]

## base tables 
base_ife <- matrix(nrow = length(types_ife), ncol = 3, 0) |> as.data.frame()  
base_bgr <- matrix(nrow = length(types_bgr), ncol = 3, 0) |> as.data.frame() 
base_brt <- matrix(nrow = length(types_brt), ncol = 3, 0) |> as.data.frame()  

names(base_ife) <- names(base_bgr) <- names(base_brt) <- c("spring", "summer", "autumn")

base_ife$type <- types_ife
base_bgr$type <- types_bgr
base_brt$type <- types_brt

names(data) <- c("spring", "summer", "autumn")



for (i in c("spring", "summer", "autumn")){
        
        i.data <- data[[i]]
        i.brt <- unique(i.data$brt12)        
        i.bgr <- unique(i.data$bg)        
        i.ife <- unique(i.data$ife)  
        
        base_brt[which(base_brt$type %in% i.brt), i] <- 1
        base_ife[which(base_ife$type %in% i.ife), i] <- 1
        base_bgr[which(base_bgr$type %in% i.bgr), i] <- 1
        
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}


## pivot to longer table where season and presence are one column each 
base_brt <- tidyr::pivot_longer(base_brt, cols = !type, names_to = "season", values_to = "presence")
base_ife <- tidyr::pivot_longer(base_ife, cols = !type, names_to = "season", values_to = "presence")
base_bgr <- tidyr::pivot_longer(base_bgr, cols = !type, names_to = "season", values_to = "presence")

## set presence to factor for better visualization 
base_brt$presence <- factor(base_brt$presence)
base_ife$presence <- factor(base_ife$presence)
base_bgr$presence <- factor(base_bgr$presence)
## set season to factor for proper order of seasons 
base_brt$season <- factor(base_brt$season, levels = c("spring", "summer","autumn"))
base_ife$season <- factor(base_ife$season, levels = c("spring", "summer","autumn"))
base_bgr$season <- factor(base_bgr$season, levels = c("spring", "summer","autumn"))

season_type_plot <- function(x){
        ggplot(x,
               aes(x = season, y = type, fill = presence))+ 
                geom_tile()  +
                theme(
                        panel.background = element_rect(fill = "white"), 
                        axis.ticks.length = unit(0, "line"),
                        axis.text.x = element_text(angle = 90),
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "none"
                ) + 
                labs(
                     x = NULL,
                     y = NULL)
}

season_type_plot(base_brt)
(bgr_season_type_plot <- season_type_plot(base_bgr))
(ife_season_type_plot <- season_type_plot(base_ife))

# save to file ----------------------------------------------------------------------
ggsave(filename = paste0("fig/si_types_per_season/",Sys.Date(), "types_bgr.png"), plot  = bgr_season_type_plot, dpi = 340)
ggsave(filename = paste0("fig/si_types_per_season/",Sys.Date(), "types_ife.png"), plot  = ife_season_type_plot, dpi = 500, width = 4, height = 4)


