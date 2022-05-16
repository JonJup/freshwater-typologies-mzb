# ———————————————————————— #
# ——— Explore: IndVal ——————— # 
# ———————————————————————— #

# ———————————————————————————————————
#  date created: 20-10-21
# last modified: 20-10-21
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Explore the results of the indval analysis 
# ———————————————————————————————————


# setup -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
# load data -------------------------------------------------------------------------
data <- readRDS("data/02_combined_data/2021_11_19_results_indval_data.rds")
pval <- readRDS("data/02_combined_data/2021_11_19_results_indval_pvalue.rds")


# highest and lowest values ---------------------------------------------------------

## as table 
median_stat <-
        data |>
        filter(id == 0) |>
        group_by(season, typology, least.impaired) |>
        summarise(median_stat = median(stat)) |>
        arrange(median_stat)
median_stat        
# effect of impairment --------------------------------------------------------------
ggplot(median_stat, aes(x = least.impaired, y = median_stat, fill = typology, col = typology)) + 
        geom_line(aes(group = typology)) + 
        geom_point(shape = 21, size = 4, col = "black") + 
        facet_wrap(.~season)

# number of indicators  -------------------------------------------------------------
n.data <- 
        data |> 
        #filter(id == 0) |> 
        group_by(season, typology, least.impaired,id) |> 
        count()
## overview 
n.data |> 
        filter(id == 0) |> 
        arrange(n)
## mean 
n.data |> 
        filter(id == 0) |> 
        ungroup() |> 
        summarise(mean_stat = mean(n), 
                  sd_stat   = sd(n))
## mean permutations 
n.data |> 
        filter(id != 0) |> 
        ungroup() |> 
        summarise(mean_stat = mean(n), 
                  sd_stat   = sd(n))
## difference impaired         
n.data |> 
        mutate(li2 = case_when(least.impaired == TRUE ~ "li", least.impaired == FALSE ~ "i")) |> 
        filter(id == 0) |> 
        select(!id) |> 
        tidyr::pivot_wider(id_cols = c("season", "typology"), names_from = li2, values_from = n) |> 
        mutate(red_imp = i-li) |> 
        mutate(perc_increase = red_imp/li * 100) |> 
        ungroup() |> 
        summarise (mean_stat = mean(perc_increase), 
                   sd_stat = sd(perc_increase))
