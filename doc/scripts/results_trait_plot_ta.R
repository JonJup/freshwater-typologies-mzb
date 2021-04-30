# ------------------------------- #
### --- PLOTS               --- ###  
### --- TRAITS              --- ###  
### --- TYPICAL ASSEMBLAGES --- ### 
# ------------------------------- #


# read in data ----------------------------------------------------------------------
save_list = readRDS("data/17_typical_assemblages_w_traits.rds")
data = save_list$data %>% as.data.frame()
data$rt = add_rt = paste0(c("RT"),
                          c("1", "2",  "3", "4_5", "8", "9", "10_11", 
                            "14", "15", "16", "18"))
gd = save_list$gd
traits = save_list$traits
fc = save_list$fc
fc2 <-
        fc %>%
        map_df(.f = ~ as.numeric(.x)) %>%
        mutate(rt = factor(add_rt)) %>%
        pivot_longer(col = !rt) %>%
        mutate(trait = str_remove(name, "_.*"),
               modality = str_remove(name, ".*_")) %>% 
        filter(trait != "dev") %>% 
        mutate(rt = str_remove_all(rt, "RT")) %>% 
        group_by(trait, rt) %>% 
        mutate(total = sum(value)) %>% 
        mutate(adj_fctr = 1/total) %>% 
        mutate(value = value * adj_fctr)

# reverse order 
#fc2$rt = factor(fc2$rt, levels = c(18,16:14, 11:8, 5:1))
fc2$rt = factor(fc2$rt,
                levels = c(
                        "1",
                        "2",
                        "3",
                        "4_5",
                        "8",
                        "9",
                        "10_11",
                        "14",
                        "15",
                        "16",
                        "18"
                )
)


#fc2 %<>% arrange(desc(rt))


fc2_plot = function(x){
        fc2 %>% 
                filter(trait == x) %>% 
                mutate(value = ifelse(value ==0,NA,value))    %>%     
                ggplot(aes(x = rt, y = value, fill = modality)) +
                geom_col(position = "stack") + 
                geom_text(position = position_stack(vjust = 0.5), aes(label = round(value,2)), na.rm = T) + 
                #scale_fill_viridis_d(option = "viridis") + 
                scale_fill_brewer(palette = "Dark2") + 
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      legend.position = "top")
}

# plot all 
# map(.x = unique(fc2$trait),
#     .f = fc2_plot) %>%
#         plot_grid(plotlist = .)
# plot single 
trait_plot1 = map(.x = unique(fc2$trait),
                  .f = fc2_plot)

trait_plot1[[7]]    
