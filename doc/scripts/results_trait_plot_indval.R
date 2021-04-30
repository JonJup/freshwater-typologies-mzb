### results trait plot indval 

save_list = readRDS(file.path(dir$dt, "save_list_indval_traits.rds"))
data = save_list$data
gd = save_list$gd
traits = save_list$traits
fc = save_list$fc
names(data)[which(names(data) == "group")] = "rt"

## --  fit and visualize a PCoA -- ## 
# pcoafit = ape::pcoa(gd) %>% 
#         proj_pcoa("feed") %>%
#         plot_pcoa 
# pcoafit
add_rt = paste0(
        c("RT"),
        c(1:5, 8:11, 14:16, 18))

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
fc2$rt = factor(fc2$rt, levels = c(1:5, 8:11, 14:16, 18))


#fc2 %<>% arrange(desc(rt))

pal = wes_palette("Zissou1", type = "continuous", n = 6)
pal = pal[c(1,4,3,2,5,6)]
fc2_plot = function(x){
       
        fc2 %>% 
                filter(trait == x) %>% 
                mutate(value = ifelse(value ==0,NA,value))    %>%     
                ggplot(aes(x = rt, y = value, fill = modality)) +
                geom_col(position = "stack") + 
                #geom_text(position = position_stack(vjust = 0.5), aes(label = round(value,2)), na.rm = T) + 
                #scale_fill_viridis_d(option = "viridis") + 
               # scale_fill_brewer(palette = "Dark2") + 
                scale_fill_manual(values = pal) + 
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      legend.position = "top", 
                      axis.text.y = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      panel.background = element_blank()) 
}

# plot all 
# map(.x = unique(fc2$trait),
#     .f = fc2_plot) %>%
#         plot_grid(plotlist = .)
# plot single 
trait_plot1 = map(.x = unique(fc2$trait),
                  .f = fc2_plot)
trait_plot1[[2]]   
