# ------------------------------ #
### --- Create Plot        --- ###
### --- Cluster Evaluation --- ### 
# ------------------------------ #

# --------------- #
# date:   
#               22.03.21
# files in:  
#               -> 11_sxs_genus_W_bio_typology.rds
#               -> 12_sxs_genus_typology_wo_bio.rds
# files out:  
#               <- 13_class_eval_mzb.rds
# Project: 
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose: 
#               Compute cluster metrics for the different typologies.   
# --------------- #


# load data -------------------------------------------------------------------------
cluster_eval  = readRDS("data/temporal_aggregation/10_class_eval_mzb.rds")

# prepare data ----------------------------------------------------------------------
## --  same name for all null clustering 
cluster_eval %<>% 
        mutate(typology = ifelse(str_detect(typology, "null"), "null", typology)) %>% 
        ## -- drop BRTred
        filter(typology != "brt6")

## -- reshape data 
plot_data <- 
        cluster_eval %>% 
        pivot_longer(cols = !typology) %>% 
        filter(name %in% c(
                "arithmetic",
                "harmonic",
                "max",
                "min",
                "m2",
                "quadratic",
                'ch',
                "ivs",
                "classification_strength"
                )
        ) %>% 
        mutate(typology = case_when(#typology == "brt6" ~ "BRTred",
                                    typology == "brt12" ~ "BRT12", 
                                    typology == "brt20" ~ "BRT20",
                                    typology == "gloric" ~ "GloRiC",
                                    typology == "illies" ~ "Illies",
                                    typology == "eea" ~ "BGR",
                                    typology == "bio" ~ "Bio",
                                    typology == "null" ~ "Random"
                                    )) %>% 
        mutate(typology = factor(typology, 
                                 levels = c("Bio", "BRT12", "BRT20", 
                                            "GloRiC", "Illies", "BGR", "Random")
                                 )
               ) %>% 
        mutate(name = replace(name, name == "ch", "Calinski Harabasz")) %>% 
        mutate(name = replace(name, name == "classification_strength", "Classification Strength")) %>% 
        mutate(name = replace(name, name == "ivs", "Indicator Value score"))  
    


data_plot2 =
        plot_data %>%
        mutate(name = case_when(name == "min" ~ "- infinity",
                                name == "m2" ~ "- 2",
                                name == "harmonic" ~ "- 1",
                                name == "arithmetic" ~ "1",
                                name == "quadratic" ~ "2",
                                name == "max" ~ "infinity")) %>% 
        mutate(name = factor(
                name,
                levels = c("- infinity", "- 2", "- 1", "1", "2", "infinity")
        )) %>%
        filter(name %in% c("- infinity", "- 2", "- 1", "1", "2", "infinity"))

### --- collapse randoms 
data_plot2 %>% filter(typology=="Random") %>% 
        group_by(name) %>% 
        summarize(value = mean(value)) %>% 
        mutate(typology = "Random") -> 
        random_means

# plot 1  ---------------------------------------------------------------------------

plot_data %<>%
    mutate(regional = ifelse(typology %in% c("BGR", "Illies"), "region","reach"))
data_plot2 %<>%
    mutate(regional = ifelse(typology %in% c("BGR", "Illies"), "region","reach"))

plot1_fun <-
        function(x,
                 y,
                 legend = FALSE) {
                
                x2 <-
                        x %>%
                        filter(name == y) %>%
                        filter(!typology %in% c("Bio", "Random"))
                bio <-
                        pull(filter(x, typology == "Bio" &
                                            name == y), value)
                ran <-
                        pull(filter(x, typology == "Random" &
                                            name == y), value)
                ran <- mean(ran)
                
                p <- ggplot(x2, aes(x = typology, y = value)) +
                        geom_point(aes(fill = typology,
                                       col = typology, 
                                       shape = regional),
                                  
                                   size = 2)
                p <- p + geom_hline(aes(lty = "bio", yintercept = bio))
                p <- p + geom_hline(aes(lty = "random", yintercept = ran))
                p <- p +
                    scale_color_brewer(palette = "Dark2", guide = guide_legend()) +
                    scale_fill_brewer(palette = "Dark2", guide = guide_legend()) +
                    scale_linetype_manual(name = "",
                                          values = c(bio = "twodash", random = "dotted")) +
                    scale_shape_manual(values = c(21, 22))
                p <- p + theme(
                        axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks = element_blank(),
                        #legend.position = "top",
                        legend.title = element_blank(),
                        #legend.direction = "horizontal"
                ) +
                        guides(fill = guide_legend(ncol = 1)) +
                        ylab("")
                
                if (!legend)
                        p <- p + theme(legend.position = "none")
                
                p
}

p1_l <-
        plot1_fun(plot_data, "Calinski Harabasz", legend = TRUE) %>% 
        get_legend

p1 <- plot1_fun(plot_data, "Calinski Harabasz")
p2 <- plot1_fun(plot_data, "Classification Strength")
p3 <- plot1_fun(plot_data, "Indicator Value score")
p4 <- plot1_fun(data_plot2, "- infinity")
p5 <- plot1_fun(data_plot2, "1")
p6 <- plot1_fun(data_plot2, "infinity")

plot1 <-
        plot_grid(
                plotlist = list(NULL, NULL,  NULL, NULL, NULL, NULL,
                                NULL, p1  ,  NULL, p2  , NULL, p3  ,
                                NULL, NULL,  NULL, NULL, NULL, NULL,
                                NULL, p4  ,  NULL, p5  , NULL, p6  ,
                                NULL, NULL,  NULL, NULL, NULL, NULL),
                labels = c("", ""  , "", ""      , "",  "", 
                           "", "CH", "","CS"    , "","IVS", 
                           "", ""  , "", ""      , "",  "", 
                           "","min", "","average", "","max",
                           "", ""  , "", ""      , "",  ""
                           ),
                hjust = 0,
                vjust = .3,
                nrow = 5,
                ncol = 6,
                rel_heights = c(.1,1,.1,1,.1),
                rel_widths = c(0.1,1,0.1,1,0.1,1)
        )
plot1 %>% 
        plot_grid(p1_l, ncol = 2, nrow = 1, rel_widths = c(4,1)) -> 
        plot2

# save plot to file  ----------------------------------------------------------------

## -- png 
ggsave(filename = "fig/tempagg/eval_cluster_combined.png", 
       plot = plot2,
       dpi = 600) 
    ## -- eps
ggsave(filename = "fig/eval_cluster_combined.eps", 
       plot = plot2,
       dpi = 600) 


# extract values  -------------------------------------------------------------------

cluster_eval <- rename(cluster_eval, "indval" = "ivs")

asw_b     = filter(cluster_eval, typology == "bio")    %>% pull('arithmetic') %>% round(2)
asw_ill   = filter(cluster_eval, typology == "illies") %>% pull('arithmetic') %>% round(2)
asw_eea   = filter(cluster_eval, typology == "eea")    %>% pull('arithmetic') %>% round(2)
asw_brt12 = filter(cluster_eval, typology == "brt12")  %>% pull('arithmetic') %>% round(2)
asw_brt20 = filter(cluster_eval, typology == "brt20")  %>% pull('arithmetic') %>% round(2)
asw_gl    = filter(cluster_eval, typology == "gloric") %>% pull('arithmetic') %>% round(2)
ch_b      = filter(cluster_eval, typology == "bio")    %>% pull('ch') %>% round(2)
ch_ill    = filter(cluster_eval, typology == "illies") %>% pull('ch') %>% round(2)
ch_eea    = filter(cluster_eval, typology == "eea")    %>% pull('ch') %>% round(2)
ch_brt12  = filter(cluster_eval, typology == "brt12")  %>% pull('ch') %>% round(2)
ch_brt20  = filter(cluster_eval, typology == "brt20")  %>% pull('ch') %>% round(2)
ch_gl     = filter(cluster_eval, typology == "gloric") %>% pull('ch') %>% round(2)
cs_b      = filter(cluster_eval, typology == "bio")    %>% pull('classification_strength') %>% round(2)
cs_ill    = filter(cluster_eval, typology == "illies") %>% pull('classification_strength') %>% round(2)
cs_eea    = filter(cluster_eval, typology == "eea")    %>% pull('classification_strength') %>% round(2)
cs_brt12  = filter(cluster_eval, typology == "brt12")  %>% pull('classification_strength') %>% round(2)
cs_brt20  = filter(cluster_eval, typology == "brt20")  %>% pull('classification_strength') %>% round(2)
cs_gl     = filter(cluster_eval, typology == "gloric") %>% pull('classification_strength') %>% round(2)
iv_b      = filter(cluster_eval, typology == "bio")    %>% pull('indval') %>% round(2)
iv_ill    = filter(cluster_eval, typology == "illies") %>% pull('indval') %>% round(2)
iv_eea    = filter(cluster_eval, typology == "eea")    %>% pull('indval') %>% round(2)
iv_brt12  = filter(cluster_eval, typology == "brt12")  %>% pull('indval') %>% round(2)
iv_brt20  = filter(cluster_eval, typology == "brt20")  %>% pull('indval') %>% round(2)
iv_gl     = filter(cluster_eval, typology == "gloric") %>% pull('indval') %>% round(2)
min.sw_b     = filter(cluster_eval, typology == "bio")    %>% pull('min') %>% round(2)
min.sw_ill   = filter(cluster_eval, typology == "illies") %>% pull('min') %>% round(2)
min.sw_eea   = filter(cluster_eval, typology == "eea")    %>% pull('min') %>% round(2)
min.sw_brt12 = filter(cluster_eval, typology == "brt12")  %>% pull('min') %>% round(2)
min.sw_brt20 = filter(cluster_eval, typology == "brt20")  %>% pull('min') %>% round(2)
min.sw_gl    = filter(cluster_eval, typology == "gloric") %>% pull('min') %>% round(2)
max.sw_b     = filter(cluster_eval, typology == "bio")    %>% pull('max') %>% round(2)
max.sw_ill   = filter(cluster_eval, typology == "illies") %>% pull('max') %>% round(2)
max.sw_eea   = filter(cluster_eval, typology == "eea")    %>% pull('max') %>% round(2)
max.sw_brt12 = filter(cluster_eval, typology == "brt12")  %>% pull('max') %>% round(2)
max.sw_brt20 = filter(cluster_eval, typology == "brt20")  %>% pull('max') %>% round(2)
max.sw_gl    = filter(cluster_eval, typology == "gloric") %>% pull('max') %>% round(2)

         
asw_null =
        filter(cluster_eval, typology == "null") %>%
        rename(focal = 'arithmetic') %>%
        summarise(mean = round(mean(focal), 2),
                  sd   = round(sd(focal), 3))      
min.sw_null =
        filter(cluster_eval, typology == "null") %>%
        rename(focal = 'min') %>%
        summarise(mean = round(mean(focal), 2),
                  sd   = round(sd(focal), 3))      
max.sw_null =
        filter(cluster_eval, typology == "null") %>%
        rename(focal = 'max') %>%
        summarise(mean = round(mean(focal), 2),
                  sd   = round(sd(focal), 3))      
ch_null =
        filter(cluster_eval, typology == "null") %>%
        rename(focal = 'ch') %>%
        summarise(mean = round(mean(focal), 2),
                  sd   = round(sd(focal), 2))      
iv_null =
        filter(cluster_eval, typology == "null") %>%
        rename(focal = 'indval') %>%
        summarise(mean = round(mean(focal), 2),
                  sd   = round(sd(focal), 3))      
cs_null =
        filter(cluster_eval, typology == "null") %>%
        rename(focal = 'classification_strength') %>%
        summarise(mean = round(mean(focal), 2),
                  sd   = round(sd(focal), 2))      


# ranking  ----------------------------------------------------------------

rank_dat = copy(cluster_eval)
id = names(rank_dat)[which(names(rank_dat) %in% 
                   c('ch',
                     "indval",
                     "classification_strength",
                     "arithmetic",
                     "typology",
                     "min",
                     "max")
                 )]
rank_dat = rank_dat[,id,with = F]  
rank_dat %<>% rename(
        cs = classification_strength,
        iv = indval,
        typ = typology,
        asw = arithmetic
)
## -- keep only real typologies 
rank_dat = rank_dat[!typ %in% c("bio", "null")]

## -- 5% of difference between bio and null 
crit_diff = data.table(
        "asw" = (asw_b - abs(asw_null$mean)) * 0.05, 
        "ch"  = (ch_b  - abs(ch_null$mean))  * 0.05, 
        "cs"  = (cs_b  - abs(cs_null$mean))  * 0.05,
        "iv"  = (iv_b  - abs(iv_null$mean))  * 0.05,
        "min"  = (min.sw_b  - abs(min.sw_null$mean))  * 0.05,
        "max"  = (max.sw_b  - abs(max.sw_null$mean))  * 0.05
)

for (i in 1:ncol(crit_diff)){

        ## -- select variable
        lp_var = names(crit_diff)[i]
        lp_var_r = paste0(lp_var, "_r")
        lp_var_c = paste0(lp_var, "_c")

        ## -- arrange data by focal variable
        rank_dat %<>% setorderv(lp_var, order = 1)
        ## -- create new variable with rank of focal variable
        rank_dat [,(lp_var_r) := lapply(.SD, rank), .SDcols = (lp_var)]
        ## -- compare differences to critical difference
        rank_dat[, (lp_var_c) := lapply(.SD, function(x) c(diff(x) > crit_diff[[i]], TRUE)), .SDcols = (lp_var)]
        if (any(!rank_dat[, ..lp_var_c])) {
                
                ## -- which ones are small 
                false_id = which(rank_dat[, ..lp_var_c] == FALSE)
                if (1%in%false_id) false_id = false_id[-which(false_id==1)]
                
                ## -- clumped 
                if (all(diff(false_id) == 1)){
                        lower = min(false_id)
                        upper = max(false_id)
                        lower2 = as.numeric(rank_dat[lower,..lp_var_r])
                        upper2 = as.numeric(rank_dat[upper,..lp_var_r])
                        new_rank = mean(c(upper2,lower2))
                        rank_dat[(lower-1):upper, (lp_var_r) := new_rank]
                        
                } else {
                        ## -- loop along false_id 
                        for(k in false_id){
                                upper = as.numeric(rank_dat[k,..lp_var_r])
                                lower = as.numeric(rank_dat[k - 1,..lp_var_r])
                                new_rank = mean(c(upper,lower))
                                rank_dat[k, (lp_var_r) := new_rank]
                                rank_dat[k-1, (lp_var_r) := new_rank]
                        }
                }
        }  
        rank_dat[,c(lp_var, lp_var_c) := NULL]
}


#rank_dat$iv_r = c(1.5, 1.5, 3, 4, 5.5, 5.5)

rank_dat[, total_points := sum(asw_r, 
                               ch_r, 
                               cs_r, 
                               iv_r,
                               min_r,
                               max_r
                               ), by =  typ]


# old -------------------------------------------------------------------------------

# ss_b      = filter(cluster_eval, typology == "bio")    %>% pull('within.cluster.ss') %>% round(2)
# ss_ill    = filter(cluster_eval, typology == "illies") %>% pull('within.cluster.ss') %>% round(2)
# ss_eea    = filter(cluster_eval, typology == "eea")    %>% pull('within.cluster.ss') %>% round(2)
# ss_brt12  = filter(cluster_eval, typology == "brt12")  %>% pull('within.cluster.ss') %>% round(2)
# ss_brt20  = filter(cluster_eval, typology == "brt20")  %>% pull('within.cluster.ss') %>% round(2)
# ss_gl     = filter(cluster_eval, typology == "gloric") %>% pull('within.cluster.ss') %>% round(2)
# ha_b      = filter(cluster_eval, typology == "bio")    %>% pull('harmonic') %>% round(2)
# ha_ill    = filter(cluster_eval, typology == "illies") %>% pull('harmonic') %>% round(2)
# ha_eea    = filter(cluster_eval, typology == "eea")    %>% pull('harmonic') %>% round(2)
# ha_brt12  = filter(cluster_eval, typology == "brt12")  %>% pull('harmonic') %>% round(2)
# ha_brt20  = filter(cluster_eval, typology == "brt20")  %>% pull('harmonic') %>% round(2)
# ha_gl     = filter(cluster_eval, typology == "gloric") %>% pull('harmonic') %>% round(2)

# is_b      = filter(cluster_eval, typology == "bio")    %>% pull('isamic') %>% round(3)
# is_ill    = filter(cluster_eval, typology == "illies") %>% pull('isamic') %>% round(3)
# is_eea    = filter(cluster_eval, typology == "eea")    %>% pull('isamic') %>% round(3)
# is_brt12  = filter(cluster_eval, typology == "brt12")  %>% pull('isamic') %>% round(3)
# is_brt20  = filter(cluster_eval, typology == "brt20")  %>% pull('isamic') %>% round(3)
# is_gl     = filter(cluster_eval, typology == "gloric") %>% pull('isamic') %>% round(3)

# is_null =
#         filter(cluster_eval, typology == "null") %>%
#         rename(focal = 'isamic') %>%
#         summarise(mean = round(mean(focal), 3),
#                   sd   = round(sd(focal), 3))
# ss_null =
#         filter(cluster_eval, typology == "null") %>%
#         rename(focal = 'within.cluster.ss') %>%
#         summarise(mean = round(mean(focal), 2),
#                   sd   = round(sd(focal), 2))

# ha_null =
#         filter(cluster_eval, typology == "null") %>%
#         rename(focal = 'harmonic') %>%
#         summarise(mean = round(mean(focal), 2),
#                   sd   = round(sd(focal), 2))

# plot 1  ---------------------------------------------------------------------------

# cluster_eval_plot1 =
#         plot_data %>%
#         mutate(name = factor(name),
#                typology = factor(typology)) %>%
#         filter(!name %in% c("arithmetic",
#                             "harmonic",
#                             "max",
#                             "min",
#                             "m2",
#                             "quadratic")) %>%
#         filter(!typology %in% c("Bio", "Random")) %>%  
#         ggplot(aes(x = typology, y = value)) +
#         geom_hline(yintercept = )
# geom_point(aes(fill = typology), shape = 22, size =4) +
#         geom_boxplot(data = filter(
#                 plot_data,
#                 typology == "null" &
#                         !name %in% c("arithmetic",
#                                      "harmonic",
#                                      "max",
#                                      "min",
#                                      "m2",
#                                      "quadratic")
#         )) +
#         facet_wrap(. ~ name, scales = "free") + 
#         scale_color_brewer(palette = "Dark2", guide = guide_legend()) + 
#         scale_fill_brewer(palette = "Dark2", guide = guide_legend()) + 
#         theme(
#                 panel.grid = element_blank(),
#                 axis.title.x = element_blank(),
#                 axis.text.x = element_blank(),
#                 axis.ticks = element_blank(), 
#                 legend.position="top",
#                 legend.title = element_blank(),
#                 legend.direction = "horizontal"
#         ) + 
#         guides(
#                 fill = guide_legend(nrow = 1)      
#         ) + 
#         ylab("")

# plot 2 ----------------------------------------------------------------------------


# cluster_eval_plot2 =
#         data_plot2 %>% 
#         ggplot(aes(x = name, y = value)) +
#         geom_hline(yintercept = 0, lty = 2) + 
#         geom_line(data = data_plot2, aes(col = typology, group = typology), size = 1) +
#         #geom_smooth(data = filter(data_plot2, typology != "null"), aes(col = typology, group = typology), method = "loess") +
#         annotate(geom = "label", 
#                  label = "Biological",
#                  color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1],
#                  x = .5, y = pull(filter(data_plot2,  name == "- infinity" & typology == "Biological"),value)) + 
#         annotate(geom = "label", 
#                  label = "BGR", 
#                  color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[7],
#                  x = .5, y =( pull(filter(data_plot2, name == "- infinity" & typology == "BGR"),value)) - 0.005) + 
#         annotate(geom = "label", 
#                  label = "Illies", 
#                  color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[6],
#                  x = .5, y = (pull(filter(data_plot2, name == "- infinity" & typology == "Illies"),value)) - .01) + 
#         annotate(geom = "label", 
#                  label = "BRT6", 
#                  color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[2],
#                  x = .5, y = (pull(filter(data_plot2, name == "- infinity" & typology == "BRT6"),value)) - .01) + 
#         annotate(geom = "label", 
#                  label = "BRT12", 
#                  color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[3],
#                  x = .5, y = (pull(filter(data_plot2, name == "- infinity" & typology == "BRT12"),value)) + .01) + 
#         annotate(geom = "label", 
#                  label = "BRT20", 
#                  color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[4],
#                  x = .5, y = (pull(filter(data_plot2, name == "- infinity" & typology == "BRT20"),value)) - .01) + 
#         annotate(geom = "label", 
#                  label = "GloRiC", 
#                  color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[5],
#                  x = .5, y = pull(filter(data_plot2, name == "- infinity" & typology == "GloRiC"),value)) + 
#         annotate(geom = "label", 
#                  label = "Null", 
#                  color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[8],
#                  x = .5, y = mean(pull(filter(data_plot2, name == "- infinity" & typology == "Null"),value))) + 
#                                                                            
#         theme(panel.grid = element_blank(),
#               #panel.background = element_blank(), 
#               legend.position = "none") + 
#         coord_cartesian(xlim = c(.5,6)) + 
#         scale_color_brewer(palette = "Dark2") + 
#         scale_fill_brewer(palette = "Dark2") 


# plot 3 ----------------------------------------------------------------------------


# cluster_eval_plot2 = 
#         data_plot2 %>% 
#         mutate(size = ifelse(typology == "Null", .3,1)) %>% 
#         ggplot(aes(x = name, y = value, fill = typology)) +
#         geom_hline(yintercept = 0, lty = 2, alpha = .5) + 
#         geom_line(aes(group = typology,col = typology), lty = 1)  +
#         stat_summary(
#                 fun = mean,
#                 fun.min = min,
#                 fun.max = max,
#                 group = "typology",
#                 geom = "pointrange",
#                 shape = 22,
#                 size = .5
#         ) +
#         scale_color_brewer(palette = "Dark2", guide = guide_legend()) + 
#         scale_fill_brewer(palette = "Dark2", guide = guide_legend()) + 
#         theme(
#                 panel.grid = element_blank(),
#                 legend.position="none"
#         ) + 
#         ylab("silhouette width") + 
#         xlab("degree of mean")

