## results traits indval 

data = readRDS("data/indicator_taxa_w_traits.rds")

# which are parasitic? 
data %>% 
        dplyr::filter(feed_parasite != 0)
