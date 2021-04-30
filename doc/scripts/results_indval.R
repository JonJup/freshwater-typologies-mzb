

indi = readRDS("data/15_indicator_list.rds")
data_genus = readRDS("data/06_sxs_genus.RDS")

taxa_in_data_genus = ncol(data_genus) - 2

rt_names =  paste0("BRT", c(1:5, 8:11, 14:16,18))

# all indicator taxa 
ind_all_taxa_table = indi %>% rbindlist()
## -- assign river type to table 
n_rt = sapply(indi, nrow)
ind_all_taxa_table[, rt := rep(rt_names, times = n_rt)]

ind_all_taxa = ind_all_taxa_table %>% pull(taxon)


# number of unique indicators 
ind_unique_taxa = uniqueN(ind_all_taxa)
## -  how often do they occur 
int_t = table(ind_all_taxa)
int_t2 = table(int_t)

#- once or twice 
ind_ot = sum(int_t < 4)
ind_ot_p = round(ind_ot/ind_unique_taxa * 100 ,0)

## -- max numer of indicatrices 
names(int_t)[which(int_t == max(int_t))]

## -- where do they occur? 
ind_all_taxa_table[taxon == "Sialis", unique(rt)]

n = indi %>% 
        map_int(nrow)

mu_n = n %>% 
        mean() %>% 
        round()
sd_n = n %>% 
        sd() %>% 
        round()

max_n = n %>% 
        max()

max_n_rt = n %>% 
        which.max() %>% 
        rt_names[.]
min_n = n %>% 
        min()

min_n_rt = n %>% 
        which.min() %>% 
        rt_names[.]



join_n = data.table(n_ind = n, 
                    rt = rt_names)

# how many are unique to max_n_rt? 
int_t = table(ind_all_taxa)
max_rt_u = sum(int_t[which(names(int_t) %in% indi[[n %>% which.max]]$taxon)] == 1 )

### --  IndVal Stats --- ###  
mean_indval = ind_all_taxa_table$indval %>% mean %>% round(2)
sd_indval = ind_all_taxa_table$indval %>% sd %>% round(2)

high_ind_genus = indi %>% rbindlist %>% arrange(indval) %>% filter(row_number()==n()) %>% pull(taxon)
high_ind_value = indi %>% rbindlist %>% arrange(indval) %>% filter(row_number()==n()) %>% pull(indval) %>% round(2)

low_ind_genus = ind_all_taxa_table[indval == min(indval), "taxon"] %>% pull
low_ind_value = ind_all_taxa_table[indval == min(indval), "indval"] %>% pull %>% round(2)

## -- most common genera 
data_genus %>% 
        dplyr::select(!c("gr_sample_id", "ls_bd_20")) %>% 
        colSums  %>% 
        sort(decreasing = TRUE) ->
        common_taxa
n_sites = nrow(data_genus)
cmn_n_1 = names(common_taxa)[1]  
cmn_a_1 = common_taxa[1]  
cmn_n_2 = names(common_taxa)[2]  
cmn_a_2 = common_taxa[2]  
cmn_n_3 = names(common_taxa)[3]  
cmn_a_3 = common_taxa[3]  
cmn_n_4 = names(common_taxa)[4]  
cmn_a_4 = common_taxa[4]  
cmn_n_5 = names(common_taxa)[5]  
cmn_a_5 = common_taxa[5]  
               
