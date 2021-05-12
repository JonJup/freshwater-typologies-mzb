## -- make table -- ## 

dt_mzb %>% 
        rename(type = group) %>% 
        dplyr::select(taxon, type, B, level) %>% 
        arrange(taxon) %>% 
        mutate(B = round(B,3)) %>% 
        mutate(B2 = as.character(B)) %>% 
        mutate(B2 = case_when(B < 0.2 ~ B2,
                              B >= 0.2 & level == "species" ~ paste0(B2, "*)"),
                              B >= 0.33 & level == "genus" ~ paste0(B2, "*)"),
                              level == "genus" & B < 0.33 ~ B2,
                              level == "fol" & B >= 0.66 ~ paste0(B2, "*"),
                              level == "fol" & B < 0.66 ~ B2)
               )  %>% 
        tidyr::pivot_wider(id_cols = taxon, names_from = "type", values_from = B2) -> 
        # mutate(across(.cols = !taxon,
        #        .fns  = function(x) ifelse(is.na(x),0,x))) %>%
        # relocate(RT1, .before = RT2_3_8_9_10_11) %>% 
        # relocate(RT4_5, .before = RT15_16) %>% 
        # relocate(RT14, .before = RT15_16) -> 
        dt_mzb_table

dt_mzb_table %<>%
        mutate(stat = apply(dt_mzb_table,
                            1,
                            function(x)
                                    any(stringr::str_detect(x, pattern = "\\*")))) %>% 
 
        filter(stat == TRUE) %>% 
        dplyr::select(!stat)


dt_mzb %>% 
        rename(type = group) %>% 
        mutate(type = factor(type)) %>% 
        select(taxon, type) %>% 
        split.data.frame(f = .$type) %>% 
        purrr::map(.f =  ~ select(.x, taxon)) -> 
        dt_mzb_list

dt_mzb %>% 
        rename(type = group) %>% 
        select(taxon,type) %>% 
        arrange(type) -> 
        excel_table

