### --- setup combined invertebrates--- ### 


# libraries ---------------------------------------------------------------
pacman::p_load(
        beepr,
        data.table,
        dplyr,
        fuzzySim,
        ggplot2,
        here,
        indicspecies,
        magrittr,
        OpenStreetMap,
        parallelDist,
        purrr,
        sf,
        stringr,
        taxize,
        tmap,
        tmaptools,
        vegan,
        mapview
)


# directories -------------------------------------------------------------
dir = list()
dir$ls = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/"
dir$ccm = here("~/01_Uni/02_getreal/01_wp1/001_stream_network/003_processed_data/Catchment/")
dir$ind = here("~/01_Uni/02_getreal/02_wp2/data/originals_processed/")
dir$crn = here("~/01_Uni/02_getreal/01_wp1/003_corine_land_cover/003_data_processed/")
dir$rs = here("~/01_Uni/02_getreal/02_wp2/R/invertebrates/")
dir$hlp = here("R/helper/")
dir$plt_sm = here("figures/seasonal_map/")
dir$plt_ndms = here("figures/nmds")

# options -----------------------------------------------------------------
tmap_mode("view")

# funcitons ---------------------------------------------------------------
prepare_plot = function(x) {
        plot_data =
                dt_data %>%
                filter(rt == x) %>%
                unique(by = "gr_sample_id") %>%
                st_as_sf()
        
        osm = read_osm(plot_data, ext = 1.1)
        return(list(data = plot_data, osm = osm))
}
plot_list = function(x){
        out =  tm_shape(x$osm) + tm_rgb() + tm_shape(x$data) + tm_dots(
                col = "season",size = .5,
                shape = 21,
                palette = ch_col
        ) +  tm_layout(
                legend.bg.color = "white",
                frame = F,
                legend.frame = T,
                legend.position = c("left", "top")
        )
}
subset_with_sites = function(sites, type){
        plot_bbox <- 
                dt_data %>%
                st_as_sf() %>% 
                filter(
                        rt == type &
                                gr_sample_id %in% sites
                ) %>%
                st_bbox()
        out =  dt_data %>%
                st_as_sf() %>%
                filter(rt == type) %>%
                st_crop(plot_bbox) %>%
                setDT
        
        
}
fill_season = function(x) {
        x[is.na(season) & !is.na(season.x), season := season.x]
        x[is.na(season) & !is.na(season.y), season := season.y]
}
