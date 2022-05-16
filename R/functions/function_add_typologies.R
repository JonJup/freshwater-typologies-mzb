# Purpose: add typologies to data 
# date created: 21-09-21
# date last modified: 21-09-21

add_typologies <- function(data){
        print("start")
        sites <- 
                unique(data, by = "site_id") |> 
                sf::st_as_sf(coords = c("x.coord", "y.coord"), crs = data4$EPSG[1]) |> 
                dplyr::select(site_id) |> 
                sf::st_transform(crs = sf::st_crs(typologies))
        print("sites")
        nn <- sf::st_nearest_feature(sites, typologies)
        nn <- typologies[nn,]
        print("nn")
        distances <- sf::st_distance(sites, y = nn, by_element = TRUE)
        
        sites <- dplyr::mutate(sites, 
                        distance = as.numeric(distances), 
                          brt12    = nn$brt12,
                          ife      = nn$ife,
                          bgr      = nn$bgr,
                          brtXife  = nn$brtXife,
                          brtXbgr  = nn$brtXbgr,
                          least.impacted = nn$least.impacted
        ) 
        sites <- sf::st_drop_geometry(sites)
        
        #- join sites with data 
        data.out <- dplyr::left_join(data, 
                                     sites, 
                                     by = "site_id")
        data.table::setDT(data.out)
        return(data.out)
}
