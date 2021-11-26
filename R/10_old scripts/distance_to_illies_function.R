# ——— Distance to Illies Function ——— #  


distance.to.illies <- function(x,data){
        i.id      <- data[x,]
        i.ill     <- illies |> filter(NAME == i.id$NAME)
        out <-
                st_geometry(obj = i.ill) %>%
                st_cast(to = 'LINESTRING') %>%
                st_distance(y = i.id) |>
                as.numeric()
        return(out)
}