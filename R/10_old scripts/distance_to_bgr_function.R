# ——— Distance to BGR Function ——— #  


distance.to.bgr <- function(x,data){
        i.id      <- data[x,]
        i.bgr     <- bgr |> filter(code == i.id$code)
        out <-
                st_geometry(obj = i.bgr) %>%
                st_cast(to = 'MULTILINESTRING') %>%
                st_distance(y = i.id) |>
                as.numeric()
        return(out)
}