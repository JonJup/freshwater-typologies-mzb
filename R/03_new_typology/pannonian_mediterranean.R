### --- BRT X BGR - Pannonian - MEDITERRANEAN --- ### 

#- extract Pannonian Mediterranean data 
data.med <-
        data |>
        filter(short_name == "pannonian" & m_btype12 %in% c("RT11", "RT12"))

mapview(data.med)

st_agr(geo) <-"constant"
#- subset fec, geo and soil
fec.med  <-
        fec |> 
        filter(ZHYD %in% data.med$m_zhyd) |> 
        select(OBJECTID, ZHYD)
st_agr(fec.med) <- "constant"

geo.med  <-
        fec.med |>
        st_intersection(geo) |>
        select(Litho3, acid_basic)
unique(geo.med$Litho3) |> 
        setdiff(geology.vector$calcareous) |> 
        setdiff(geology.vector$siliceous) |> 
        sort()

geo.med[which(geo.med$Litho3 %in% geology.vector$calcareous), "geology"] <- "calcareous"
geo.med[which(geo.med$Litho3 %in% geology.vector$siliceous), "geology"] <- "siliceous"
geo.med[which(geo.med$Litho3 %in% geology.vector$unknown), "geology"] <- "unknown"

geo.med |> pull(geology) |> table()
geo.med |> filter(geology == "unknown") |> pull(acid_basic) |> unique()
setDT(geo.med)
geo.med[geology == "unknown" & acid_basic != "sediments", geology := acid_basic]
geo.med[is.na(geology) & acid_basic != "sediments", geology := acid_basic]
# geo.med |> filter(geology == "unknown") |> pull(Litho3) |> unique()
geo.med %<>% st_as_sf()

#- prevent warnings from intersection 
st_agr(geo.med) = "constant"
st_agr(fec.med) = "constant"

for (i in 1:nrow(fec.med)){
        
        i.geo      <- st_intersection(fec.med[i,], geo.med)
        i.fec.geo  <- st_area(i.geo)/st_area(fec.med[i,]) 
        
        area_calc <-
                ifelse(any(i.geo$geology == "calcareous"), 
                       sum(i.fec.geo[which(i.geo$geology == "calcareous")], na.rm = TRUE),
                       0)
        
        if (area_calc>0.5){
                fec.med[i,"dominant_geology"] <- "calcareous"
                print("calcareous")
                next()  
        }
        area_sil <-
                ifelse(any(i.geo$geology == "siliceous"), 
                       sum(i.fec.geo[which(i.geo$geology == "siliceous")], na.rm = TRUE),
                       0)
        
        if (area_sil>0.5){
                fec.med[i,"dominant_geology"] <- "siliceous"
                print("siliceous")
                next() 
        }
        if (area_sil > 0.4 & area_calc > 0.4){
                fec.med[i,"dominant_geology"] <- "mixed"
                print("mixed")
                next()}
        area_uk <-
                ifelse(any(i.geo$geology == "unknown"),
                       sum(i.fec.geo[which(i.geo$geology == "unknown")], na.rm = TRUE),
                       0)
        if (area_uk > 0.5){
                fec.med[i,"dominant_geology"] <- "unknown"
                print("unknown")
                next()
        }
        fec.med[i,"dominant_geology"] <- "missing";print("missing");next()
}

fec.med |> pull(dominant_geology) |> table(useNA = "always")

#fec.med |> filter(is.na(dominant_geology)) |> mapview()

for (i in 1:nrow(fec.med)){
        
        if (!is.na(fec.med$dominant_geology[i])) next()
        i.geo      <- st_intersection(fec.med[i,], geo.med)
        i.fec.geo  <- st_area(i.geo)/st_area(fec.med[i,]) 
        area_calc <-
                ifelse(any(i.geo$geology == "calcareous"), 
                       sum(i.fec.geo[which(i.geo$geology == "calcareous")], na.rm = TRUE),
                       0)
        area_sil <-
                ifelse(any(i.geo$geology == "siliceous"), 
                       sum(i.fec.geo[which(i.geo$geology == "siliceous")], na.rm = TRUE),
                       0)
        if (any(c(area_calc, area_sil) != 0)) {
                max.id <- which.max(c(area_calc, area_sil))
                fec.med[i,"dominant_geology"] <- ifelse(max.id == 1, "calcareous", "siliceous")
        }
}

fec.med |> pull(dominant_geology) |> table(useNA = "always")
fec.med |> mapview(zcol = "dominant_geology")

setDT(fec.med)
fec.med[ZHYD == "F020016553", dominant_geology := "siliceous"]
fec.med[ZHYD %in% c("F020009042", "F020009266"), dominant_geology := "calcareous"]
fec.med[dominant_geology == "unknown", dominant_geology := "calcareous"]
fec.med %<>% st_as_sf()
# reassign geology to river segments 
fec.join <- select(fec.med, m_zhyd = ZHYD, dominant_geology) |> st_drop_geometry()
data.med %<>% 
        left_join(fec.join,
                  by = "m_zhyd")
setDT(data)
data[, dominant_geology := character()]
for (i in 1:nrow(data.med)) data[m_zhyd == data.med$m_zhyd[i], dominant_geology := data.med$dominant_geology[i]]
data[dominant_geology == "calcareous" & m_btype20c == "RT17" & short_name == "pannonian" , brt_bgr := "RT02_pannonian"]
data[dominant_geology == "siliceous"  & m_btype20c == "RT17" & short_name == "pannonian" , brt_bgr := "RT04_pannonian"]
data[dominant_geology == "calcareous" & m_btype20c == "RT18" & short_name == "pannonian" , brt_bgr := "RT06_pannonian"]
data[dominant_geology == "siliceous"  & m_btype20c == "RT18" & short_name == "pannonian" , brt_bgr := "RT08_pannonian"]
data[dominant_geology == "calcareous" & m_btype20c == "RT19" & short_name == "pannonian" , brt_bgr := "RT07_pannonian"]
data[dominant_geology == "siliceous"  & m_btype20c == "RT19" & short_name == "pannonian" , brt_bgr := "RT09_pannonian"]
data[dominant_geology == "calcareous" & m_btype20c == "RT20" & short_name == "pannonian" , brt_bgr := "RT07_pannonian"]
data[dominant_geology == "siliceous"  & m_btype20c == "RT20" & short_name == "pannonian" , brt_bgr := "RT09_pannonian"]

data[, dominant_geology := NULL]
data <- st_as_sf(data)

data |> 
        filter(short_name == "pannonian") |> 
        pull(brt_bgr) |> 
        unique()


saveRDS(data, "R/new_typology/auxilliary/data_after_pannonia.rds")
