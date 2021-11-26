### --- BRT X BGR - Alpine - MEDITERRANEAN --- ### 

#- extract alpine Mediterranean data 
data.med <-
        data |>
        filter(short_name == "alpine" & m_btype12 %in% c("RT11", "RT12"))

st_agr(geo) <-"constant"

#- subset FEC
fec.med  <- 
        fec |> 
        filter(ZHYD %in% data.med$m_zhyd) |> 
        select(OBJECTID, ZHYD)
st_agr(fec.med) <- "constant"

#- subset geo- 
# -- The commented code was used to compute geo.med which is now loaded from the drive as 
# -- computation takes approx. 1 minute. 

# geo.med  <- 
#         fec.med |> 
#         st_intersection(geo) |> 
#         select(Litho3, acid_basic)
#saveRDS(geo.med, "R/new_typology/auxilliary/alpine_geo_med.RDS")
geo.med <- readRDS("R/new_typology/auxilliary/alpine_geo_med.RDS")

#-which lithologies are missing assigned acidity
unique(geo.med$Litho3) |> 
        setdiff(geology.vector$calcareous) |> 
        setdiff(geology.vector$siliceous) |> 
        sort()

geo.med[which(geo.med$Litho3 %in% geology.vector$calcareous), "geology"] <- "calcareous"
geo.med[which(geo.med$Litho3 %in% geology.vector$siliceous), "geology"]  <- "siliceous"
geo.med[which(geo.med$Litho3 %in% geology.vector$unknown), "geology"]    <- "unknown"

geo.med |> 
        pull(geology) |> 
        table()

geo.med |> 
        filter(geology == "unknown") |> 
        pull(acid_basic) |> 
        unique()
setDT(geo.med)

geo.med[geology == "unknown" & acid_basic != "sediments", geology := acid_basic]
geo.med |> filter(geology == "unknown") |> pull(Litho3) |> unique()
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
                progress_bar(i, nrow(fec.med))
                next()  
        }
        area_sil <-
                ifelse(any(i.geo$geology == "siliceous"), 
                       sum(i.fec.geo[which(i.geo$geology == "siliceous")], na.rm = TRUE),
                       0)
        
        if (area_sil>0.5){
                fec.med[i,"dominant_geology"] <- "siliceous"  
                progress_bar(i, nrow(fec.med))
                next() 
        }
        if (area_sil > 0.4 & area_calc > 0.4){
                fec.med[i,"dominant_geology"] <- "mixed"
                progress_bar(i, nrow(fec.med))
                next()
        }
                
        area_uk <-
                ifelse(any(i.geo$geology == "unknown"),
                       sum(i.fec.geo[which(i.geo$geology == "unknown")], na.rm = TRUE),
                       0)
        if (area_uk > 0.5){
                fec.med[i,"dominant_geology"] <- "unknown"
                progress_bar(i, nrow(fec.med))
                next()
        }

        fec.med[i,"dominant_geology"] <- "missing";next()
        progress_bar(i, nrow(fec.med))       
}

fec.med |> 
        pull(dominant_geology) |> 
        table(useNA = "always")

for (i in 1:nrow(fec.med)){

        if (!is.na(fec.med$dominant_geology[i]) & fec.med$dominant_geology[i] != "unknown" & fec.med$dominant_geology[i] != "missing") next()
        
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
        progress_bar(i, nrow(fec.med))
}

fec.med |> pull(dominant_geology) |> table(useNA = "always")

# reassign geology to river segments 
fec.join <- select(fec.med, m_zhyd = ZHYD, dominant_geology) |> st_drop_geometry()

data.med %<>% 
        left_join(fec.join,
                  by = "m_zhyd")

data.med$dominant_geology |> table()

setDT(data.med)
data.med[, uniqueN(m_btype20c), by = "m_zhyd"]   |> pull(V1) |> table()     

setDT(data)
data[, dominant_geology := character()]
for (i in 1:nrow(data.med)) data[m_zhyd == data.med$m_zhyd[i], dominant_geology := data.med$dominant_geology[i]]
data$dominant_geology |> table()

data[dominant_geology == "calcareous" & m_btype20c == "RT17" & short_name == "alpine" , brt_bgr := "RT02_alpine"]
data[dominant_geology == "siliceous"  & m_btype20c == "RT17" & short_name == "alpine" , brt_bgr := "RT04_alpine"]
data[dominant_geology == "calcareous" & m_btype20c == "RT18" & short_name == "alpine" , brt_bgr := "RT06_alpine"]
data[dominant_geology == "siliceous"  & m_btype20c == "RT18" & short_name == "alpine" , brt_bgr := "RT08_alpine"]
data[dominant_geology == "calcareous" & m_btype20c == "RT19" & short_name == "alpine" , brt_bgr := "RT07_alpine"]
data[dominant_geology == "siliceous"  & m_btype20c == "RT19" & short_name == "alpine" , brt_bgr := "RT09_alpine"]
data[dominant_geology == "calcareous" & m_btype20c == "RT20" & short_name == "alpine" , brt_bgr := "RT07_alpine"]
data[dominant_geology == "siliceous"  & m_btype20c == "RT20" & short_name == "alpine" , brt_bgr := "RT09_alpine"]
data[, dominant_geology := NULL]
data <- st_as_sf(data)

#- check 
data |> 
        filter(short_name == "alpine") |> 
        pull(brt_bgr) |> 
        unique()

saveRDS(data, "R/new_typology/auxilliary/data_after_alpine.RDS")
