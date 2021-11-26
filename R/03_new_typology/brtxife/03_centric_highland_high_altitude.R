### --- BRT X IFE - Centric highland - High altitude --- ### 

#- extract alpine Mediterranean data 
data.med <- data[NAME == type & m_btype20c %in% c("RT16")]

#- subset FEC
fec.med  <-
        fec |>  
        filter(ZHYD %in% data.med$m_zhyd) |> 
        select(OBJECTID, ZHYD)

st_agr(fec.med) <- "constant"

#- subset geo- 
# -- The commented code was used to compute geo.med which is now loaded from the drive as 
# -- computation takes approx. 1 minute. 

geo.med  <-
        fec.med |>
        st_intersection(geo) |>
        select(Litho3, acid_basic)

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
        table(useNA = "ifany")

geo.med |> 
        filter(geology == "unknown" | is.na(geology)) |> 
        pull(acid_basic) |> 
        unique()


geo.med %<>% mutate(geology = ifelse (is.na(geology)|geology=="unknown", acid_basic, geology))
geo.med %<>% mutate(geology = ifelse(geology == "sediments", NA, geology))


#- prevent warnings from intersection 
st_agr(geo.med) = "constant"
st_agr(fec.med) = "constant"

for (i in 1:nrow(fec.med)){
        
        i.geo      <- st_intersection(fec.med[i,], geo.med)
        i.fec.geo  <- st_area(i.geo)/st_area(fec.med[i,]) 
        
        area_calc <-
                ifelse(any(i.geo$geology == "calcareous", na.rm = T), 
                       sum(i.fec.geo[which(i.geo$geology == "calcareous")], na.rm = TRUE),
                       0)
        
        if (area_calc>0.5){
                fec.med[i,"dominant_geology"] <- "calcareous"
                print(i)
                next()  
        }
        area_sil <-
                ifelse(any(i.geo$geology == "siliceous", na.rm = T), 
                       sum(i.fec.geo[which(i.geo$geology == "siliceous")], na.rm = TRUE),
                       0)
        
        if (area_sil>0.5){
                fec.med[i,"dominant_geology"] <- "siliceous"  
                print(i)
                next() 
        }
        if (area_sil > 0.4 & area_calc > 0.4){
                fec.med[i,"dominant_geology"] <- "mixed"
                print(i)
                next()
        }
                
        area_uk <-
                ifelse(any(i.geo$geology == "unknown"),
                       sum(i.fec.geo[which(i.geo$geology == "unknown")], na.rm = TRUE),
                       0)
        if (area_uk > 0.5){
                fec.med[i,"dominant_geology"] <- "unknown"
                print(i)
                next()
        }

        fec.med[i,"dominant_geology"] <- "missing";next()
        print(i)
}

fec.med |> 
        pull(dominant_geology) |> 
        table(useNA = "ifany")

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
        
}

fec.med |> pull(dominant_geology) |> table(useNA = "ifany")

# reassign geology to river segments 
fec.join <- select(fec.med, m_zhyd = ZHYD, dominant_geology) |> st_drop_geometry()

data.med %<>% 
        left_join(fec.join,
                  by = "m_zhyd")

data.med$dominant_geology |> table()

setDT(data.med)
data.med[, uniqueN(m_btype20c), by = "m_zhyd"]   |> pull(V1) |> table()     


data[, dominant_geology := character()]
for (i in 1:nrow(data.med)) data[m_zhyd == data.med$m_zhyd[i], dominant_geology := data.med$dominant_geology[i]]
data$dominant_geology |> table()

data[dominant_geology == "calcareous" & m_btype20c == "RT16" & NAME == type , brt12_illies := paste0("RT9_",type)]
data[dominant_geology == "siliceous"  & m_btype20c == "RT16" & NAME == type , brt12_illies := paste0("RT7_",type)]

data[NAME == type, unique(brt12_illies)]
data[, dominant_geology := NULL]

saveRDS(data, paste0("R/new_typology/brtxife/data/03_data_",type,".rds"))

        