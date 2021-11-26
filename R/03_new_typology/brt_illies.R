# ———————— #
# Purpose: # 
# ———————— #

# In this script, I aggregate the rare types of the broad river types X Illies Freshwater
# Ecoregions combination.

# ————— #
# Date: # 
# ————— #

# created: 20.09.21
# last modified: 21.09.21
  

# SETUP -----------------------------------------------------------------------------

#- libraries 
pacman::p_load(sf, magrittr, dplyr, tmap, stringr, ggplot2, data.table, mapview)
#- package options 
tmap_mode("view")
#- auxilliary scripts 
source("R/new_typology/auxilliary/geology_vector.R")
source("~/my documents/R/progress_bar.R")


#- Function used to assign all reaches within a bbox defined by a collection points (id)
#- to another type (type)
reassign <- function (id,old_type, new_type){
        current.data           <- 
                data |> 
                st_as_sf() |> 
                filter(m_zhyd %in% c(id))
        current.window         <- 
                st_bbox(current.data) |> 
                st_as_sfc()
        
        st_agr(current.data)   <-"constant"
        selection.river <- 
                st_crop(st_as_sf(data), current.window) |> 
                pull(m_zhyd)
        x <- 
                data |> 
                setDT() |> 
                {\(x) x[m_zhyd %in% selection.river, 
                        brt12_illies := str_replace(brt12_illies, 
                                                    pattern = old_type, 
                                                replacement = new_type)]}() 
        return(x)
}


# LOAD DATA -------------------------------------------------------------------------

data    <- readRDS("E://Arbeit/Data/broad_river_types/with_illies/brt_with_illies.rds")
illies  <- st_read("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
geo     <- st_read("E://Arbeit/Data/IHME1500_v11/ihme_1500_litho4changed.shp")
fec     <- st_read("E://Arbeit/Data/ecrins/EcrFEC.sqlite")
brt_bgr <- readRDS("E://Arbeit/Data/broad_river_types/with_bgr/2021_09_17_bgr_and_brt.rds")

# PREPARE DATA ----------------------------------------------------------------------

data %<>% st_transform(crs = st_crs(fec))
geo  %<>% st_transform(crs = st_crs(fec))
st_agr(data) <- "constant"
st_agr(geo)  <- "constant"

setDT(data)

data <- data[!is.na(m_btype12)]

# Alps ------------------------------------------------------------------------------
type <- "Alps"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
# - fix Mediterranean types  
source("R/new_typology/brtxife/01_alps_mediterranean.R")
# --> RT4 and 5 to mid-altitude 
data[brt12_illies %in% c("RT4_Alps"), brt12_illies := "RT8_Alps"]
data[brt12_illies %in% c("RT5_Alps"), brt12_illies := "RT9_Alps"]

# --> RT11, RT2, RT3, RT12 
# --> RT2 and 3 to mid-altitude 
data[brt12_illies %in% c("RT2_Alps"), brt12_illies := "RT6_Alps"]
data[brt12_illies %in% c("RT3_Alps"), brt12_illies := "RT7_Alps"]

# Baltic province ------------------------------------------------------------------------------  
type <- "Baltic province"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
# - fix Mediterranean types  
source("R/new_typology/brtxife/02_blatic_mediterranean.R")
# --> RT12, RT6, RT9
data[brt12_illies == "RT6_Baltic province", brt12_illies := "RT2_Baltic province"]
data[brt12_illies == "RT7_Baltic province", brt12_illies := "RT3_Baltic province"]
data[brt12_illies == "RT9_Baltic province", brt12_illies := "RT5_Baltic province"]


# Borealic uplands ------------------------------------------------------------------------------ 

#- The Tundra ecoregion only covers a marginal fraction of the broad river types. - We
#- will resolve it and add its western parts to the borealic uplands and the eastern parts
#- to the Fennoscandian shield

mapview(st_as_sf(data[NAME == "Tundra"]), zcol = "brt12_illies") + mapview(illies)

data <- reassign(id = c("W3I5003603", "W3I5004302", "W3I5005308", "H020089346"),
                 old_type = "Tundra",
                 new_type = "Borealic uplands")
data <- reassign(id = c("W3I5100301", "H020090311", "H020092425"),
                 old_type = "Tundra",
                 new_type = "Fenno-scandian shield")


type = "Borealic uplands"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#-> RT2,6,7,3
data[brt12_illies == "RT2_Borealic uplands", brt12_illies := "RT4_Borealic uplands"]
data[brt12_illies == "RT3_Borealic uplands", brt12_illies := "RT5_Borealic uplands"]
data[brt12_illies == "RT6_Borealic uplands", brt12_illies := "RT8_Borealic uplands"]
data[brt12_illies == "RT7_Borealic uplands", brt12_illies := "RT9_Borealic uplands"]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]


# Central highlands ------------------------------------------------------------------------------   
type = "Central highlands"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
# - fix highland types 
source("R/new_typology/brtxife/03_centric_highland_high_altitude.R")
#-> RT10, 3, 5
data[brt12_illies == "RT3_Central highlands", brt12_illies := "RT2_Central highlands"]
data[brt12_illies == "RT5_Central highlands", brt12_illies := "RT4_Central highlands"]
#-> fix highland types 
data[m_btype20c == "RT14" & NAME == "Central highlands", brt12_illies := "RT9_Central highlands"]
data[m_btype20c == "RT15" & NAME == "Central highlands", brt12_illies := "RT7_Central highlands"]

# Central plains ------------------------------------------------------------------------------        
type = "Central plains"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#-> RT 6,7,8,9
data[brt12_illies == paste0("RT6_", type), brt12_illies := paste0("RT2_", type)]
data[brt12_illies == paste0("RT7_", type), brt12_illies := paste0("RT3_", type)]
data[brt12_illies == paste0("RT8_", type), brt12_illies := paste0("RT4_", type)]
data[brt12_illies == paste0("RT9_", type), brt12_illies := paste0("RT5_", type)]


# Dinaric western Balkan ------------------------------------------------------------------------------     
type = "Dinaric western Balkan"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#-> RT 5,3,4,2
data[brt12_illies == paste0("RT2_", type), brt12_illies := paste0("RT6_", type)]
data[brt12_illies == paste0("RT3_", type), brt12_illies := paste0("RT7_", type)]
data[brt12_illies == paste0("RT4_", type), brt12_illies := paste0("RT8_", type)]
data[brt12_illies == paste0("RT5_", type), brt12_illies := paste0("RT9_", type)]

# Eastern Balkan ------------------------------------------------------------------------------      
type = "Eastern Balkan"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#-> RT3 
data[brt12_illies == paste0("RT3_", type), brt12_illies := paste0("RT2_", type)]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]

# Eastern plains ------------------------------------------------------------------------------  
type = "Eastern plains"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
# - fix mediterranean
source("R/new_typology/brtxife/04_eastern_plains_mediterranean.R")
#-> all mid-altitude 
data[brt12_illies == paste0("RT6_", type), brt12_illies := paste0("RT2_", type)]
data[brt12_illies == paste0("RT7_", type), brt12_illies := paste0("RT3_", type)]
data[brt12_illies == paste0("RT8_", type), brt12_illies := paste0("RT4_", type)]
data[brt12_illies == paste0("RT9_", type), brt12_illies := paste0("RT5_", type)]

# England ------------------------------------------------------------------------------   
type = "England"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#-> RT6, 7
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#- combine all mid-altitude types 
data[brt12_illies %in% paste0("RT",6:8, "_", type), brt12_illies := paste0("RT9_", type)]


# Fenno-scandian shield ------------------------------------------------------------------------------  
type = "Fenno-scandian shield"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#- the calcareous types
data[brt12_illies == paste0("RT6_", type), brt12_illies := paste0("RT8_", type)]
data[brt12_illies == paste0("RT7_", type), brt12_illies := paste0("RT9_", type)]
data[brt12_illies == paste0("RT2_", type), brt12_illies := paste0("RT4_", type)]
data[brt12_illies == paste0("RT3_", type), brt12_illies := paste0("RT5_", type)]

# Hellenic western Balkan ------------------------------------------------------------------------------  
type = "Hellenic western Balkan"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]

data[brt12_illies %in% paste0("RT", c(2,3,6), "_", type), brt12_illies := paste0("RT7_", type)]
data[brt12_illies %in% paste0("RT", c(4,5,8), "_", type), brt12_illies := paste0("RT9_", type)]
mapview(st_as_sf(data[NAME == type]), zcol = "brt12_illies")

# Hungarian lowlands ------------------------------------------------------------------------------  
type = "Hungarian lowlands"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
# - fix Mediterranean types
source("R/new_typology/brtxife/05_hungarian_lowlands_mediterranean.R")

data[brt12_illies == paste0("RT6_", type), brt12_illies := paste0("RT2_", type)]
data[brt12_illies == paste0("RT7_", type), brt12_illies := paste0("RT3_", type)]
data[brt12_illies == paste0("RT8_", type), brt12_illies := paste0("RT4_", type)]
data[brt12_illies == paste0("RT9_", type), brt12_illies := paste0("RT5_", type)]

# Ibero-Macaronesian region ------------------------------------------------------------------------------  
type = "Ibero-Macaronesian region"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
data[brt12_illies %in% paste0("RT", 2:9, "_",type), brt12_illies := "RT11_Ibero-Macaronesian region"]

# Iceland ------------------------------------------------------------------------------              
type = "Iceland"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]

# Ireland and Northern Ireland ------------------------------------------------------------------------------
type = "Ireland and Northern Ireland"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#-> RT7, 8, 9
data[brt12_illies == paste0("RT7_", type), brt12_illies := paste0("RT3_", type)]
data[brt12_illies == paste0("RT8_", type), brt12_illies := paste0("RT4_", type)]
data[brt12_illies == paste0("RT9_", type), brt12_illies := paste0("RT5_", type)]

# Italy and Corsica ------------------------------------------------------------------------------    
type = "Italy and Corsica"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#-> RT5,4,9,3,8
data[brt12_illies %in% paste0("RT", 2:9, "_",type), brt12_illies := "RT11_Italy and Corsica"]

# Pontic province ------------------------------------------------------------------------------      
type = "Pontic province"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#- RT9,8,4,5
data[brt12_illies == paste0("RT4_", type), brt12_illies := paste0("RT2_", type)]
data[brt12_illies == paste0("RT5_", type), brt12_illies := paste0("RT3_", type)]
data[brt12_illies == paste0("RT8_", type), brt12_illies := paste0("RT6_", type)]
data[brt12_illies == paste0("RT9_", type), brt12_illies := paste0("RT7_", type)]


# Pyrenees ------------------------------------------------------------------------------      
type = "Pyrenees"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
data[brt12_illies %in% paste0("RT", c(6,7,9,11,12), "_",type), brt12_illies := "RT7_Pyreenes"]


# Taiga ------------------------------------------------------------------------------   
type = "Taiga"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#-> Rt9,6,7

data[brt12_illies == paste0("RT6_", type), brt12_illies := paste0("RT2_", type)]
data[brt12_illies == paste0("RT7_", type), brt12_illies := paste0("RT3_", type)]
data[brt12_illies == paste0("RT8_", type), brt12_illies := paste0("RT4_", type)]
data[brt12_illies == paste0("RT9_", type), brt12_illies := paste0("RT5_", type)]

# The Carpathiens ------------------------------------------------------------------------------
type = "The Carpathiens"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#-> RT4,5,3,2,12,
# - mediterranean types
source("R/new_typology/brtxife/06_caparthiens_mediterranean.R")

data[brt12_illies == paste0("RT2_", type), brt12_illies := paste0("RT6_", type)]
data[brt12_illies == paste0("RT3_", type), brt12_illies := paste0("RT7_", type)]
data[brt12_illies == paste0("RT4_", type), brt12_illies := paste0("RT8_", type)]
data[brt12_illies == paste0("RT5_", type), brt12_illies := paste0("RT9_", type)]



# The Caucasus  ------------------------------------------------------------------------------   
# -- remove Caucasus from data 

type = "The Caucasus"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]

data <- data[NAME != type]


# Western highlands ------------------------------------------------------------------------------
type = "Western highlands"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
#- Mediterranean types 
source("R/new_typology/brtxife/07_western_highlands_mediterranean.R")

#mapview(st_as_sf(data[NAME == type]), zcol = "brt12_illies")

data[brt12_illies == paste0("RT2_",type), brt12_illies := paste0("RT6_",type)]
data[brt12_illies == paste0("RT3_",type), brt12_illies := paste0("RT7_",type)]
data[brt12_illies == paste0("RT4_",type), brt12_illies := paste0("RT8_",type)]
data[brt12_illies == paste0("RT5_",type), brt12_illies := paste0("RT9_",type)]

data[NAME == type & m_btype20c == "RT14", brt12_illies := paste0("RT9_", type)]
data[NAME == type & m_btype20c == "RT15", brt12_illies := paste0("RT7_", type)]

# Western plains ------------------------------------------------------------------------------
type = "Western plains"
#- absolute 
data[NAME == type, sort(table(brt12_illies))]
#- relative 
data[NAME == type, sort(round(proportions(table(brt12_illies)),3))]
mapview(st_as_sf(data[NAME == type]), zcol = "brt12_illies")
data[brt12_illies == paste0("RT6_",type), brt12_illies := paste0("RT2_",type)]
data[brt12_illies == paste0("RT7_",type), brt12_illies := paste0("RT3_",type)]
data[brt12_illies == paste0("RT8_",type), brt12_illies := paste0("RT4_",type)]
data[brt12_illies == paste0("RT9_",type), brt12_illies := paste0("RT5_",type)]

# Very large rivers  ----------------------------------------------------------------

#- extract very large rivers from brtXbgr
vlr <- 
        brt_bgr |> 
        filter(m_btype12 == "RT01") |> 
        select(brt_bgr, m_zhyd) |> 
        st_drop_geometry() |> 
        setDT() |> 
        unique(by = "m_zhyd")
#- join to brtXife data I
data <- vlr[data, on = "m_zhyd"]
#- replace entries for very large rivers 
data[m_btype12 == "RT1", brt12_illies := brt_bgr]
#- drop brt_bgr column
data[, brt_bgr := NULL]

uniqueN(data$brt12_illies)
table(data$brt12_illies) |> sort()

# SAVE TO FILE ----------------------------------------------------------------------
st_write(st_as_sf(data), "E://Arbeit/Data/broad_river_types/with_illies/2021_09_21_aggregated.gpkg")
saveRDS(data, "R/new_typology/brtxife/2021_09_21_brtxife.rds")
