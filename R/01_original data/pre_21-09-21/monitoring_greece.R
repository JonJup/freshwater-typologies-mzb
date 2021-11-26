# ——————————————————————————————————— #
# ——— Clean Greek Monitoring data ——— # 
# ——————————————————————————————————— #


# ———————————————————————————————————
# date: 
#       10.08.21
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the Greek monitoring data provided by Maria Lazaridou. 
# ————————————————

# SETUP  -----------------------------------------------------------------------------

pacman::p_load(
        here,
        taxize,
        data.table,
        sf,
        dplyr,
        lubridate,
        magrittr,
        mapview,
        purrr,
        readr,
        readxl,
        stringr,
        tidyr
)

#- functions 
source("R/distance_to_illies_function.R")
source("R/distance_to_bgr_function.R")
source("~/my documents/R/functions/fill_taxon_table.R")

# LOAD DATA  ------------------------------------------------------------------------

data       <- read_excel("data/original data/monitoring_greece/raw_data/Database_Macroinvertebrate_GR_08.09.2021.xlsx") 
taxontable <- readRDS("data/original data/2021-08-09taxontable.rds")
brt        <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies     <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr        <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# PREPARE DATA   ---------------------------------------------------------------------

#- transform to data.table 
setDT(data)

#- the first two columns can be discarded 
data <- data[, -c(1,2)]

#- the first two rows can be discarded 
data <- data[-c(1,2),]

#- there are two site columns ('Station' and 'Site Name'). For most entries they are the same. 
data[`Site Name` != Station, c("Site Name", "Station")]
uniqueN(data$`Site Name`) - uniqueN(data$Station)
#- Site Name has 38 more unique entries so I stick to this ID. 

#- season and year 
data[, season := str_to_lower(str_remove(str_extract(Season, ".*_"), "_"))]
data[, year  := as.numeric(str_remove(str_extract(Season, "_.*"), "_"))]
#- coordinates 
data[,x.coord := data$`Coordinates (wgs84)`]
data[,y.coord := data$`...6`]
#- drop variables 
data %<>% select(c(1,9:ncol(data)))

#- create taxon column in long format 
data %<>% pivot_longer(cols = !c("Site Name", "year", "season", "y.coord", "x.coord"), names_to = "taxon", values_to = "abundance")

#- drop absence records 
data %<>% filter(abundance != 0)
#- rename site variable 
data %<>% rename(original_site_name = 'Site Name')
#- add NA date variable 
data %<>% mutate(date = as.Date(NA))
#- add data set variable 
data %<>% mutate(data.set = "monitoring_greece")

#- verify that there is only one x and y coordinate per site name 
setDT(data)
data[, uniqueN(x.coord), by = "original_site_name"] |> pull(V1) |> table()
data[, uniqueN(y.coord), by = "original_site_name"] |> pull(V1) |> table()

#- except for one all are fine. The one special case needs to be fixed. 
data[, uniqueN(x.coord), by = "original_site_name"][V1 == 3]
data[original_site_name == "SYMVOLI" & year == 2010, original_site_name := "SYMVOLI_1"]
data[original_site_name == "SYMVOLI" & year == 2009, original_site_name := "SYMVOLI_2"]
data[original_site_name == "SYMVOLI" & year == 2008, original_site_name := "SYMVOLI_3"]
#- test spatial coordinates 
sites <- 
        unique(data, by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = 4326)

mapview(sites)
#- one site is far removed from all other sites towards the eastern border of Turkey. I will remove it. 
data %<>% filter(original_site_name != "DW_STRATOPEDO")


(TU <- sort(unique(data$taxon)))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
TU <- TU[new_tu]

#- save to or load from file 
saveRDS(data, paste0("data/original data/monitoring_greece/auxilliary/01_",Sys.Date(),"_data2.rds"))
data <- readRDS("data/original data/monitoring_greece/auxilliary/01_2021-08-10_data2.rds")

# TAXONOMY --------------------------------------------------------------------------

taxontable_new <- data.table(
        original_name = TU,
        species = character(length(TU)),
        genus = character(length(TU)),
        family = character(length(TU)),
        order = character(length(TU)),
        subclass = character(length(TU)),
        class = character(length(TU)),
        phylum = character(length(TU)),
        kingdom = character(length(TU)),
        clean = F
)

taxontable <- rbindlist(list(taxontable, taxontable_new))

for (i in seq_along(TU)){
        
        #- skip this iteration of the loop if the focal taxon has already been evaluated. 
        if (taxontable[original_name == TU[i], clean]) next()
        
        i.co <- 
                classification(TU[i], db = "gbif") |> 
                {\(x) x[[1]]}()
        
        # skip this iteration of the taxon is not found 
        if (is.na(i.co)) next()
        #- assign taxon levels  
        taxontable[original_name == TU[i], species  := ifelse("species"  %in% i.co$rank, i.co$name[which(i.co$rank == "species")], NA)]
        taxontable[original_name == TU[i], genus    := ifelse("genus"    %in% i.co$rank, i.co$name[which(i.co$rank == "genus")], NA)]
        taxontable[original_name == TU[i], family   := ifelse("family"   %in% i.co$rank, i.co$name[which(i.co$rank == "family")], NA)]
        taxontable[original_name == TU[i], order    := ifelse("order"    %in% i.co$rank, i.co$name[which(i.co$rank == "order")], NA)]
        taxontable[original_name == TU[i], subclass := ifelse("subclass" %in% i.co$rank, i.co$name[which(i.co$rank == "subclass")], NA)]
        taxontable[original_name == TU[i], class    := ifelse("class"    %in% i.co$rank, i.co$name[which(i.co$rank == "class")], NA)]
        taxontable[original_name == TU[i], phylum   := ifelse("phylum"   %in% i.co$rank, i.co$name[which(i.co$rank == "phylum")], NA)]
        taxontable[original_name == TU[i], kingdom  := ifelse("kingdom"  %in% i.co$rank, i.co$name[which(i.co$rank == "kingdom")], NA)]
        taxontable[original_name == TU[i], clean := TRUE]
        rm(i.co)
        
}

#- identify elements that need to be entered manually 
taxontable[clean == FALSE]
#- manual fixes 
fill.taxon.table("Belostomatidae", NA, NA, "Belostomatidae ", "Hemiptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Potamonidae",    NA, NA, "Potamonidae"    , "Decapoda", NA, "	Malacostraca", "Artrhopoda")
taxontable[, clean := TRUE]

#- set clean to true for manually edited entries 
taxontable[, clean := TRUE]

#- save to or load from file 
saveRDS(taxontable, paste0("data/original data/",Sys.Date(),"_taxontable.rds"))
taxontable <- readRDS("data/original data/2021-08-10_taxontable.rds")

# COMBINE DATA SETS -----------------------------------------------------------------
data2 <- data
setDT(data2)
names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := .GRP, by = c("year", "season")]

#- add leading zeros 
data3[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data3[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_greece")]



data4 <- data3[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
        season,
        site_id,
        date_id,
        original_name,
        species,
        genus,
        family,
        order,
        subclass,
        class,
        phylum,
        kingdom,
        abundance,
        x.coord,
        y.coord,
        EPSG = 4326,
        data.set
)]

#- combine entries of same taxon 
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum,kingdom)))))))]

#- check 
data4[is.na(lowest.taxon)]

data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/monitoring_greece/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/monitoring_greece/auxilliary/02_2021-08-10_data4.rds")

# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/monitoring_greece/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/monitoring_greece/auxilliary/03_2021-08-02_sites.rds")

# ——————————————————————— #
# ——— DISTANCE TO BRT ——— #
# ——————————————————————— #

sites <- sites[, c("site_id", "geometry")]
sites  <- st_transform(sites, crs = st_crs(brt))
nn     <- st_nearest_feature(sites, brt)
brt_nn <- brt[nn,]

distance_list <-
        map(.x = 1:nrow(sites),
            .f = ~ as.numeric(st_distance(x = sites[.x, ],
                                          y = brt_nn[.x, ])))
beepr::beep()
# #- save to or load from file
saveRDS(distance_list, paste0("data/original data/monitoring_greece/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/monitoring_greece/auxilliary/04_2021-08-02_distance_to_brt.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "brt_distance" = unlist(distance_list),
        "brt20"    = brt_nn$m_btype20c,
        "brt12"    = brt_nn$m_btype12
)

sites2 <- distance_table[sites, on = "site_id"]

# —————————————— #
# ——— Illies ——— #
# —————————————— #
sites3 <- 
        sites2 |> 
        st_as_sf() |> 
        st_transform(crs = st_crs(illies)) |> 
        st_join(select(illies, NAME)) 

#- Distance to the closest border of an Illies Freshwater ecoregion
sites3$illies_distance <- sapply(1:nrow(sites), distance.to.illies, sites3)

# ——————————— #
# ——— BGR ——— #
# ——————————— #
sites4 <- 
        sites3 |>
        st_transform(crs = st_crs(bgr)) |>
        st_join(select(bgr, code)) 

#- Distance to the closest border of an Biogeographical Region
sites4$bgr_distance <- sapply(1:nrow(sites), distance.to.bgr, sites4)

# —————————————— #
# ——— FEC    ——— #
# —————————————— #
sites5 <- 
        sites4 |>
        st_transform(crs = st_crs(fec)) |>
        st_join(select(fec, least.impacted)) 

#- reshape data 
sites6 <-
        sites5 |> 
        rename(illies = NAME, bgr = code, fec.least.impacted = least.impacted) %>%
        select(c("brt20", "brt12", "site_id", "illies", "bgr", "fec.least.impacted", "brt_distance", "bgr_distance", "illies_distance")) %>%
        st_drop_geometry() |> 
        setDT()


#- add combined types BRT + region
sites6[, brt12_illies := paste0(brt12, "_", illies)]
sites6[, brt12_bgr    := paste0(brt12, "_", bgr)]

#- join sites with data 
data5 <- sites6[data4, on = "site_id"]

#- remove data before 2005 
data5 <- data5[year >=2005]

#- save to or load from file 
saveRDS(sites6, paste0("data/original data/monitoring_greece/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,  paste0("data/original data/monitoring_greece/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites2 <- readRDS("data/original data/monitoring_greece/auxilliary/06_2021-08-10_sites2.rds")
data5 <- readRDS("data/original data/monitoring_greece/auxilliary/07_2021-08-10_data5.rds")

# SUMMARY STATISTCS -----------------------------------------------------------------
# - number of sites 
sites2 |> nrow()
sites2[brt_distance <= 500, .N]
sites2[fec.least.impacted == TRUE, .N]
sites2[fec.least.impacted == TRUE & brt_distance <= 500, .N]

data5[, uniqueN(gr_sample_id)]

# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)

#- no temporal aggregation necessary

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data6, paste0("data/original data/monitoring_greece/",Sys.Date(),"_final.rds"))       
