# —————————————————————————— #
# ——— Clean EcoSurv data ——— # 
# —————————————————————————— #


# ———————————————————————————————————
# date: 
#       14.07.21
# files in: 
#       -> sampling locations and results    | data/original data/denes_schmera/Hungary-data2.xlsx 
#       -> table for taxonomic harmonization | data/original data/taxontable.rds
#       -> broad river types                 | E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp
#       -> GloRiC                            | E://Arbeit/Data/GloRiC_v10/gloric_europe.gpkg
#       -> Illies Ecoregions                 | E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp
#       -> Biogeographical Regions           | E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the raw data
#       provided from EcoSurv by Denes Schmera. 
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
        stringr
)
# LOAD DATA  ------------------------------------------------------------------------
data       <- read_excel("data/original data/denes_schmera/Hungary-data2.xlsx") 
taxontable <- readRDS   ("data/original data/2021-08-02_taxontable.rds")
brt        <- st_read   ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies     <- st_read   ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr        <- st_read   ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read   ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

#- functions 
source("R/distance_to_illies_function.R")
source("R/distance_to_bgr_function.R")

# PREPARE DATA   ---------------------------------------------------------------------
setDT(data)

#- information on year and season is taken from  Schmera & Bauer (2011): Testing a
#- typology system of running waters for conservation planning in Hungary

data = data[, 
            list(   original_site_name = SiteID,
                    year =  2005,
                    season = "spring",
                    x.coord = EOVY,
                    y.coord = EOVX,
                    taxon = TAXON,
                    EPSG = 23700,
                    date = as.Date(NA),
                    data.set = "ecosurv"
            )
]

#- save to or load from file 
saveRDS(data, paste0("data/original data/denes_schmera/auxilliary/01_",Sys.Date(),"_data2.rds"))
data <- readRDS("data/original data/denes_schmera/auxilliary/01_2021-07-20_data2.rds")

# TAXONOMY --------------------------------------------------------------------------
(TU <- sort(unique(data$taxon)))

#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
TU <- TU[new_tu]

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
taxontable[original_name == "Agabus neglectus (Erichson 1837)"    ,`:=` (species = "Ilybius neglectus"   , genus = "Ilybius"     , family = "Dytiscidae"     , order = "Coleoptera"       , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Baetis muticus (Linnaeus 1758)"      ,`:=` (species = "Alainites muticus"   , genus = "Alainites"   , family = "Baetidae"       , order = "Ephemeroptera"    , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Baetis niger (Linnaeus 1761)"        ,`:=` (species = "Nigrobaetis niger"   , genus = "Nigrobaetis" , family = "Baetidae"       , order = "Ephemeroptera"    , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]                                             
taxontable[original_name == "Baetis tricolor Tshernova 1928"      ,`:=` (species = "Labiobaetis tricolor", genus = "Labiobaetis" , family = "Baetidae"       , order = "Ephemeroptera"    , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]                                                                             
taxontable[original_name == "Fagotia esperi (Ferussac 1823)"      ,`:=` (species = "Esperiana esperi"    , genus = "Esperiana"   , family = "Melanopsidae"   , order = NA                 , subclass = NA, class = "Gastropoda", phylum = "Mollusca"  , kingdom = "Animalia")]                                                   
taxontable[original_name == "Gomphus flavipes (Charpentier 1825)" ,`:=` (species = "Gomphus pulchellus"  , genus = "Gomphus"     , family = "Gomphidae"      , order = "Odonata"          , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]                                                   
taxontable[original_name == "Gyraulus crista (Linnaeus 1758)"     ,`:=` (species = "Armiger crista"      , genus = "Armiger"     , family = "Planorbidae"    , order = NA                 , subclass = NA, class = "Gastropoda", phylum = "Mollusca"  , kingdom = "Animalia")]                                                                                                     
taxontable[original_name == "Habrophlebia lauta Eaton 1884"       ,`:=` (species = "Habrophlebia lauta"  , genus = "Habrophlebia", family = "Leptophlebiidae", order = "Ephemeroptera"    , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]                                                 
taxontable[original_name == "Pisidium casertanum (Poli 1791)"     ,`:=` (species = "Euglesa casertana"   , genus = "Euglesa"     , family = "Sphaeriidae"    , order = "Sphaeriida"       , subclass = NA, class = "Bivalvia"  , phylum = "Mollusca"  , kingdom = "Animalia")]                                               
#- set clean to true for manually edited entries 
taxontable[, clean := TRUE]


unique(taxontable$kingdom)
unique(taxontable$phylum)   |> sort()
unique(taxontable$class)    |> sort()
unique(taxontable$subclass) |> sort()

#- save to or load from file 
saveRDS(taxontable, "data/original data/taxontable.rds")
taxontable <- readRDS("data/original data/taxontable.rds")

# COMBINE DATA SETS -----------------------------------------------------------------

names(data)[which(names(data) == "taxon")] <- "original_name"
data3 <- taxontable[data, on = "original_name"]

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := 00001]

#- add leading zeros 
data3[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_ecosurv")]

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
        abundance = NA,
        x.coord,
        y.coord,
        EPSG,
        data.set = "ecosurv"
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

data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/denes_schmera/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/denes_schmera/auxilliary/02_2021-07-27_data4.rds")


# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/denes_schmera/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/denes_schmera/auxilliary/03_2021-07-27_sites.rds")

# ——————————————————————— #
# ——— DISTANCE TO BRT ——— #
# ——————————————————————— #

sites  <- st_transform(sites, crs = st_crs(brt))
nn     <- st_nearest_feature(sites, brt)
brt_nn <- brt[nn,]

# distance_list <-
#         map(.x = 1:nrow(sites),
#             .f = ~ as.numeric(st_distance(x = sites[.x, ],
#                                           y = brt_nn[.x, ])))
# beepr::beep()
#- save to or load from file 
# saveRDS(distance_list, paste0("data/original data/denes_schmera/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/denes_schmera/auxilliary/04_2021-07-14_distance_to_brt.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "brt_distance" = unlist(distance_list),
        "brt20"    = brt_nn$m_btype20c,
        "brt12"    = brt_nn$m_btype12
)

sites <- distance_table[sites, on = "site_id"]
#sites.close <- sites[brt_distance <= 500]
mapview(st_as_sf(sites), zcol = "brt_distance" )

# —————————————————————————— #
# ——— DISTANCE TO GLORIC ——— #
# —————————————————————————— #

# sites.close  <- st_as_sf(sites.close) |> st_transform(crs = st_crs(gloric))
# nn           <- st_nearest_feature(sites.close, gloric)
# gloric_nn    <- gloric[nn,]
# 
# distance_list <-
#         map(.x = 1:nrow(sites.close),
#             .f = ~ as.numeric(st_distance(x = sites.close[.x, ],
#                                           y = gloric_nn[.x, ])))
# beepr::beep()
# #- save to or load from file 
# saveRDS(distance_list, paste0("data/original data/denes_schmera/auxilliary/05_",Sys.Date(),"_distance_to_gloric.rds"))
# distance_list  <- readRDS("data/original data/denes_schmera/auxilliary/05_2021-07-14_distance_to_gloric.rds")
# 
# #- create distance table 
# distance_table <- data.table(
#         "site_id" = sites.close$site_id,
#         "glroic_distance" = unlist(distance_list),
#         "gloric"    = gloric_nn$Kmeans_30
# )
# 
# sites.close <- distance_table[sites.close, on = "site_id"]
# sites.close <- sites.close[glroic_distance <= 500]
# mapview(st_as_sf(sites.close))

# —————————————— #
# ——— Illies ——— #
# —————————————— #

sites |> 
        st_as_sf() |> 
        st_transform(crs = st_crs(illies)) |> 
        st_join(select(illies, NAME)) -> sites

# —————————————— #
# ——— BGR    ——— #
# —————————————— #

sites |>
        st_as_sf() |>
        st_transform(crs = st_crs(bgr)) |>
        st_join(select(bgr, code)) -> sites


# —————————————— #
# ——— FEC    ——— #
# —————————————— #

sites |>
        st_as_sf() |>
        st_transform(crs = st_crs(fec)) |>
        st_join(select(fec, least.impacted)) -> sites

sites |> filter(is.na(least.impacted)) |> unique(by = "site_id") |> st_write(dsn = "ecosurv.gpkg")


sites2 <-
        sites |> 
        rename(illies = NAME, bgr = code, fec.least.impacted = least.impacted) %>%
        select(c("brt20", "brt12", "site_id", "illies", "bgr", "fec.least.impacted", "brt_distance")) %>%
        st_drop_geometry() |> 
        setDT()

sites2[is.na(fec.least.impacted), fec.least.impacted := FALSE]

sites2[, brt12_illies := paste0(brt12, "_", illies)]
sites2[, brt12_bgr := paste0(brt12, "_", bgr)]

data5 <- sites2[data4, on = "site_id"]


#- save to or load from file 
saveRDS(sites2, paste0("data/original data/denes_schmera/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,  paste0("data/original data/denes_schmera/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites2 <- readRDS("data/original data/denes_schmera/auxilliary/06_2021-08-04_sites2.rds")
data5  <- readRDS("data/original data/denes_schmera/auxilliary/07_2021-08-04_data5.rds")

# Statistics ------------------------------------------------------------------------
uniqueN(sites2$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)

data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()

sites2 |> filter(brt_distance <= 500) |> count()
sites2 |> filter(fec.least.impacted) |> count()
sites2 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()


# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)

#- Aggregation not necessary 

# distance to region border ---------------------------------------------------------
sites <- unique(data6, by = "site_id")
sites %<>%rename(code = bgr)
sites %<>%rename(NAME = illies)
sites %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])

illies %<>% st_transform(crs = st_crs(sites))
bgr    %<>% st_transform(crs = st_crs(sites))
#- Distance to the closest border of an Illies Freshwater ecoregion
sites$illies_distance <- sapply(1:nrow(sites), distance.to.illies, sites)
#- Distance to the closest border of an Biogeographical Region
sites$bgr_distance <- sapply(1:nrow(sites), distance.to.bgr, sites)
beepr::beep()
sites <- sites[,c("site_id", "illies_distance", "bgr_distance")]
setDT(sites)
data6 <- sites[data6, on = "site_id"]

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data6, paste0("data/original data/denes_schmera/",Sys.Date(),"_final.rds"))       
data6 <- readRDS("data/original data/denes_schmera/2021-08-04_final.rds")
