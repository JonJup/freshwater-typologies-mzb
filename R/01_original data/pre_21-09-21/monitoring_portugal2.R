# ———————————————————————————————————————— #
# ——— Clean Portuguese Monitoring data ——— # 
# ———————————————————————————————————————— #


# ———————————————————————————————————
# date: 
#       16.08.21
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the 2019er Portuguese 
#       monitoring data provided by Teresa Ferreira. 
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


data <- read_excel("data/original data/monitoring_portugual/raw_data/Macroinvertebrados_2019 .xlsx", sheet = 3)

taxontable <- readRDS("data/original data/2021-08-11_taxontable.rds")
brt        <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies     <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr        <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# PREPARE DATA   ---------------------------------------------------------------------

site_name <- names(data)[-c(1:2)]
original_site_names <- 
        data[3,-c(1:2)] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[1,-c(1:2)] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[2,-c(1:2)] |> 
        unlist() |> 
        unname() |> 
        as.numeric()

data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = as.Date(NA), year = 2019, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
data2$season <- "summer"

taxa <- data[-c(1:3), -1]
taxa %<>% 
        rename(taxon = "...2") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       season = NA,
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "protugal19"
                      )

data <- data[taxon != "SOMA"]
data[, taxon := str_remove(taxon, "\\(Ad\\)")]
data[, taxon := str_remove(taxon, "\\(Lv\\)")]
data[, taxon := str_trim(taxon)]

TU <- sort(unique(data$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
TU <- TU[new_tu]

#- save to or load from file 
saveRDS(data, paste0("data/original data/monitoring_portugual/auxilliary/01_",Sys.Date(),"_data2_2.rds"))
data <- readRDS("data/original data/monitoring_portugual/auxilliary/01_2021-08-16_data2_2.rds")

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
fill.taxon.table("HIRUDINEA n.d",  NA, NA, NA, NA, "Hirudinea", "Clitellata", "Annelida")
fill.taxon.table("Planaria n.d.",  NA, NA, NA, NA,          NA, "Turbellaria", "Plathyheminthes")
fill.taxon.table("So. Anisoptera", NA, NA, NA, "Odonata",       NA, "Insecta"    , "Arthropoda")

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
data3$year = 2019
data3[, date_id := .GRP, by = c("year")]


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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_portugual")]



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
data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/monitoring_portugual/auxilliary/02_",Sys.Date(),"_data4_2.rds"))
data4 <- readRDS("data/original data/monitoring_portugual/auxilliary/02_2021-08-13_data4.rds")

# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/monitoring_portugual/auxilliary/03_",Sys.Date(),"_sites_2.rds"))
sites <- readRDS("data/original data/monitoring_portugual/auxilliary/03_2021-08-13_sites.rds")

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
saveRDS(distance_list, paste0("data/original data/monitoring_portugual/auxilliary/04_",Sys.Date(),"_distance_to_brt2.rds"))
distance_list  <- readRDS("data/original data/monitoring_portugual/auxilliary/04_2021-08-02_distance_to_brt.rds")

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
saveRDS(sites6, paste0("data/original data/monitoring_portugual/auxilliary/06_", Sys.Date(), "_sites2_2.rds"))
saveRDS(data5,  paste0("data/original data/monitoring_portugual/auxilliary/07_", Sys.Date(), "_data5_2.rds"))

sites2 <- readRDS("data/original data/monitoring_portugual/auxilliary/06_2021-08-04_sites2.rds")
data5 <- readRDS("data/original data/monitoring_portugual/auxilliary/07_2021-08-04_data5.rds")

# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)

#- not necessary 

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data5, paste0("data/original data/monitoring_portugual/",Sys.Date(),"_final2.rds"))       
