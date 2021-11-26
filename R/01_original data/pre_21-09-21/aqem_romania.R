# —————————————————————————— #
# ——— Clean EcoSurv data ——— # 
# —————————————————————————— #


# ———————————————————————————————————
# date: 
#       02.08.21
# files in: 

# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the AQEM data data from Romania. 
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
        fs,
        geodist
)

#- functions 
source("R/distance_to_illies_function.R")
source("R/distance_to_bgr_function.R")
source("R/fill_taxon_table.R")

# LOAD DATA  ------------------------------------------------------------------------

data       <- read_excel("data/original data/aqem_romania/raw_data/RO_Species+AQEM_ID.xlsx") 
sites      <- read_excel("data/original data/aqem_romania/raw_data/RO_RiversSections.xlsx") 


most_recent_date <- 
        dir_ls("data/original data/", regexp = "taxontable.rds") |> 
        str_remove("data/original data/") |>  tibble() |> rename(date = "str_remove(...)") |> 
        mutate(date = ymd(date)) |>  
        slice_max(date) |> pull(date)


taxontable <- readRDS(paste0("data/original data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")


# brt        <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
# illies     <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
# bgr        <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
# fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# PREPARE DATA   ---------------------------------------------------------------------
setDT(data)

#- rename abundance data
names(data)[13] <- "iz"


data2 <- data.table()
data2[, original_site_name := data$`Sampling site code`]
data2[, taxon := data$`Species name`]
data2[, date := mdy_hms(data$`Sampling date`)]
data2[, year := year(date)]
data2[, month := month(date)]
data2[, abundance := data$iz]
data2[, data.set := "aqem_romania"]


sites2 <- data.table()
sites2[, original_site_name := sites$`Sampling site code`]
sites2[, x.coord := sites$Longitude]
sites2[, y.coord := sites$Latitude]

test <- unique(sites2, by = "original_site_name")
test  |>  st_as_sf(coords = c("x.coord", "y.coord"), crs = 4326)
mapview(test)

data2 <- sites2[data2, on = "original_site_name"]

anyNA(data2$x.coord)
anyNA(data2$y.coord)

summary(data2$year)

data2$season <- case_when(
        data2$month %in% c(3, 4, 5) ~ "spring",
        data2$month %in% c(12, 1, 2) ~ "winter",
        data2$month %in% c(6, 7, 8) ~ "summer",
        data2$month %in% c(9, 10, 11) ~ "autumn"
)


(TU <- sort(unique(data2$taxon)))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
TU <- TU[new_tu]

#- save to or load from file 
saveRDS(data2, paste0("data/original data/aqem_romania/auxilliary/01_",Sys.Date(),"_data2.rds"))
data2 <- readRDS("data/original data/aqem_romania/auxilliary/01_2021-08-24_data2.rds")

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
#- error in Nais sp. 
#- identify elements that need to be entered manually 
taxontable[clean == FALSE]
#- manual fixes 
fill.taxon.table("Nais sp.", NA, "Nais", "Naididae", "Haplotaxida", NA, "Clitellata", "Annelida")
fill.taxon.table("Clinocerinae sp.", NA, NA, "Empididae", "Diptera", NA, "Insecta", "Artrhopoda")
fill.taxon.table("Empididae - larva", NA, NA, "Empididae", "Diptera", NA, "Insecta", "Artrhopoda")
fill.taxon.table("Fam. Ceratopogonidae", NA, NA, "Ceratopogonidae", "Diptera", NA, "Insecta", "Artrhopoda")

taxontable[original_name == "Fam. Dixidae", family := "Dixidae"]
taxontable[original_name == "Fam. Dolichopodidae", family := "Dolichopodidae"]
taxontable[original_name == "Fam. Dryopidae",      family := "Dryopidae"]
taxontable[original_name == "Fam. Empididae",      family := "Empididae"]
taxontable[original_name == "Fam. Enchytraeidae",  family := "Enchytraeidae"]
taxontable[original_name == "Fam. Ephydridae",     family := "Ephydridae"]
taxontable[original_name == "Fam. Limoniidae",     family := "Limoniidae"]
taxontable[original_name == "Fam. Psychodidae",    family := "Psychodidae"]
taxontable[original_name == "Fam. Rhagionidae",    family := "Rhagionidae"]
taxontable[original_name == "Fam. Stratiomyidae",  family := "Stratiomyidae"]
taxontable[original_name == "Fam. Tipulidae",      family := "Tipulidae"]
taxontable[original_name == "Nematoda  Gen.sp..",  phylum := "Nematoda"]
taxontable[original_name == "Orthocladiinae Gen. sp.", family := "Chironomidae"]
taxontable[original_name == "Orthocladiinae Gen.sp.",  family := "Chironomidae"]
taxontable[original_name == "Prostigmata sp.",         order := "Trombidiformes"    ]
taxontable[original_name == "sfam. Chironominae",      family := "Chironomidae"  ]
taxontable[original_name == "sfam. Hemerodromiinae",   family := "Empididae" ]
taxontable[original_name == "Trib Eriopterini",        family :=  "Limoniidae"]
taxontable[original_name == "Pericoma sp.",            genus := "Pericoma"]
taxontable[original_name == "Dugesia sp.",             genus := "Dugesia"]
taxontable[original_name == "Fridericia sp.",          genus := "Fridericia"]
taxontable[original_name == "Isotoma sp.", genus := "Isotoma"]
taxontable[original_name == "Limonia sp.", genus := "Loxostege"]
taxontable[original_name == "Oligochaeta varia", class := "Clitellata"]
taxontable[original_name == "Ormosia sp.", family := "Limoniidae"]

# ——— GENUS ——— #
taxontable[genus %in% c("Loxostege"), family := "Crambidae"]
taxontable[genus %in% c("Dugesia"), family := "Dugesiidae"]
taxontable[genus %in% c("Fridericia"), family := "Enchytraeidae"]
taxontable[genus %in% c("Isotoma") , family := "Isotomidae"]
taxontable[genus %in% c("Pericoma"), family := "Psychodidae"]

# ——— FAMILY ——— #
taxontable[family %in% c("Isotomidae")     , class := "Collembola"]
taxontable[family %in% c("Dryopidae")     , order := "Coleoptera"]
taxontable[family %in% c("Dixidae","Dolichopodidae", "Empididae", "Ephydridae",
                         "Limoniidae", "Psychodidae", "Rhagionidae", "Stratiomyidae",
                         "Tipulidae", "Chironomidae") , order := "Diptera"]
taxontable[family %in% c("Enchytraeidae") , order := "Haplotaxida"]
taxontable[family %in% c("Crambidae") , order := "Lepidoptera"]
taxontable[family %in% c("Dugesiidae"), order := "Tricladia"]
# ——— ORDER ——— #
taxontable[order %in% c("Diptera", "Coleoptera", "Lepidoptera", ""), c("subclass", "class") := .(NA, "Insecta")]
taxontable[order %in% c("Trombidiformes"), class := "Arachnida"]
taxontable[order %in% c("Haplotaxida"), class := "Clitellata"]
taxontable[order %in% c("Tricladia"), phylum := "Platyhelminthes"]
# ——— CLASS ——— #        
taxontable[class %in% c("Insecta", "Arachnida", "Collembola"), phylum := "Arthropoda"]
taxontable[class %in% c("Clitellata"), phylum := "Annelida"]
# ——— PHYLUM ——— #
taxontable[phylum %in% c("Arthropoda", "Nematoda", "Annelida", "Platyhelminthes"), kingdom := "Animalia"]


#- set clean to true for manually edited entries 
taxontable[, clean := TRUE]

unique(taxontable$kingdom)
taxontable[kingdom == "Chromista"]

unique(taxontable$phylum)   |> sort()
unique(taxontable$class)    |> sort()
unique(taxontable$subclass) |> sort()

#- save to or load from file 
saveRDS(taxontable, paste0("data/original data/",Sys.Date(),"_taxontable.rds"))

# COMBINE DATA SETS -----------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

unique(data3$kingdom)
unique(data3$phylum)
unique(data3$class)
unique(data3$subclass)
unique(data3$order)

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := .GRP, by = "date"]

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_aqem_romania")]



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
saveRDS(data4, paste0("data/original data/aqem_romania/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/aqem_romania/auxilliary/02_2021-08-24_data4.rds")

# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
#mapview(sites)
#- save sites to file 
#saveRDS (sites, paste0("data/original data/aqem_romania/auxilliary/03_",Sys.Date(),"_sites.rds"))
#sites <- readRDS("data/original data/aqem_romania/auxilliary/03_2021-08-02_sites.rds")

#- Assign typology 
sites <-
        sites[, c("site_id", "geometry")] |> 
        st_transform(crs = st_crs(typologies))
nn <- st_nearest_feature(sites, typologies)
nn <- typologies[nn,]
distances <- st_distance(sites, y = nn, by_element = TRUE)
sites %<>% mutate(distance = distances, 
                  brt12    = nn$brt12,
                  ife      = nn$bgr,
                  bgr      = nn$ife,
                  brtXife  = nn$brtXife,
                  brtXbgr  = nn$brtXbgr,
                  least.impacted = nn$least.impacted
                          )


$distance = distances
sites$brt12 <- typologies_nn$brt12
sites$ife <- typologies_nn$illies
sites$bgr <- typologies_nn$bgr

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
#- save to or load from file
saveRDS(distance_list, paste0("data/original data/aqem_romania/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/aqem_romania/auxilliary/04_2021-08-02_distance_to_brt.rds")

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
sites6[, brt12_bgr := paste0(brt12, "_", bgr)]

#- join sites with data 
data5 <- sites6[data4, on = "site_id"]

#- save to or load from file 
saveRDS(sites6, paste0("data/original data/aqem_romania/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,  paste0("data/original data/aqem_romania/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites2 <- readRDS("data/original data/aqem_romania/auxilliary/06_2021-08-04_sites2.rds")
data5 <- readRDS("data/original data/aqem_romania/auxilliary/07_2021-08-24_data5.rds")


# Statistics ------------------------------------------------------------------------
uniqueN(sites6$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)
sites6 |> filter(brt_distance <= 500) |> count()
data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
sites6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()

# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)

for (i in 1:nrow(sites)) {
        #for (i in 1:10){
        
        #- setup for a new list
        if (i == 1)
                new.lst <- list()
        
        #- subset to focal site
        lp.sub   <- data5[site_id == sites$site_id[i]]
        #- how many sampling events have taken place here?
        lp.n     <- unique(lp.sub$sampling.events)
        #- if it is only one - check richness and keep as such
        #- of there are less then five taxa at this site, drop it
        if (lp.n == 1) {
                if (any(lp.sub$richness < 5)) {
                        new.lst[[i]] <- lp.sub[data.set == "none"]
                        next()
                } else {
                        new.lst[[i]]  <- lp.sub
                        next()
                }
        } 
        #- otherwise verify that its one site with coordinates
        lp.x     <-
                uniqueN(lp.sub$x.coord)
        lp.y     <- uniqueN(lp.sub$y.coord)
        if (!(lp.x == 1 & lp.y == 1)) {
                print(paste("in", i, " there are more than two under a week"))
                break()
        }
        
        lp.ls.season <-
                list(
                        spring = lp.sub[season == "spring"],
                        summer = lp.sub[season == "summer"],
                        autumn = lp.sub[season == "autumn"],
                        winter = lp.sub[season == "winter"]
                )
        
        #- which seasons are not empty?
        lp.season.id <- which(sapply(lp.ls.season, nrow) != 0)
        
        #- loop over non-empty seasons
        for (k in lp.season.id) {
                lp.k.dat <- lp.ls.season[[k]]
                
                #- how many samples from this season?
                lp.k.n <- uniqueN(lp.k.dat$gr_sample_id)
                
                #- if only one sample ...
                #- check richness
                #- keep as is if richness is OK
                #- replace is empty data.table (lp.sub[season == "dummy"]) if richness is smaller than 5
                if (lp.k.n == 1) {
                        if (any(lp.k.dat$richness < 5)) {
                                lp.ls.season[[k]] <- lp.sub[data.set == "none"]
                                next()
                        } else {
                                lp.ls.season[[k]]  <- lp.k.dat
                                next()
                        }
                }
                
                #- ... if there is more than one sample in the season k
                #- how far apart are the sampling dates?
                lp.dates <- unique(lp.k.dat$date)
                lp.time.diff <- diff(lp.dates)
                #- if samples were taken within the same week combine them.
                if (any(lp.time.diff < 7)) {
                        #- which samples are close together?
                        lp.diff.id <- which(lp.time.diff < 7)
                        #- extract date from lp.diff.ids
                        lp.diff.date <-
                                lapply(lp.diff.id, function(x)
                                        lp.dates[x:(x + 1)])
                        
                        #- loop over samples the combinations.
                        #- implemented for cases where multiple lp.diff.ids
                        for (j in seq_along(lp.diff.id)) {
                                #- select dates for this loop iteration
                                lp.j.diff <- lp.diff.date[[j]]
                                #- split data set: i) data to be combined (lp.combine) and ii). rest of the data (lp.rest)
                                lp.combine <-
                                        lp.k.dat[date %in% lp.j.diff]
                                lp.rest     <-
                                        lp.k.dat[!date %in% lp.j.diff]
                                
                                #- combine taxa; assign mean abundance
                                lp.combine[, abundance := mean(abundance), by = "lowest.taxon"]
                                lp.combine <-
                                        unique(lp.combine, by = "lowest.taxon")
                                lp.combine[, richness     := .N]
                                lp.draw.id <- which.max(unique(lp.combine$date))
                                lp.combine[, date         := unique(lp.combine$date)[lp.draw.id]]
                                lp.combine[, gr_sample_id := unique(lp.combine$gr_sample_id)[lp.draw.id]]
                                lp.combine[, date_id := unique(lp.combine$date_id)[lp.draw.id]]
                                lp.k.dat <- rbindlist(list(lp.rest, lp.combine))
                        }
                        
                        
                        lp.k.n <- uniqueN(lp.k.dat$gr_sample_id)
                        lp.k.dat$sampling.events <- lp.k.n
                        
                        if (lp.k.n == 1) {
                                #- drop date
                                lp.k.dat[, date := NA]
                                lp.k.dat[, gr_sample_id := paste0(
                                        site_id,
                                        "_",
                                        c("spring", "summer", "autumn", "winter")[k],
                                        "_combined_aqem_romania"
                                )]
                                lp.ls.season[[k]] <- lp.k.dat
                                next()
                        }
                } # END OF: SAMPLES WITHIN ONE WEEK 
                
                #- check richness - remove samples with less than five taxa
                if (any(unique(lp.k.dat$richness) < 5)) {
                        #- if all samples are below 5
                        if (all(lp.k.dat$richness < 5)) {
                                lp.ls.season[[k]] <- lp.sub[season == "dummy"]
                                next()
                        }
                        lp.k.dat <- lp.k.dat[richness > 5]
                        lp.k.n   <- uniqueN(lp.k.dat$gr_sample_id)
                        #- if only one remains
                        if (lp.k.n == 1) {
                                lp.ls.season[[k]] <- lp.k.dat
                                next()
                        }
                }
                
                lp.tab  <- table(lp.k.dat$lowest.taxon)
                lp.tab2 <- lp.tab / lp.k.n
                lp.sub2 <-
                        lp.k.dat[lowest.taxon %in%  names(lp.tab2)[lp.tab2 >= 0.5]]
                #- combine multiple observations of the same taxon. Take mean of abundance
                lp.sub2[, abundance := mean(abundance), by = "lowest.taxon"]
                lp.sub2 <- unique(lp.sub2, by = "lowest.taxon")
                lp.sub2[, richness := .N]
                #- add new sample id
                lp.sub2[, gr_sample_id := paste0(site_id,
                                                 "_",
                                                 c("spring", "summer", "autumn", "winter")[k],
                                                 "_combined_aqem_romania")]
                #- drop date
                lp.sub2[, date := NA]
                #- keep year if both were from the same year.
                lp.sub2[, year := ifelse(uniqueN(year) == 1, year, NA)]
                #- add to list
                lp.ls.season[[k]] <- lp.sub2
        }
        
        lp.ls.season <- lp.ls.season[lp.season.id]
        lp.ls.season <- rbindlist(lp.ls.season)
        new.lst[[i]] <- lp.ls.season
        
        lp.progress <- i/nrow(sites) * 100
        print(lp.progress)
        rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
        rm(i)
        gc()
}

data6 <- rbindlist(new.lst,use.names=TRUE,fill =TRUE)
data6[, richness := .N, by = "gr_sample_id"]

# summary aggregated ----------------------------------------------------------------
uniqueN(data6$gr_sample_id)

data6 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data6 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data6, paste0("data/original data/biodrought/",Sys.Date(),"_final_aggregated.rds"))       
saveRDS(data5, paste0("data/original data/biodrought/",Sys.Date(),"_final.rds"))       


