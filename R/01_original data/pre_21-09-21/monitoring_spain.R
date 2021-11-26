# ---------------------------------------------------- #
# -------- Clean Spanish Monitoring data ------------- # 
# ---------------------------------------------------- #


# --------------------------------------------------------------------------------------------------------
# date written: 
#       17.08.21
# date last modified:
#       14.09.21
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the Spain monitoring data provided by Nuria Bonada. 
#       Note: In her Email from the 17.08.12, Nuria writes that, for the data on all of Spain, we should better use the later years to ensure consistency with the national monitoring standards. 
#       For the data on Catalonia, after 2013 is best. 
# --------------------------------------------------------------------------------------------------------

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

dt.ctln1.1 <- read_excel("data/original data/monitoring_spain/raw_data/Catalan data.xlsx", sheet = 1) 
dt.ctln2.1 <- read_excel("data/original data/monitoring_spain/raw_data/Catalan data.xlsx", sheet = 2) 
dt.as.1 <- read_excel("data/original data/monitoring_spain/raw_data/Data long term Spain.xlsx", sheet = 1)

taxontable <- readRDS("data/original data/2021-08-27_taxontable.rds")
brt        <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies     <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr        <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# PREPARE DATA   ---------------------------------------------------------------------

# Nuria wrote: "For the Spanish data, all should be data collected with the official
# sampling method but better to use the latter years" and "For the Catalan data, the
# period 2013-2018 was also sampled using the official method".

# —————— Data Catalan
#- transform to data.table 
setDT(dt.ctln1.1)
setDT(dt.ctln2.1)


#- all site names refer to more than one x.coordiante value hence I create a new unique identifier 
dt.ctln1.2 = data.table(
        taxon              = dt.ctln1.1$Taxa,
        abundance          = dt.ctln1.1$`Abundancia (ind/mostra, fins un màxim de 100)`,
        date               = ymd(dt.ctln1.1$DataRealització),
        x.coord            = dt.ctln1.1$UTMX,
        y.coord            = dt.ctln1.1$UTMY
                       )

dt.ctln2.2 = data.table(
        taxon              = dt.ctln2.1$Tbl_MacInv_Dic.Nombre,
        abundance          = dt.ctln2.1$Cantidad,
        date               = ymd(dt.ctln2.1$Fecha),
        x.coord            = dt.ctln2.1$UTMX,
        y.coord            = dt.ctln2.1$UTMY
                       )

dt.ctln <- rbindlist(list(dt.ctln1.2, dt.ctln2.2))
dt.ctln[, original_site_name := .GRP, by = c("x.coord", "y.coord")]

#- check that each site has only one coordinate 
all(dt.ctln[,uniqueN(x.coord), by = "original_site_name"]$V1 == 1)

#- assign year and EPSG code
dt.ctln[, year := year(date)]
dt.ctln[, EPSG := 25830]
 
summary(dt.ctln$year)

#- drop observations before 2013 or without date 
dt.ctln <- dt.ctln[!is.na(year) & year > 2012]

#- check sites on a map 
sites <-
        dt.ctln |>
        unique(by = "original_site_name") |>
        st_as_sf(coords = c("x.coord", "y.coord"), crs = dt.ctln$EPSG[1])

mapview(sites)

#- transform to WGS 84
dt.ctln %<>% 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = dt.ctln$EPSG[1]) %>%
        st_transform(crs = 4326)
#- add cat_ prefix to site name
dt.ctln %<>% mutate(original_site_name = paste0("cat_", original_site_name))

# —————— Monitoring all of Spain 
dt.as.2 <- copy(dt.as.1)

names(dt.as.2)%<>% str_remove_all("-")
names(dt.as.2)%<>% str_trim()

for (i in seq_along(names(dt.as.2))){
        if (names(dt.as.2)[i] %in% c("Site", "EU_water body", "surfaceWaterBodyName", "longitude", "latitude", "Sampling date")) next()
        x <- names(dt.as.2)[i]
        x1 <- word(x, -1)
        if (
                substring(x1, 1,1) == str_to_upper(substring(x1,1,1))
        ) {
                x2 <- x1
        } else {
                x1b <- word(x, -5)
                x2 <- paste(x1b, x1)
        }
        names(dt.as.2)[i] <- x2        
}
#- fix problems 
names(dt.as.2)[which(names(dt.as.2) == " caurelensis")]  <- "Philopotamus montanus"
names(dt.as.2)[which(names(dt.as.2) ==  " lusitanicus")] <- "Austropotamobius pallipes"
names(dt.as.2)[which(names(dt.as.2) ==  " saturniae")]   <- "Silo mediterraneus"
#- drop emtpy taxon column
dt.as.2 <- dt.as.2[,-which(names(dt.as.2) == "")]
#- reshape into long format 
dt.as.3 <- pivot_longer(data = dt.as.2, 
                        cols = Dytiscidae:Hydrochus,
                        names_to = "taxon", 
                        values_to = "abundance")
#- reshape into common format 
dt.as.4 <- data.table(
        taxon              = dt.as.3$taxon,
        abundance          = dt.as.3$abundance,
        x.coord            = dt.as.3$longitude,
        y.coord            = dt.as.3$latitude,
        EPSG               = 4326,
        date               = ymd(dt.as.3$`Sampling date`)
        )
dt.as.4[, original_site_name := .GRP, by = c("x.coord", "y.coord")]
dt.as.4[, original_site_name := paste0("as_",original_site_name)]
dt.as.4 <- dt.as.4[abundance != 0]
dt.as.4[, year := year(date)]
dt.as.4 %<>% st_as_sf(coords = c("x.coord", "y.coord"))

#- remove early years 
summary(dt.as.4$year)
dt.as.4 %<>% filter(year > 2012)

data <- bind_rows(dt.ctln, dt.as.4)

setDT(data)

data[, season := ifelse(month(date) %in% c(12,1,2), "winter", ifelse(month(date) %in% c(3,4,5), "spring", ifelse(month(date) %in% c(6,7,8), "summer", ifelse(month(date) %in% c(9,10,11), "autumn", "what?"))))]
any(data$season == "what?")
#- some entries (not many) have no date 
data <- data[season != "what?"]

data[taxon == "Cambaridae (P. clarkii)", taxon := "Procambarus clarkii"]
data[taxon == "Cambaridae (P. clarkii)", taxon := "Procambarus clarkii"]
data[taxon == "Cambaridae (Procambarus clarkii)", taxon := "Procambarus clarkii"]
data[taxon == "Cambaridae al·lòcton", taxon := "Cambaridae"]
data[taxon == "Coleopter sp1", taxon := "Coleopter"]
data[taxon == "Coleopter sp2", taxon := "Coleopter"]
data[taxon == "Corbiculidae (Corbicula fluminea)", taxon := "Corbicula fluminea"]
data[taxon == "Corbiculidae al·lòcton", taxon := "Corbiculidae"]
data[taxon == "Dípter no identificat", taxon := "Diptera"]
data[taxon %in% c("Hydrobiidae (P. antipodarum)", "Hydrobiidae (Potamopyrgus antipodarum)"), taxon := "Potamopygrus antipodarum"]
data <- data[taxon != "Sphaerotillus"]   
TU <- sort(unique(data$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

#- save to or load from file 
saveRDS(data, paste0("data/original data/monitoring_spain/auxilliary/01_",Sys.Date(),"_data2.rds"))
data <- readRDS("data/original data/monitoring_spain/auxilliary/01_2021-08-18_data2.rds")

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
fill.taxon.table("Aciculata"        , NA, NA, NA , "Eunicida", NA, "Polycheta", "Annelida")
fill.taxon.table("Actinedida"      , NA, NA, NA, NA, "Acari", "Arachnida", "Arthropoda")
fill.taxon.table("Archaeogastropoda", NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Basommatophora"   , NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Canalipalpata"    , NA, NA, NA, NA, NA, "Polycheta", "Annelida")
fill.taxon.table("CLADOCERA"        , NA, NA, NA, NA, NA, "Branchiopoda", "Arthropoda")
fill.taxon.table("COPEPODA"         , NA, NA, NA, NA, NA, "Hexanauplia", "Arthropoda")
fill.taxon.table("Ferrissidae"      , NA, NA, "Planorbidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Heterostropha"    , NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Larainae"         , NA, NA, "Elmidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Mesogastropoda"   , NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Neotaenioglossa"  , NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Seriata"          , NA, NA, NA, "Seriata", NA, "Turbellaria", "Platyhelminthes")
taxontable <- taxontable[original_name != "Sphaerotillus"]
data <- data[taxon != "Sphaerotillus"]        
        
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
data3[, date_id := .GRP, by = c("date")]

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_spain")]



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
        x.coord = st_coordinates(st_as_sf(data3))[,1],
        y.coord = st_coordinates(st_as_sf(data3))[,2],
        EPSG = 4326,
        data.set = "Monitorig data from Spain"
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
saveRDS(data4, paste0("data/original data/monitoring_spain/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/monitoring_spain/auxilliary/02_2021-08-18_data4.rds")

# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/monitoring_spain/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/monitoring_spain/auxilliary/03_2021-08-18_sites.rds")

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
saveRDS(distance_list, paste0("data/original data/monitoring_spain/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/monitoring_spain/auxilliary/04_2021-08-18_distance_to_brt.rds")

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
beepr::beep()
# ——————————— #
# ——— BGR ——— #
# ——————————— #
sites4 <- 
        sites3 |>
        st_transform(crs = st_crs(bgr)) |>
        st_join(select(bgr, code)) 

#- Distance to the closest border of an Biogeographical Region
sites4$bgr_distance <- sapply(1:nrow(sites), distance.to.bgr, sites4)
beepr::beep()
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

#- save to or load from file 
saveRDS(sites6, paste0("data/original data/monitoring_spain/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,  paste0("data/original data/monitoring_spain/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites6 <- readRDS("data/original data/monitoring_spain/auxilliary/06_2021-09-20_sites2.rds")
data5 <-  readRDS("data/original data/monitoring_spain/auxilliary/07_2021-09-20_data5.rds")

# SUMMARY STATISTICS ------------------------------------------------------------------------
uniqueN(sites6$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)

# samples 
# one factor 
data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
# two factor 
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
# three factors
data5 |> filter(year > 2004) |> filter(brt_distance <= 500) |> filter(fec.least.impacted)|> pull(gr_sample_id) |> uniqueN()

sites6 |> filter(brt_distance <= 500) |> count()
sites6 |> filter(fec.least.impacted) |> count()
sites6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()

# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)
for (i in 1:nrow(sites6)) {
        #for (i in 1:10){
        
        #- setup for a new list
        if (i == 1)
                new.lst <- list()
        
        #- subset to focal site
        lp.sub   <- data5[site_id == sites6$site_id[i]]
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
        lp.x     <- uniqueN(lp.sub$x.coord)
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
                                        "_combined_monitoring_spain"
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
                                                 "_combined_monitoring_spain")]
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
        
        lp.progress <- i/nrow(sites6) * 100
        print(lp.progress)
        rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
        rm(i)
        gc()
}

data6 <- rbindlist(new.lst,use.names=TRUE,fill =TRUE)
data6[, richness := .N, by = "gr_sample_id"]

# SUMMARY STATISTICS ----------------------------------------------------------------
data6[, uniqueN(gr_sample_id)]
data6[brt_distance <= 500, uniqueN(gr_sample_id)]
data6[fec.least.impacted == TRUE, uniqueN(gr_sample_id)]
data6[fec.least.impacted == TRUE & brt_distance <= 500, uniqueN(gr_sample_id)]

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data5, paste0("data/original data/monitoring_spain/",Sys.Date(),"_final.rds"))       
saveRDS(data6, paste0("data/original data/monitoring_spain/",Sys.Date(),"_final_aggregated.rds"))       

