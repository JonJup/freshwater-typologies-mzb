# ----------------------------------------------------- #
# -------- Clean Croatian Monitoring data ------------- # 
# ----------------------------------------------------- #


# --------------------------------------------------------------------------------------------------------
# date: 
#       20.09.21
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the Croatian monitoring data 
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


# load data -------------------------------------------------------------------------

taxa  <- read_xlsx("data/original data/monitoring_croatia//raw/Copy of Baza MZB MED GIG.xlsx") 



taxontable <- readRDS("data/original data/2021-09-03_taxontable.rds")

brt        <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies     <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr        <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# prepare data ----------------------------------------------------------------------


names(taxa)
names(sites)
names(sites)[1] <- "eventID"
all(taxa$eventID %in% sites$eventID)
data <- sites[taxa, on = "eventID"]
names(data)
sort(table(data$samplingProtocol))
#- keep only handnet (=kicknet) samples 
data <- data[samplingProtocol == "Handnet"]

sort(unique(data$habitat))

data <- data[habitat %in% c("RtNt:River;Water body", "BkK:Small stream Kempen;Water body", "Bk:Small stream;Water body", "Bg:Large stream;Water body", "BgK:Large stream Kempen;Water body",
                            "Rk:Small river;Water body", "Rg:Large river;Water body", "No type:ditch;Water body", "Rzg:Very large river;Water body")]

data2 <- data.table(
        original_site_name = data$locationID,
        date               = ymd(data$eventDate),
        taxon              = data$scientificName,
        x.coord = data$verbatimLongitude,
        y.coord  = data$verbatimLatitude,
        EPSG = 31370,
        data.set = "monitoring_belgium",
        abundance = 1,
        countryCode = data$countryCode
)

data2 |> 
        unique(by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) -> 
        sites 
mapview(sites)


data2[, `:=` 
     (year = year(date), 
             season = ifelse(month(date) %in% c(12,1,2), "winter", 
                             ifelse(month(date) %in% c(3,4,5), "spring", 
                                    ifelse(month(date) %in% c(6,7,8), "summer", "autumn")
                             )
             )
     )
]
#- check that each site only has set of coordinates 
all(data2[,uniqueN(x.coord), by = "original_site_name"]$V1 == 1)
all(data2[,uniqueN(y.coord), by = "original_site_name"]$V1 == 1)


TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

#- clean up taxon names 
#- remove all brackets 

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


saveRDS(taxontable, paste0("data/original data/",Sys.Date(),"_taxontable.rds"))
# COMBINE DATA SETS -----------------------------------------------------------------

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_monitoring_belgium")]



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
        EPSG,
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
saveRDS(data4, paste0("data/original data/monitoring_belgium/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/monitoring_belgium/auxilliary/02_2021-09-03_data4.rds")

# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/monitoring_belgium/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/monitoring_belgium/auxilliary/03_2021-08-18_sites.rds")

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
saveRDS(distance_list, paste0("data/original data/monitoring_belgium/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
#distance_list  <- readRDS("data/original data/monitoring_belgium/auxilliary/04_2021-08-18_distance_to_brt.rds")

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
saveRDS(sites6, paste0("data/original data/monitoring_belgium/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,  paste0("data/original data/monitoring_belgium/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites6 <- readRDS("data/original data/monitoring_belgium/auxilliary/06_2021-08-18_sites2.rds")
data5 <- readRDS("data/original data/monitoring_belgium/auxilliary/07_2021-08-18_data5.rds")


# SUMMARY STATISTICS ------------------------------------------------------------------------
uniqueN(sites6$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)

# samples 
# one factor 
data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(year >= 2004)  |> pull(gr_sample_id) |> uniqueN()
# two factor 
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(year >= 2004) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(year >= 2004) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
# three factors
data5 |> filter(year > 2004) |> filter(brt_distance <= 500) |> filter(fec.least.impacted)|> pull(gr_sample_id) |> uniqueN()

sites6 |> filter(brt_distance <= 500) |> count()
sites6 |> filter(fec.least.impacted) |> count()
data5  |> filter(year >=2004) |> pull(site_id) |> uniqueN()
sites6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
data5  |> filter(year >=2004 & brt_distance <= 500) |> pull(site_id) |> uniqueN()
data5  |> filter(year >=2004 & brt_distance <= 500 & fec.least.impacted) |> pull(site_id) |> uniqueN()



# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

data5.save <- copy(data5)
data5 <- data5[year >=2004]
sites6 <- sites6[site_id %in% data5$site_id]
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
                                        "_combined_monitoring_belgium"
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
                                                 "_combined_monitoring_belgium")]
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
saveRDS(data6, paste0("data/original data/monitoring_belgium/",Sys.Date(),"_final_aggregated.rds")) 
saveRDS(data5.save, paste0("data/original data/monitoring_belgium/",Sys.Date(),"_final.rds")) 
