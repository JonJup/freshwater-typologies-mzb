# ——————————————————————————— #
# ——— Clean data from GLD ——— # 
# ——————————————————————————— #

# ———————————————————————————————————
# date: 
#       21.07.21
# files in: 
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the raw data
#       provided by the GLD. 
# ————————————————

# setup -----------------------------------------------------------------------------
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
        xlsx
)

source("~/my documents/R/functions/fill_taxon_table.R")

# LOAD DATA -------------------------------------------------------------------------
samples <- fread("data/original data/gld/raw_data/gld_data/Fliess_Makrozoobenthos_Taxa.csv")
sites   <- fread("data/original data/gld/raw_data/gld_data/data.csv")
perlodes <- readRDS("data/original data/gld/auxilliary/perlodes_results.rds")
taxontable <- readRDS("data/original data/2021-08-04_taxontable.rds")
brt              <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies           <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr              <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec              <- st_read("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

#gloric           <- st_read  ("E://Arbeit/Data/GloRiC_v10/gloric_europe.gpkg")


# PREPARE DATA ----------------------------------------------------------------------
samples2 <-
        samples |>
        select(
                original_site_name = Mst_Nr_Bio,
                date = Datum,
                DV,
                taxon = Taxon,
                abundance = IZ
        ) %>%
        mutate(date = dmy(date)) |>
        mutate(year = year(date)) |> 
        mutate(season = ifelse(month(date) %in% c(12,1,2), "Winter", ifelse(month(date) %in% c(3,4,5), "spring" ,ifelse(month(date) %in% c(6,7,8), "summer", ifelse(month(date) %in% c(9,10,11), "autumn", NA))))) |> 
        filter(year > 2004) |>
        setDT()

samples2[, site_id := .GRP, by = original_site_name]
samples2[, date_id := .GRP, by = date]

#- add leading zeros 
samples2[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
samples2[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]
samples2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_gld")]

sites2 <- 
        sites |> 
        select(original_site_name = Mst_Nr_Bio, 
               x.coord = RW,
               y.coord = HW, 
               EPSG = UTM, 
               typ = Typ_LAWA,
               Nutzung
               )

data2 <- sites2[samples2, on = "original_site_name"]

#- save to or load from file - for perlodes 
saveRDS(data, paste0("data/original data/gld/auxilliary/01_",Sys.Date(),"_data.rds"))
data2 <- readRDS("data/original data/gld/auxilliary/01_2021-07-30_data.rds")

#- add perlodes results 
names(perlodes)[1] <- "gr_sample_id"
data2 <- perlodes[data2, on = "gr_sample_id"]

#- subset to least impacted sites 
#data2 <- data2[ÖZK < 3]

# TU <- unique(data2$taxon)
# (TU <- TU[which(!TU %in% taxontable$original_name)])

data2 <- data2[!taxon %in% c("Salmo trutta f. fario", "Cottus gobio", "Lampetra planeri", 
                             "Barbatula barbatula", "Gasterosteus aculeatus (Binnenform)", "Pungitius pungitius",
                             "Esox lucius", "Cobitis taenia", "Gasterosteus aculeatus", "Perca fluviatilis", "Gobio gobio",
                             "Barbus barbus", "Lota lota", "Tinca tinca", "Ameiurus nebulosus", "Misgurnus fossilis",
                             "Lampetra fluviatilis", "Silurus glanis", "Rutilus rutilus")]

data2[taxon == "Capnia/Zwicknia", taxon := "Capniidae"]
data2[taxon == "Hydroptila sparsa-Gruppe", taxon := "Hydroptila sparsa"]
data2[taxon == "Dixa maculata - Gruppe", taxon := "Dixa maculata"]
data2[taxon == "Leuctra hippopus-Gruppe", taxon := "Leuctra hippopus"]


#- clean up 
rm(perlodes)
gc()

#- save to or load from file 
saveRDS(data2, paste0("data/original data/gld/auxilliary/01_",Sys.Date(),"_data2.rds"))
data2 <- readRDS("data/original data/gld/auxilliary/01_2021-07-30_data2.rds")

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
fill.taxon.table("Chironomus (Camptochironomus)", NA, "Chironomus", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")

#- set clean to true for manually edited entries 
taxontable[, clean := TRUE]

unique(taxontable$kingdom)
unique(taxontable$phylum)   |> sort()
unique(taxontable$class)    |> sort()
unique(taxontable$subclass) |> sort()

#- save to or load from file 
saveRDS(taxontable, "data/original data/taxontable.rds")
taxontable <- readRDS("data/original data/taxontable.rds")

# prepare data 4 --------------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

unique(data3$EPSG)
# WGS 84 / UTM zone 32N
data3[,EPSG := 32632]


data4 <- data3[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
        season,
        german_type = typ,
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
        data.set = "monitoring data from German federal state Saxony Anhalt"
        
)]


data4 <- data4[!is.na(x.coord)]
#- combine entries of same taxon 

data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum,kingdom)))))))]


data4[is.na(lowest.taxon)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/gld/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/gld/auxilliary/02_2021-08-04_data4.rds")

# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
#mapview(sites)


#- save sites to file 
saveRDS (sites, paste0("data/original data/gld/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/gld/auxilliary/03_2021-07-30_sites.rds")

# ——————————————————————— #
# ——— DISTANCE TO BRT ——— #
# ——————————————————————— #

sites  <- st_transform(sites, crs = st_crs(brt))
nn     <- st_nearest_feature(sites, brt)
brt_nn <- brt[nn,]

distance_list <-
        map(.x = 1:nrow(sites),
            .f = ~ as.numeric(st_distance(x = sites[.x, ],
                                          y = brt_nn[.x, ])))
beepr::beep()
#- save to or load from file
saveRDS(distance_list, paste0("data/original data/gld/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/gld/auxilliary/04_2021-08-04_distance_to_brt.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "brt_distance" = unlist(distance_list),
        "brt20"    = brt_nn$m_btype20c,
        "brt12"    = brt_nn$m_btype12
)

sites <- distance_table[sites, on = "site_id"]
#sites <- sites[brt_distance <= 500]
#mapview(st_as_sf(sites))

# —————————————————————————— #
# ——— DISTANCE TO GLORIC ——— #
# —————————————————————————— #

# sites  <- st_as_sf(sites) |> st_transform(crs = st_crs(gloric))
# nn           <- st_nearest_feature(sites, gloric)
# gloric_nn    <- gloric[nn,]
# 
# distance_list <-
#         map(.x = 1:nrow(sites),
#             .f = ~ as.numeric(st_distance(x = sites[.x, ],
#                                           y = gloric_nn[.x, ])))
# beepr::beep()
# #- save to or load from file 
# saveRDS(distance_list, paste0("data/original data/gld/auxilliary/05_",Sys.Date(),"_distance_to_gloric.rds"))
# #distance_list  <- readRDS("data/original data/gld/auxilliary/05_2021-07-21_distance_to_gloric.rds")
# 
# #- create distance table 
# distance_table <- data.table(
#         "site_id" = sites$site_id,
#         "glroic_distance" = unlist(distance_list),
#         "gloric"    = gloric_nn$Kmeans_30
# )
# 
# sites <- distance_table[sites, on = "site_id"]
# sites <- sites[glroic_distance <= 500]
# mapview(st_as_sf(sites))

# ————————————————————— #
# ——— Illies  & BGR ——— #
# ————————————————————— #

sites <- 
        sites |> 
        st_as_sf() |> 
        st_transform(crs = st_crs(illies)) |> 
        st_join(select(illies, NAME)) |> 
        rename(illies = NAME)  

sites <- 
        sites |> 
        st_transform(crs = st_crs(bgr)) |>
        st_join(select(bgr, code)) |> 
        rename(bgr = code) 
sites <- 
        sites |> 
        st_transform(crs = st_crs(fec)) |>
        st_join(select(fec, least.impacted)) |> 
        rename(fec.least.impacted = least.impacted)
        
sites2 <- 
        sites |> 
        select(c("brt20", "brt12", "site_id", "illies", "german_type",  "bgr", "fec.least.impacted", "brt_distance"))  |> 
        st_drop_geometry() |> 
        setDT()


sites2[is.na(fec.least.impacted), fec.least.impacted := FALSE]

sites2[, brt12_illies := paste0(brt12, "_", illies)]
sites2[, brt12_bgr := paste0(brt12, "_", bgr)]

#data5 <- data4[site_id %in% sites$site_id]
data5 <- sites2[data4, on = "site_id"]

#- save to or load from file 
saveRDS(sites, paste0("data/original data/gld/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,       paste0("data/original data/gld/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites <- readRDS("data/original data/gld/auxilliary/06_2021-08_02_sites2.rds")
data5 <- readRDS("data/original data/gld/auxilliary/07_2021-08_02_data5.rds")

# TEMPORAL AGGREGATION --------------------------------------------------------------
data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness) 

for (i in 1:nrow(sites)) {
        
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
                                        "_combined_saxonmy_anhalt"
                                )]
                                lp.ls.season[[k]] <- lp.k.dat
                                next()
                        }
                }
                
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
                                                 "_combined_gld")]
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

data6 <- rbindlist(new.lst,use.names=TRUE,fill =T )
data6[, i.german_type := NULL]


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
saveRDS(data6, paste0("data/original data/gld/",Sys.Date(),"_final.rds"))
data6 <- readRDS("data/original data/gld/2021-08-04_final.rds")
