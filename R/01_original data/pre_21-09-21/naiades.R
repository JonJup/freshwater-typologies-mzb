# ——————————————————————————————— #
# ——— Clean data from Naiades ——— # 
# ——————————————————————————————— #

# ———————————————————————————————————
# date first written: 
#       16.07.21
# date last modified: 
#       14.09.21
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the raw data
#       provided from Naiades. 
# ————————————————

# SETUP -----------------------------------------------------------------------------

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
#- functions 
source("R/distance_to_illies_function.R")
source("R/distance_to_bgr_function.R")
source("R/fill_taxon_table.R")

# LOAD DATA  ------------------------------------------------------------------------
fauna_table <- fread("data/original data/naiades/raw_data/fauneflore.csv")
geo_table   <- fread("data/original data/naiades/raw_data/stations.csv")
#chem_table  <- fread("data/original data/naiades/raw_data/resultat.csv")

taxontable  <- readRDS("data/original data/2021-09-13_taxontable.rds")

brt         <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies      <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
her         <- st_read("E://Arbeit/Data/Hydroecoregions_france/Hydroecoregion2/Hydroecoregion2.shp")
bgr         <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# PREPARE DATA  ---------------------------------------------------------------------

#- The variable LbSupport from fauna table contains the tells us what kind of observation is in the row.
#- There are diatoms, fishes, macroinvertebrates, macrophytes and phytoplancton.
unique(fauna_table$LbSupport)
#- We subset fauna_table to macroinvertebrate observations. 
fauna_table <- fauna_table[LbSupport == unique(fauna_table$LbSupport)[3] & MnTypTaxRep == "NbrTax"]
#- check results 
unique(fauna_table$LbSupport)
#- join macroinvertebrate data with station data in geo_table. The latter contains station coordiantes.  
fauna_table <- geo_table[fauna_table, on = "CdStationMesureEauxSurface"]
data        <- fauna_table[,
             list(
                     "original_site_name" = CdStationMesureEauxSurface,
                     "date"               = ymd(DateDebutOperationPrelBio),
                     "taxon"              = NomLatinAppelTaxon,
                     "x.coord"            = CoordXStationMesureEauxSurface,
                     "y.coord"            = CoordYStationMesureEauxSurface,
                     "EPSG"               = LibelleProjection,
                     "abundance"          = RsTaxRep
             )
]

data[,c("year", "season") := .(year(date), case_when(month(date) %in% c(12,1,2) ~ "winter",
                                                     month(date) %in% c(3,4,5) ~ "spring",
                                                     month(date) %in% c(6,7,8) ~ "summer",
                                                     month(date) %in% c(9,10,11) ~ "autumn"))]

#- The data set contains data in four different coordinate reference systems. 
#- Most data are RGF93 / Lambert 93 (EPSG: 2154). These are the only data we will keep. 
#- The remaining data are in French Guyana (CRS: RGFG95 / UTM 22; EPSG: 2972), 
#- Fort-de-France (a Caribbean island; CRS: RRAF 91 (WGS84) / UTM 20; EPSG: 2989) or are lacking 
#- spatial coordinates and hence also a reference system. 

data <- data[EPSG == "RGF93 / Lambert 93"]
data[, c("EPSG", "data.set") := .(2154, "naiades")]
data[, EPSG := as.numeric(EPSG)]
data2 <- data

data2[taxon == "Triaenodes/Ylodes", taxon := "Leptoceridae"]
data2[taxon == "Chaetopteryx villosa/fusca", taxon := "Chaetopteryx"]

#- some taxa seem to have received a wrong tag. These are not invertebrates 
data2 <- data2[taxon != "Appellation de Taxon inconnue"]
data2 <- data2[taxon != "Marsilea quadrifolia"]
data2 <- data2[taxon != "Elodes palustris"]
data2 <- data2[taxon != "Merluccius merluccius"]
data2 <- data2[taxon != "Code gelÃ© 1999 (nematomorphes)"]
data2 <- data2[taxon != "Hantzschia amphioxys var. vivax"]
data2 <- data2[taxon != "Nemathelmintha"]

#- check sites on map 
# data2 |> 
#         unique(by = "original_site_name") |> 
#         st_as_sf(coords = c("x.coord", "y.coord"), crs = 2154) |> 
#         mapview()


#- first look at taxa 
TU <- sort(unique(data2$taxon))
#- which ones are not yet in taxa table
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

#- clean up 
rm(data, fauna_table, geo_table, new_tu)
gc()

#- save to or load from file 
saveRDS(data2, paste0("data/original data/naiades/auxilliary/01_",Sys.Date(),"_data.rds"))
data2 <- readRDS("data/original data/naiades/auxilliary/01_2021-08-27_data.rds")


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
taxontable <- taxontable[original_name != "Appellation de Taxon inconnue"]
taxontable <- taxontable[original_name != "Marsilea quadrifolia"]
taxontable <- taxontable[original_name != "Elodes palustris"]
taxontable <- taxontable[original_name != "Merluccius merluccius"]


fill.taxon.table("Agapetus-synagapetus"              , NA                 ,        NA, "Glossosomatidae", "Trichoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Anophelinae"                       , NA                 ,        NA,       "Culicidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Asellus (Asellus) aquaticus"       , "Asellus aquaticus", "Asellus",       "Asellidae",     "Isopoda",  NA, "Malacostraca", "Arthropoda" )
fill.taxon.table("Conchostraca"                      , NA                 ,        NA,                NA,            NA,  NA, "Branchiopoda", "Arthropoda" )
fill.taxon.table("Corynoneurinae"                    , NA                 ,        NA,    "Chironomidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Dasyheleinae"                      , NA                 ,        NA, "Ceratopogonidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Dicosmoecinae"                     , NA                 ,        NA,   "Limnephilidae", "Trichoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Drusini"                           , NA                 ,        NA,   "Limnephilidae", "Trichoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Dysticinae"                        , NA                 ,        NA,      "Dytiscidae",  "Coleoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Eriopterini"                       , NA                 ,        NA,      "Limoniidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Forcypomyiinae"                    , NA                 ,        NA, "Ceratopogonidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Hexatomini"                        , NA                 ,        NA,      "Limoniidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Leptoconopinae"                    , NA                 ,        NA, "Ceratopogonidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Limnebiidae"                       , NA                 ,        NA,     "Hydraenidae",  "Coleoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Limoniini"                         , NA                 ,        NA,      "Limoniidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Pediciini"                         , NA                 ,        NA,      "Limoniidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Pericarides"                       , NA                 ,        NA,                NA,            NA,  NA, "Malacostraca", "Arthropoda" )
fill.taxon.table("Planipennia"                       , NA                 ,        NA,                NA,  "Neuroptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Podonominae"                       , NA                 ,        NA,    "Chironomidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Prosimuliinae"                     , NA                 ,        NA,      "Simuliidae",     "Diptera",  NA,      "Insecta", "Arthropoda")  
fill.taxon.table("Prosobranchia"                     , NA                 ,        NA,                NA,            NA,  NA,   "Gastropoda", "Mollusca")
fill.taxon.table("Silo-lithax"                       , NA                 ,        NA,        "Goeridae", "Trichoptera",  NA,      "Insecta", "Arthropoda")
fill.taxon.table("Simuliini"                         , NA                 ,        NA,      "Simuliidae",     "Diptera",  NA,      "Insecta", "Arthropoda")  
fill.taxon.table("Stenophylacini-chaetopterygini"    , NA                 ,        NA,   "Limnephilidae", "Trichoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Tubificinae avec soies capillaires", NA                 ,        NA,        "Naididae", "Haplotaxida",  NA,   "Clitellata", "Annelida")
fill.taxon.table("Tubificinae sans soies capillaires", NA                 ,        NA,        "Naididae", "Haplotaxida",  NA,   "Clitellata", "Annelida")
fill.taxon.table("Unionacea"                         , NA                 ,        NA,                NA,    "Unionida",  NA,     "Bivalvia", "Mollusca")
fill.taxon.table("Elodea"          , NA, NA, "Scirtidae", "Coleoptera",  NA,      "Insecta", "Arthropoda" )

taxontable[original_name == "Nematoda  Gen.sp..", c("class", "phylum") := .(NA, "Nematoda")]

taxontable[order == "Ansotraca", order := "Anostraca"]
taxontable[order == "Doptera", order := "Diptera"]
#- set clean to true for manually edited entries 
taxontable[, clean := TRUE]

taxontable[kingdom == "Chromista"]
taxontable[phylum == "Chordata"]

unique(taxontable$kingdom)
unique(taxontable$phylum)   |> sort()
unique(taxontable$class)    |> sort()
unique(taxontable$subclass) |> sort()
unique(taxontable$order)    |> sort()
unique(taxontable$family)    |> sort()

# - save to file 
saveRDS(taxontable, paste0("data/original data/",Sys.Date(),"_taxontable.rds"))

# COMBINE DATA AND TAXONOMY --------------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

sort(unique(data3$kingdom))
sort(unique(data3$phylum))
sort(unique(data3$class))
sort(unique(data3$subclass))
sort(unique(data3$order))

data3 <- data3[kingdom != "Chromista"]


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
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_naiades")]

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

data4 <- data4[!is.na(x.coord)]

#- combine entries of same taxon 
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]

data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/naiades/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/naiades/auxilliary/02_2021-08-27_data4.rds")

# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
#nrow(sites)
#mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/naiades/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/naiades/auxilliary/03_2021-07-26_sites.rds")

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
saveRDS(distance_list, paste0("data/original data/naiades/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/naiades/auxilliary/04_2021-08-27_distance_to_brt.rds")

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

sites3$illies_distance <- sapply(1:nrow(sites), distance.to.illies, sites3)
beepr::beep()
# —————————————— #
# ——— BGR    ——— #
# —————————————— #

sites4 <- 
        sites3 |>
        st_transform(crs = st_crs(bgr)) |>
        st_join(select(bgr, code)) 

#- Distance to the closest border of an Biogeographical Region
sites4$bgr_distance <- sapply(1:nrow(sites4), distance.to.bgr, sites4)

# —————————————— #
# ——— FEC    ——— #
# —————————————— #

sites5 <- 
        sites4|>
        st_as_sf() |>
        st_transform(crs = st_crs(fec)) |>
        st_join(select(fec, least.impacted)) 

# —————————————— #
# ——— HER    ——— #
# —————————————— #

valid.id <- st_is_valid(her)
her.valid <- her[valid.id, ]
her.invalid <- her[!valid.id, ]
her.invalid %<>% st_make_valid()
her2 <- bind_rows(her.valid, her.invalid)

sites6 <- 
        sites5 |> 
        st_transform(crs = st_crs(her2)) |> 
        st_join(select(her2, NomHER2)) 

sites7 <-
        sites6 |> 
        rename(illies = NAME,
               her = NomHER2,
               bgr = code,
               fec.least.impacted = least.impacted) %>%
        select(c("brt20", "brt12", "site_id", "illies", "bgr", "fec.least.impacted", "brt_distance", "her", "illies_distance", "bgr_distance")) %>%
        st_drop_geometry() %>% 
        setDT

data5 <- sites7[data4, on = "site_id"]

#- save to or load from file 
saveRDS(sites6, paste0("data/original data/naiades/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,  paste0("data/original data/naiades/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites6 <- readRDS("data/original data/naiades/auxilliary/06_2021-08-27_sites2.rds")
data5 <- readRDS("data/original data/naiades/auxilliary/07_2021-08-27_data5.rds")


# Least impacted sites --------------------------------------------------------------
# gr_sites <- unique(data5, by = "gr_sample_id")
# 
# i2m2 <-  chem_table[LbSupport == unique(chem_table$LbSupport)[2] & LbLongParametre == "I2M2"]
# i2m2 <- i2m2[, c("CdStationMesureEauxSurface", "DateDebutOperationPrelBio", "ResIndiceResultatBiologique")]
# 
# names(i2m2) <- c("site.name", "date", "score")
# i2m2[, date := ymd(date)]
# 
# gr_sites$score = 0 
# 
# for (i in 1:nrow(gr_sites)) { 
#         
#         if (gr_sites$score[i] != 0)
#                 next()
#         
#         lp.id <- gr_sites$original_site_name[i]
#         lp.dt <- gr_sites$date[i]
#         
#         lp.i2m2 <- i2m2[site.name == lp.id]
#         if (nrow(lp.i2m2) == 0) 
#                 next()
#         lp.diff.time <- lp.i2m2$date - lp.dt
#         if (any(lp.diff.time == 0)){
#                 lp.i2m2 <- lp.i2m2[which(lp.diff.time == 0), ]
#                 gr_sites$score[i] <- lp.i2m2$score
#         } else {
#                 gr_sites$score[i] <- mean(lp.i2m2$score)  
#         }        
#         
#         
#         if (i %% 100 == 0)
#                 print(paste(i, "@", Sys.time()))
#         
#         rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
#         rm(i)
#         gc()
#         
# }
# 
# 
# saveRDS(gr_sites, "data/original data/naiades/auxilliary/11_results_i2m2.rds")
# gr_sites <- readRDS("data/original data/naiades/auxilliary/11_results_i2m2.rds")
# 
# gr_sites |> 
#         filter(score > 0.7) |> 
#         st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1]) -> quickplot 
# mapview(quickplot)


# -----------------------------------------------------------------------------------

# SUMMARY STATISTICS ----------------------------------------------------------------
summary(data5$year)
sites6 |> nrow()
sites6 |> filter(brt_distance <= 500) |> nrow()
sites6 |> filter(fec.least.impacted)  |> nrow()
data5  |> filter(year >= 2004)        |> unique(by="site_id") |> nrow()
sites6 |> filter(brt_distance <= 500 & fec.least.impacted) |> nrow()
data5  |> filter(year >= 2004)        |> unique(by="site_id") |> filter(brt_distance <= 500 & fec.least.impacted) |> nrow()

data5[, uniqueN(gr_sample_id)]
data5[brt_distance <= 500, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE, uniqueN(gr_sample_id)]
data5[year >= 2004, uniqueN(gr_sample_id)]
data5[brt_distance <= 500 & year >= 2004, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE & year >= 2004, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE & brt_distance <= 500, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE & brt_distance <= 500 & year >= 2004, uniqueN(gr_sample_id)]

# save to file ----------------------------------------------------------------------

data6 <- data5[year>=2005]

# saveRDS (data6, paste0("data/original data/naiades/",Sys.Date(),"_final_mzb_data.rds"))   
# data6 <- readRDS("data/original data/naiades/2021-07-23_final_mzb_data.rds")

sites <- unique(data6, by = "site_id")

data6[, sample_events := uniqueN(gr_sample_id), by = "site_id"]
data6[, richness := .N, by = "gr_sample_id"]
table(data6$sample_events)

for (i in 1:nrow(sites)) {
        #for (i in 1:10){
        
        #- setup for a new list
        if (i == 1)
                new.lst <- list()
        
        #- subset to focal site
        lp.sub   <- data6[site_id == sites$site_id[i]]
        #- how many sampling events have taken place here?
        lp.n     <- unique(lp.sub$sample_events)
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
        lp.x     <- lp.sub[, uniqueN(x.coord)]
                # lp.sub |> 
                # st_as_sf() |> 
                # st_coordinates() |> 
                # as.data.frame() |> 
                # dplyr::select(X) |> 
                # uniqueN()
        
        lp.y     <- lp.sub[, uniqueN(y.coord)]
                # lp.sub |> 
                # st_as_sf() |> 
                # st_coordinates() |> 
                # as.data.frame() |> 
                # dplyr::select(Y) |> 
                # uniqueN()
        
        
        if (!(lp.x == 1 & lp.y == 1)) {
                print(paste("in", i, ", there are more than two coordiantes"))
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
                                lp.k.dat <- rbindlist(list(lp.rest, lp.combine), fill = TRUE)
                        }
                        
                        
                        lp.k.n <- uniqueN(lp.k.dat$gr_sample_id)
                        lp.k.dat$sample_events <- lp.k.n
                        
                        if (lp.k.n == 1) {
                                #- drop date
                                lp.k.dat[, date := NA]
                                lp.k.dat[, gr_sample_id := paste0(
                                        site_id,
                                        "_",
                                        c("spring", "summer", "autumn", "winter")[k],
                                        "_combined_naiades"
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
                                                 "_combined_naiades")]
                #- drop date
                lp.sub2[, date := NA]
                #- keep year if both were from the same year.
                lp.sub2[, year := ifelse(uniqueN(year) == 1, year, NA)]
                #- add to list
                lp.ls.season[[k]] <- lp.sub2
                
        }
        
        lp.ls.season <- lp.ls.season[lp.season.id]
        lp.ls.season <- rbindlist(lp.ls.season, fill = TRUE)
        new.lst[[i]] <- lp.ls.season
        
        lp.progress <- i/nrow(sites) * 100
        print(lp.progress)
        rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
        rm(i)
        gc()
}
#- combine loop output 
data6 <- rbindlist(new.lst,use.names=TRUE)
#- update species richness per sample 
data6[, richness := .N, by = "gr_sample_id"]

data6[, brt12_illies := paste0(brt12, "_", illies)]
data6[, brt12_bgr := paste0(brt12, "_", bgr)]

# SUMMARY STATISTICS ----------------------------------------------------------------
data6[, uniqueN(gr_sample_id)]
data6[brt_distance <= 500, uniqueN(gr_sample_id)]
data6[fec.least.impacted == TRUE, uniqueN(gr_sample_id)]
data6[fec.least.impacted == TRUE & brt_distance <= 500, uniqueN(gr_sample_id)]

# save to file ----------------------------------------------------------------------
saveRDS (data5, paste0("data/original data/naiades/",Sys.Date(),"_final.rds"))  
saveRDS (data6, paste0("data/original data/naiades/",Sys.Date(),"_final_aggregated.rds"))  
data6 <- readRDS("data/original data/naiades/2021-08-02_final.rds")
