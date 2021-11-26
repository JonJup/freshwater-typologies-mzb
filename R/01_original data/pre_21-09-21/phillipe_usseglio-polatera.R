# —————————————————————————————————————————————————— #
# ——— Clean data from Phillipe Usseglio Polatera ——— # 
# —————————————————————————————————————————————————— #

# ———————————————————————————————————
# date: 
#       16.07.21
# files in: 
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the raw data
#       provided by Phillipe Usseglio Polatera. 
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
source("~/my documents/R/functions/fill_taxon_table.R")

# LOAD DATA  ------------------------------------------------------------------------

data       <- read_excel("data/original data/rcs/raw_data/data_invertebrate_GETREAL project-final version.xlsx", skip = 4)
taxontable <- readRDS("data/original data/2021-08-27_taxontable.rds")
brt        <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies     <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
her        <- st_read("E://Arbeit/Data/Hydroecoregions_france/Hydroecoregion2/Hydroecoregion2.shp")
bgr        <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# PREPARE DATA  ---------------------------------------------------------------------

#- Extract site IDs (List No) to add later. 
#- I will add this as late as possible to make changing this later easier. 
#- Pristine are sites were maximally one of the categorical quality variables is at or below "intermediate".
# pristine_id <- 
#         select(data, c(1, 15:29)) %>%
#         select_if(~class(.) == "character") %>% 
#         mutate(site = data$`List No`) %>% 
#         rename(
#                 OM = `Organic Matter`,
#                 N_tot = `Nitrogen compounds (except nitrates)`,
#                 NO_qual = Nitrates,
#                 P_tot = `Phosphorous compounds`,
#                 SM = `Suspended Matter`,
#         ) %>% 
#         mutate(OM      = as.numeric(factor(OM, levels = c("High", "Good", "Intermediate", "Poor", "Bad"))),
#                N_tot   = as.numeric(factor(N_tot, levels = c("High", "Good", "Intermediate", "Poor", "Bad"))),
#                NO_qual = as.numeric(factor(NO_qual, levels = c("High", "Good", "Intermediate", "Poor", "Bad"))),
#                P_tot = as.numeric(factor(P_tot, levels = c("High", "Good", "Intermediate", "Poor", "Bad"))),
#                SM      = as.numeric(factor(SM, levels = c("High", "Good", "Intermediate", "Poor", "Bad"))),
#                `Organic Micropollutants (other)` = as.numeric(factor(`Organic Micropollutants (other)`, levels = c("High", "Good", "Intermediate", "Poor", "Bad"))),
#                `Mineral micropolluants (metal)` = as.numeric(factor(`Mineral micropolluants (metal)`, levels = c("High", "Good", "Intermediate", "Poor", "Bad"))),
#                Pesticides = as.numeric(factor(Pesticides, levels = c("High", "Good", "Intermediate", "Poor", "Bad"))),
#                PAH = as.numeric(factor(PAH, levels = c("High", "Good", "Intermediate", "Poor", "Bad")))
#         ) %>% 
#         select(!c("Acidification")) %>% 
#         mutate(across(!site, ~ .x-2)) %>% 
#         mutate(across(!site, ~ ifelse(.x < 0, 0, .x))) %>% 
#         mutate(stressors = rowSums(.,na.rm = T)) %>%
#         mutate(stressors = stressors - site) %>%
#         select(site, stressors) %>%
#         setDT()
# 
# pristine_id = pristine_id[stressors <= 1, site]
# 
#- to data tale
setDT(data)

# ——— SAMPLES 1 ——— #

data[, date := ymd(`Date (D/M/Y)`)]
data[, `:=` 
     (year = year(date), 
       season = ifelse(month(date) %in% c(12,1,2), "winter", 
                       ifelse(month(date) %in% c(3,4,5), "spring", 
                              ifelse(month(date) %in% c(6,7,8), "summer", "autumn")
                       )
       )
     )
]

samples <- data[,.SD, .SDcols = c(1,33:381)]
site    <- data[,.SD, .SDcols = c(1,5,6,382:384)]

samples2 <- melt(samples, 
                id.vars = "List No", 
                variable.name = "taxon")

samples2 = samples2[value != 0]

data2 = samples2[site, on = "List No"]
data2$taxon = as.character(data2$taxon)
data2[,value := NULL]

data2[, c("data.set", "EPSG",  "abundance") := .("phillipe_ussseglio-polatera", 27572, NA)]

data2 %<>% rename(x.coord = "Coordinate (xl2)", 
                  y.coord = "Coordinate (yl2)",
                  original_site_name = "List No")

data2[,taxon := str_remove(taxon, "\\.\\.\\.[0-9]+")]
data2[,taxon := str_replace(taxon, "\\_", "\\ ")]     
data2[taxon %in% c("Triaenodes/Ylodes"), taxon := "Leptoceridae"]

#- clean up 
rm(data, samples, samples2, site)
gc()

#- save to or load from file 
saveRDS(data2, paste0("data/original data/rcs/auxilliary/01_",Sys.Date(),"_data.rds"))
data2 <- readRDS("data/original data/rcs/auxilliary/01_2021-07-26_data.rds")


# TAXONOMY --------------------------------------------------------------------------
#- first look at taxa 
(TU <- sort(unique(data2$taxon)))

#data2[str_detect(taxon, "/"), unique(taxon)]

#- which ones are not yet in taxa table
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

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

fill.taxon.table("Anostraces",             NA,            NA,               NA,   "Ansotraca", NA, "Branchiopoda", "Arthropoda" )
fill.taxon.table("Copelatinae",            NA,            NA,     "Dytiscidae",  "Coleoptera", NA, "Insecta"   , "Arthropoda")
fill.taxon.table("Nemathelmintha",         NA,            NA,               NA,            NA, NA,           NA,         NA)
fill.taxon.table("Physa stricto-sensu",    NA,       "Physa",       "Physidae",            NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Rhyacophila lato-sensu", NA, "Rhyacophila", "Rhyacophilidae", "Trichoptera", NA, "Insecta"   , "Arthropoda")
fill.taxon.tabke("Nemathelmintha", NA, NA, NA, NA, NA, )

#- set clean to true for manually edited entries 
taxontable[, clean := TRUE]

unique(taxontable$kingdom)
unique(taxontable$phylum)   |> sort()
unique(taxontable$class)    |> sort()
unique(taxontable$subclass) |> sort()

#- save to or load from file 
saveRDS(taxontable, "data/original data/taxontable.rds")



# prepare data 4 --------------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

sort(unique(data3$kingdom))
data3[kingdom == "", unique(original_name)]
data3 <- data3[original_name != "Nemathelmintha"]
sort(unique(data3$phylum))
sort(unique(data3$class))

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_rcs")]

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
        data.set = "rcs"
        
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

data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- drop impacted sites
#data4 <- data4[original_site_name %in% pristine_id]

#- save to or load from file 
saveRDS(data4, paste0("data/original data/rcs/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/rcs/auxilliary/02_2021-08-02_data4.rds")


# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/rcs/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/rcs/auxilliary/03_2021-08-02_sites.rds")

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
saveRDS(distance_list, paste0("data/original data/rcs/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/rcs/auxilliary/04_2021-08-02_distance_to_brt.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "brt_distance" = unlist(distance_list),
        "brt20"    = brt_nn$m_btype20c,
        "brt12"    = brt_nn$m_btype12
)

sites <- distance_table[sites, on = "site_id"]
#sites <- sites[brt_distance <= 500]
#mapview(st_as_sf(sites.close))

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
# saveRDS(distance_list, paste0("data/original data/rcs/auxilliary/05_",Sys.Date(),"_distance_to_gloric.rds"))
# distance_list  <- readRDS("data/original data/rcs/auxilliary/05_2021-07-20_distance_to_gloric.rds")
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
# #mapview(st_as_sf(sites.close))

# —————————————— #
# ——— Illies ——— #
# —————————————— #

sites <- 
        sites |> 
        st_as_sf() |> 
        st_transform(crs = st_crs(illies)) |> 
        st_join(select(illies, NAME)) 

sites <- 
        sites |>
        st_as_sf() |>
        st_transform(crs = st_crs(bgr)) |>
        st_join(select(bgr, code))


sites <- 
        sites |>
        st_as_sf() |>
        st_transform(crs = st_crs(fec)) |>
        st_join(select(fec, least.impacted))

# ——————————— #
# ——— HER ——— #
# ——————————— #

valid.id <- st_is_valid(her)
her.valid <- her[valid.id, ]
her.invalid <- her[!valid.id, ]
her.invalid %<>% st_make_valid()
her2 <- bind_rows(her.valid, her.invalid)

sites <- 
        sites |> 
        st_transform(crs = st_crs(her2)) |> 
        st_join(select(her2, NomHER2)) 


sites2 <-
        sites |> 
        rename(illies = NAME, bgr = code, fec.least.impacted = least.impacted, her = NomHER2) %>%
        select(c("brt20", "brt12", "site_id", "illies", "bgr", "fec.least.impacted", "brt_distance", "her")) %>%
        st_drop_geometry() |> 
        setDT()

sites2[, brt12_illies := paste0(brt12, "_", illies)]
sites2[, brt12_bgr := paste0(brt12, "_", bgr)]

sites2[, c() := NULL]

data5 <- sites2[data4, on = "site_id"]

#- save to or load from file 
saveRDS(sites2, paste0("data/original data/rcs/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,       paste0("data/original data/rcs/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites <- readRDS("data/original data/rcs/auxilliary/06_2021-08-05_sites2.rds")
data5 <- readRDS("data/original data/rcs/auxilliary/07_2021-08-05_data5.rds")


# SUMMARY STATISTIC -----------------------------------------------------------------
summary(data5$year)
sites |> nrow()
sites |> filter (brt_distance <= 500) |> nrow()
sites |> filter (fec.least.impacted) |> nrow()
sites |> filter (fec.least.impacted) |> filter (brt_distance <= 500) |> nrow()

data5[, uniqueN(gr_sample_id)]


# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)

#- no temporal aggregation necessary. 
#- minimal richnesses are ok. 

# distance to region border ---------------------------------------------------------
sites <- unique(data5, by = "site_id")
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
data5 <- sites[data5, on = "site_id"]

# SAVE TO FILE ---------------------------------------------------------------------
saveRDS(data5, paste0("data/original data/rcs/",Sys.Date(),"_final.rds"))
data5 <-      readRDS("data/original data/rcs/2021-08-05_final.rds")
