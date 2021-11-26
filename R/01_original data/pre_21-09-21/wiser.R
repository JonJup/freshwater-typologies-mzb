# ———————————————————————————— #
# ——— Clean WISER MZB data ——— # 
# ———————————————————————————— #


# ———————————————————————————————————
# date: 
#       14.07.21
# Project:
#       Evaluating European Broad River Types with Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the raw data
#       provided from WISER by Christian Feld. 
# ————————————————


# 01. Setup -------------------------------------------------------------------

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

# - functions 
source("R/distance_to_illies_function.R")
source("R/distance_to_bgr_function.R")
source("R/fill_taxon_table.R")

# load data -------------------------------------------------------------------------
samples <- read_excel("data/original data/wiser/raw_data/WISER_Invertebrate_taxa.xlsx") 
sites   <- read_excel("data/original data/wiser/raw_data/WISER_Metadata_Abiotics.xls") 
taxontable <- readRDS("data/original data/2021-08-27_taxontable.rds")
brt      <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies   <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr      <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
typology_germany <- st_read("E://Arbeit/Data/WFD_waterbodytypes_germany/AM_surfaceWaterBody-DE_GDB/AM_surfaceWaterBody-DE.gdb", layer = "AM_riverWaterBody_DE")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# prepare data ----------------------------------------------------------------------
setDT(samples)
setDT(sites)

samples <- samples[, list(
        original_site_name = StationCode, 
        date = ymd(InvSampleDate),
        year = year(InvSampleDate),
        season = ifelse(
                month(InvSampleDate) %in% c(12,1,2), "winter", 
                ifelse(month(InvSampleDate) %in% c(3,4,5), "spring", 
                       ifelse(month(InvSampleDate) %in% c(6,7,8), "summer", "autumn"))),
        taxon = InvTaxonName_ad,
        abundance = InvAbundance
)]


sites <- sites[, list(
        original_site_name = StationCode, 
        x.coord = Longitude, 
        y.coord = Latitude,
        EPSG = 4326,
        data.set = "wiser"
)]

#- join sites and sample data 
data <- sites[samples, on = "original_site_name"]

#- taxomomy 
data[, taxon := str_remove_all(taxon, "\\-Gr\\.")                 ] 
data[, taxon := str_remove_all(taxon, "\\ ssp\\.$")               ]   
data[, taxon := str_remove_all(taxon, "\\ sp\\.$")                ] 
data[, taxon := str_remove_all(taxon, "\\ Gen\\.$")               ] 
data[, taxon := str_remove_all(taxon, "\\ Ad\\.$")                ] 
data[, taxon := str_remove_all(taxon, "\\ Lv\\.$")                ] 
data[, taxon := str_remove_all(taxon, "\\-Agg\\.$")               ] 
data[, taxon := str_remove_all(taxon, "\\-Agg\\.$")               ] 
data[, taxon := str_remove_all(taxon, "\\ \\(Hyperrhyacophila\\)")] 
data[, taxon := str_remove_all(taxon, "\\ \\(Rhyacophila\\)")     ] 
data[, taxon := str_remove_all(taxon, "\\ \\(Holotanypus\\)")     ] 
data[, taxon := str_remove_all(taxon, "\\ \\(Psilotanypus\\)")    ] 
data[, taxon := str_remove_all(taxon, "\\ sp\\.$")                ] 
data[, taxon := str_remove_all(taxon, "\\ \\(Glyptotendipes\\)")  ] 
data[, taxon := str_remove_all(taxon, "\\ \\-Gr\\.")              ] 
data[, taxon := str_remove_all(taxon, "\\-Gr\\.")                 ] 
data[, taxon := str_remove_all(taxon, "\\ ssp\\.$")               ]
data[, taxon := str_replace_all(taxon, pattern = "Nemoura/Nemurella", replacement = "Nemouridae")]
data[, taxon := str_replace_all(taxon, pattern = "Jungiella/Psychoda/Tinearia", replacement = "Psychodidae")]
data[, taxon := str_replace_all(taxon, pattern = "Anisus leucostoma/spirorbis", replacement = "Anisus")]
data[, taxon := str_replace_all(taxon, pattern = "Conchapelopia/Arctopelopia", replacement = "Chironomidae")]
data[, taxon := str_replace_all(taxon, pattern = "Haliplus \\(Haliplus\\)", replacement = "Haliplus")]
data[, taxon := str_replace_all(taxon, pattern = "Haliplus \\(Haliplus\\)", replacement = "Haliplus")]
data[, taxon := str_replace_all(taxon, pattern = "Helophorus aequalis/aquaticus", replacement = "Helophorus aequalis")]
data[, taxon := str_replace_all(taxon, pattern = "Hydraena assimilis/riparia", replacement = "Hydraena")]
data[, taxon := str_replace_all(taxon, pattern = "Sigara distincta/falleni/iactans/longipalis", replacement = "Sigara")] 


#- save to or load from file 
saveRDS(data, paste0("data/original data/wiser/auxilliary/01_",Sys.Date(),"_data2.rds"))
data <- readRDS("data/original data/wiser/auxilliary/01_2021-07-20_data2.rds")

# taxonomy --------------------------------------------------------------------------
TU <- sort(unique(data$taxon))

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

taxontable[clean == FALSE]
#- manual fixes 
taxontable[original_name == "Clinocerinae",`:=` (species = NA, genus = NA, family = "Empididae"    , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Drusinae"    ,`:=` (species = NA, genus = NA, family = "Limnephilidae", order = "Trichoptera", subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Fridericia"    ,`:=` (species = NA, genus = "Fridericia", family = "Enchytraeidae", order = "Enchytraeida", subclass = "Oligochaeta", class = "Clitellata", phylum = "Annelida", kingdom = "Animalia")]
taxontable[original_name == "Melanogaster",`:=` (species = NA, genus = "Melanogaster", family = "Syrphidae"    , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Turbellaria" ,`:=` (species = NA, genus = NA, family = NA, order = NA, subclass = NA, class = "Turbellaria", phylum = "Platyhelminthes", kingdom = "Animalia")]
taxontable[, clean := TRUE]


unique(taxontable$kingdom)
unique(taxontable$phylum)   |> sort()
unique(taxontable$class)    |> sort()
unique(taxontable$subclass) |> sort()

#- save to or load from file 
saveRDS(taxontable, "data/original data/taxontable.rds")

# COMBINE DATA AND TAXONOMY ---------------------------------------------------------

names(data)[which(names(data) == "taxon")] <- "original_name"
data3 <- taxontable[data, on = "original_name"]

sort(unique(data3$kingdom))
sort(unique(data3$phylum))
sort(unique(data3$order))

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_wiser")]

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
        x.coord = x.coord,
        y.coord = y.coord,
        EPSG = EPSG,
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


#- save sites to file 
saveRDS (data4, paste0("data/original data/wiser/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/wiser/auxilliary/02_2021-08-27_data4.rds")

# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)


#- save sites to file 
saveRDS (sites, paste0("data/original data/wiser/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/wiser/auxilliary/03_2021-07-26_sites.rds")

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
# 
# #- save to or load from file 
# saveRDS(distance_list, paste0("data/original data/wiser/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list      <- readRDS("data/original data/wiser/auxilliary/04_2021-07-20_distance_to_brt.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "brt_distance" = unlist(distance_list),
        "brt20"    = brt_nn$m_btype20c,
        "brt12"    = brt_nn$m_btype12
)

sites2 <- distance_table[sites, on = "site_id"]

# ———————————————————————————————— #
# ——— DISTANCE TO GERMAN TYPES ——— #
# ———————————————————————————————— #

sites2 %<>% st_as_sf() %>% st_transform(crs = st_crs(typology_germany))
nn            <- st_nearest_feature(sites2, typology_germany)
tg_nn         <- typology_germany[nn, ]

distance_list <-
    map(.x = 1:nrow(sites),
        .f = ~ as.numeric(st_distance(x = sites[.x, ],
                                      y = tg_nn[.x, ])))
beepr::beep()
#- save to or load from file 
saveRDS(distance_list, paste0("data/original data/wiser/auxilliary/07_",Sys.Date(),"_distance_to_german_types.rds"))
distance_list  <- readRDS("data/original data/wiser/auxilliary/07_2021-08-02_distance_to_german_types.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites2$site_id,
        "german.type.distance" = unlist(distance_list),
        "german_type"    = tg_nn$TY_CD_RW
)
setDT(sites)
sites2 <- distance_table[sites2, on = "site_id"]

# —————————————— #
# ——— Illies ——— #
# —————————————— #

sites3 <- 
    sites2 |> 
        st_as_sf() |> 
        st_transform(crs = st_crs(illies)) |> 
        st_join(select(illies, NAME)) 

sites3$illies_distance <- sapply(1:nrow(sites3), distance.to.illies, sites3)

# —————————————— #
# ——— BGR    ——— #
# —————————————— #

sites4 <-
    sites3 |>
    st_as_sf() |>
    st_transform(crs = st_crs(bgr)) |>
    st_join(select(bgr, code)) 

sites4$bgr_distance <- sapply(1:nrow(sites4), distance.to.bgr, sites4)
# —————————————— #
# ——— FEC    ——— #
# —————————————— #

site5 <- 
    sites4 |>
    st_as_sf() |>
    st_transform(crs = st_crs(fec)) |>
    st_join(select(fec, least.impacted)) 

sites6 <- 
    site5 |> 
    rename(illies = NAME, bgr = code, fec.least.impacted = least.impacted) %>%
    select(c("brt20", "brt12", "site_id", "illies", "bgr", "fec.least.impacted", "brt_distance", "illies_distance", "bgr_distance")) %>%
    st_drop_geometry() |> 
    setDT()

sites6[, brt12_illies := paste0(brt12, "_", illies)]
sites6[, brt12_bgr := paste0(brt12, "_", bgr)]

data5 <- sites6[data4, on = "site_id"]


#- save to or load from file 
saveRDS(sites6, paste0("data/original data/wiser/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,       paste0("data/original data/wiser/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites <- readRDS("data/original data/wiser/auxilliary/06_2021-08-02_sites2.rds")
data5 <- readRDS("data/original data/wiser/auxilliary/07_2021-08-02_data5.rds")

# SUMMARY STATISTICS ------------------------------------------------------------------------
uniqueN(sites$site_id)
uniqueN(data5$gr_sample_id)
summary(data5$year)

# samples 
# one factor 
data5 |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted)  |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(year > 2004)  |> pull(gr_sample_id) |> uniqueN()
# two factor 
data5 |> filter(year > 2004) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> pull(gr_sample_id) |> uniqueN()
data5 |> filter(fec.least.impacted) |> filter(year > 2004) |> pull(gr_sample_id) |> uniqueN()
# three factors
data5 |> filter(year > 2004) |> filter(brt_distance <= 500) |> filter(fec.least.impacted)|> pull(gr_sample_id) |> uniqueN()

sites |> filter(brt_distance <= 500) |> count()
sites |> filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(brt_distance <= 500) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
sites |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()



# TEMPORAL AGGREGATION --------------------------------------------------------------
data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)  

# ASPT ------------------------------------------------------------------------------

# aspt_data <- data5[, c("gr_sample_id", "family", "abundance")]
# aspt_data[, abundance := sum(abundance), by = c("gr_sample_id", "family")]
# aspt_data <- unique(aspt_data, by = c("gr_sample_id", "family"))
# aspt_data |> 
#         tidyr::pivot_wider(id_cols = "family", names_from = "gr_sample_id", values_from = abundance, values_fill = 0) |> 
#         biotic::calcBMWP() |> 
#         rename(gr_sample_id = Sample) |> 
#         setDT() -> 
#         ASPT
# 
# data6 <- ASPT[data5, on = "gr_sample_id"]
# data6[, ASPT_bool := ASPT > 4.5]
# data6[, BMWP_bool := BMWP > 100]
# data6[, least.impacted := ASPT_bool & BMWP_bool]
# gr_sites <- unique(data6, by = "gr_sample_id")
# gr_sites %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
# mapview(gr_sites, zcol = "least.impacted")

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data5, paste0("data/original data/wiser/", Sys.Date(), "_final.rds"))
data5      <- readRDS("data/original data/wiser/2021-08-02_final.rds")
