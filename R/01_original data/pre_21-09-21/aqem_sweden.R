# —————————————————————————————— #
# ——— Clean AQEM SWEDEN data ——— # 
# —————————————————————————————— #

# ———————————————————————————————————
# date: 
#       14.07.21
# Project:
#       Evaluating European Broad River Types with Macroinvertebrates
# Purpose:
#       In this script, I create a harmonized spatial data set from the raw data AQEM Sweden
#       provided from  by Leonard Sandin. 
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
        stringr
)

# — — — FUNCTIONS
source("R/distance_to_illies_function.R")
source("R/distance_to_bgr_function.R")
source("R/fill_taxon_table.R")

# load data  ------------------------------------------------------------------------
samples    <- read_excel("data/original data/aqem_sweden/raw_data/CompleteTaxalist_AQEM_Site_info.xls", skip = 2) 
sites      <- read_excel("data/original data/aqem_sweden/raw_data/CompleteTaxalist_AQEM_Site_info.xls", sheet = 2)
taxontable <- readRDS("data/original data/2021-08-26_taxontable.rds")
brt        <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies     <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr        <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# prepare data  ---------------------------------------------------------------------

#- transform to data table 
setDT(samples)
setDT(sites)

#- rename 
names(samples)[c(1,2)] <- c("taxon", "taxon_supp")

#- Some rows are only summaries of the rows below them, aggregated to a Family
#- level. They need to be removed. In these rows the second column is empty. Some
#- data are only collected at family level though. I want to maintain them but
#- their second column is also empty. 
samples <-
        samples[!(
                str_detect(samples$taxon, "[A-Z]{2,}") |
                        taxon %in% c(
                                "[Kl:Turbellaria]",
                                "Turbellaria",
                                "Nematoda",
                                "[Kl:Nematoda]",
                                "[Kl:Oligochaeta]",
                                "Oligochaeta",
                                "[Ord:Lepidoptera]"
                        )
        )]

samples[taxon_supp %in% c("Gen. sp.", "sp.", "Gen. sp. Lv.", "sp. Lv.", "sp. Ad."), 
         taxon_supp := ""]

samples[, taxon_clean := str_trim(paste(taxon,taxon_supp))]

samples <- samples[,.SD, .SDcols = 7:157]
samples <- samples[-1]

site_names <- colnames(samples)[-151]
samples = data.table::melt.data.table(samples, id.vars = c("taxon_clean"))
samples = samples[value != 0]

sites = data.table(
        site = rep(sites$`sampling site`,2),
        x.coord = as.numeric(rep(sites$`X_RAK (Xnew)`,2)),
        y.coord = as.numeric(rep(sites$`Y_RAK (Xnew)`,2)),
        id = append(sites$`Sample Number Spring`,sites$`Sample Number Autumn`),
        date = append(sites$`1st sampling date`, sites$`2nd sampling date`)
        
)
sites[,date := dmy(date)]
sites[,c("year","season") := 
               list(
                       year(date),
                       ifelse(month(date) %in% c(3,4,5), "spring", 
                              ifelse(month(date) %in% c(6,7,8), "summer", 
                                     ifelse(month(date) %in% c(9,10,11), "autumn", "winter"
                                     )))
               )]
samples$id = as.character(samples$variable)


data <- sites[samples, on = "id"]
data2 <- data[, list(
        original_site_name = site,
        date = ymd(date),
        year,
        season,
        taxon = taxon_clean,
        abundance = value,
        x.coord ,
        y.coord,
        EPSG = 2400,
        data.set = "AQEM_sweden"
)]


data2[, taxon := str_remove_all(taxon, "-Gr\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Lv\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Ad\\.$")]

data2[taxon == "Amphinemura standfussi/sulcicollis", taxon := "Amphinemura"]
data2[taxon == "Baetis macani/bundaye", taxon := "Baetis"]
data2[taxon == "Gyraulus acronicus/albus/laevis", taxon := "Gyraulus"]
data2[taxon == "Leuctra fusca/digitata/hippopus", taxon := "Leuctra"]
data2[taxon == "Oulimnius troglodytes/tuberculatus", taxon := "Oulimnius"]
data2[taxon == "Radix peregra/ovata", taxon := "Radix"]
data2[taxon == "Rhyacophila obliterata/nubila", taxon := "Rhyacophila"]
data2[taxon == "Chaetopteryx/Anitella", taxon := "Limnephilidae"]
data2[taxon == "Mystacides longicornis/nigra", taxon := "Mystacides"]

TU <- sort(unique(data2$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

#- save to or load from file 
saveRDS(data2, paste0("data/original data/aqem_sweden/auxilliary/01_",Sys.Date(),"_data.rds"))
data2 <- readRDS("data/original data/aqem_sweden/auxilliary/01_2021-07-20_data.rds")

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


unique(taxontable$kingdom)
unique(taxontable$phylum)   |> sort()
unique(taxontable$class)    |> sort()
unique(taxontable$subclass) |> sort()

#- save to or load from file 
saveRDS(taxontable, "data/original data/taxontable.rds")
# COMBINE DATA AND TAXONOMY ------------------------------------------------------------

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_aqem_Sweden")]

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
        x.coord = y.coord,
        y.coord = x.coord,
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
data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/aqem_sweden/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/aqem_sweden/auxilliary/02_2021-08-26_data4.rds")


# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/aqem_sweden/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <-       readRDS("data/original data/aqem_sweden/auxilliary/03_2021-08-31_sites.rds")

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
# #- save to or load from file 
saveRDS(distance_list, paste0("data/original data/aqem_sweden/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/aqem_sweden/auxilliary/04_2021-08-31_distance_to_brt.rds")

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
sites3$illies_distance <- sapply(1:nrow(sites2), distance.to.illies, sites3)
beepr::beep()

# —————————————— #
# ——— BGR    ——— #
# —————————————— #

sites4 <- 
        sites3 |>
        st_as_sf() |>
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
        st_as_sf() |>
        st_transform(crs = st_crs(fec)) |>
        st_join(select(fec, least.impacted))


sites6 <-
        sites5 |> 
        rename(illies = NAME, bgr = code, fec.least.impacted = least.impacted) %>%
        select(c("brt20", "brt12", "site_id", "illies", "bgr", "fec.least.impacted", "brt_distance", "bgr_distance", "illies_distance")) %>%
        st_drop_geometry() %>%
        setDT()

data5 <- sites6[data4, on = "site_id"]


#- save to or load from file 
saveRDS(sites, paste0("data/original data/aqem_sweden/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,       paste0("data/original data/aqem_sweden/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites <- readRDS("data/original data/aqem_sweden/auxilliary/06_2021-08-31_sites2.rds")
data5 <- readRDS("data/original data/aqem_sweden/auxilliary/07_2021-08-31_data5.rds")

# Statistics ------------------------------------------------------------------------
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
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(brt_distance <= 500) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
sites |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()

# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)
#- always sampled in two different seasons 
data5[, uniqueN(season), by = "site_id"]

#- Aggregation not necessary 

# ASPT ------------------------------------------------------------------------------
# aspt_data <- data5[, c("gr_sample_id", "family", "abundance")]
# aspt_data[, abundance := as.numeric(abundance)]
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
# data6[, bio.least.impacted := ASPT_bool & BMWP_bool]
# gr_sites <- unique(data6, by = "gr_sample_id")
# gr_sites %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
# mapview(gr_sites, zcol = "least.impacted")


# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data5, paste0("data/original data/aqem_sweden/",Sys.Date(),"_final.rds"))
data6 <- readRDS("data/original data/aqem_sweden/2021-08-02_final.rds")

