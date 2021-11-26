# ——————————————————————————————————— #
# ——— Clean data from Peter Haase ——— # 
# ——————————————————————————————————— #

# ———————————————————————————————————
# date: 
#       16.07.21
# files in: 
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the raw data
#       provided by Peter Haase. 
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
# load data  ------------------------------------------------------------------------

samples1         <- read_excel("data/original data/peter_haase/raw_data/HE_MZB.xlsx")
samples2         <- read_excel("data/original data/peter_haase/raw_data/HE_MZB2.xlsx", sheet = 2)
sites1           <- read_excel("data/original data/peter_haase/raw_data/HE_STAMM.xlsx") 
sites2           <- read_excel("data/original data/peter_haase/raw_data/HE_STAMM2.xls")
perlodes         <- readRDS("data/original data/peter_haase/auxilliary/perlodes_results.rds")
taxontable       <- readRDS("data/original data/2021-07-26_taxontable.rds")
brt              <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
#gloric           <- st_read  ("E://Arbeit/Data/GloRiC_v10/gloric_europe.gpkg")
illies           <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr              <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
typology_germany <- st_read("E://Arbeit/Data/WFD_waterbodytypes_germany/AM_surfaceWaterBody-DE_GDB/AM_surfaceWaterBody-DE.gdb", layer = "AM_riverWaterBody_DE")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")



#- functions
source("~/my documents/R/functions/fill_taxon_table.R")
source("R/distance_to_illies_function.R")
source("R/distance_to_bgr_function.R")

# prepare data  ---------------------------------------------------------------------

#- to data tale ok now to the 
setDT(samples1)
setDT(samples2)
setDT(sites1)

# ————————————————— # 
# ——— SAMPLES 1 ——— #
# ————————————————— #

#- add ecological state to samples 
perlodes %<>% rename(ID_PN = Probe)
samples1 <- perlodes[samples1, on = "ID_PN"]

samples1 <-
        samples1[,list(
                         original_site_name = as.numeric(ID_RS),
                         date = dmy(Datum),
                         year = as.numeric(Year),
                         season = ifelse(
                                 Month %in% c("12", "01", "02"),
                                 "winter",ifelse(Month %in% c("03", "04", "05"),
                                         "spring",ifelse(Month %in% c("06", "07", "08"), "summer", "autumn"))),
                         taxon = Taxon,
                         abundance = as.numeric(IZpqm),
                         ecological_state = ÖZK
                 )]

sites1 = sites1[,list(
        original_site_name = as.numeric(ID_RS),
        x.coord = as.numeric(RW_GK3),
        y.coord = as.numeric(HW_GK3),
        EPSG = 31467
)]        

data1 = sites1[samples1, on = "original_site_name"]

nrow(data1) == nrow(samples1)

data1 = data1[!is.na(taxon)]        

# ————————————————— # 
# ——— SAMPLES 2 ——— #
# ————————————————— #

samples2 %<>% rename(ID_PN = "Prodenahme-ID")
samples2[, ID_PN := as.character(ID_PN)]
samples2 <- perlodes[samples2, on = "ID_PN"]

samples2 <-
        samples2[,
                 list(
                         original_site_name = as.numeric(Site),
                         date = dmy(Datum),
                         taxon = Tax,
                         x.coord = as.numeric(RW),
                         y.coord = as.numeric(HW),
                         EPSG = 31467,
                         abundance = as.numeric(izqm),
                         ecological_state = ÖZK
                         
                 )]
#- add season 
samples2[, c("year", "season") := list(
        year(date),
        ifelse(month(date) %in% c("12", "01","02"), "winter",
               ifelse(month(date) %in% c("03", "04", "05"), "spring",
                      ifelse(month(date) %in% c("06","07","08"), "summer", "autumn"))))]

#- Combine samples 1 and 2. use.names ensures that columns are matched by names not by position.  
data2 <- rbindlist(list(samples2, data1), use.names = TRUE)

#- taxonomy 
data2[taxon %in% c("Baetis fuscatus/scambus", "Baetis alpinus/lutheri/melanonyx/vardarensis", "Baetis lutheri/vardarensis", "Caenis beskidensis/pseudorivulorum", 
                   "Caenis luctuosa/macrura",      "Chaetopteryx fusca/villosa", "Coenagrion puella/pulchellum-Gr.", "Dikerogammarus haemobaphes/villosus",
                   "Drusus annulatus/biguttatus", "Dugesia lugubris/polychroa", "Ecdyonurus torrentis/venosus", "Elmis aenea/maugetii/rietscheli/rioloides"  , 
                   "Elmis aenea/maugetii",   "Gammarus fossarum/pulex",  "Glossosoma boltoni/conformis",  "Halesus digitatus/tesselatus", 
                   "Halesus digitatus/radiatus/tesselatus",  "Hydropsyche pellucidula/incognita", "Liponeura brevirostris/decipiens/vimmeri", 
                   "Melampophylax mucoreus/nepos", "Micropterna lateralis/sequax", "Mystacides longicornis/nigra", "Naididae/Tubificidae",
                   "Nebrioporus depressus/elegans",  "Polycelis nigra/tenuis",  "Potamophylax cingulatus/latipennis/luctuosus", "Radix balthica/labiata"   , 
                   "Radix ovata/peregra",     "Rhyacophila polonica/praemorsa", "Rhyacophila dorsalis/nubila", "Rhyacophila dorsalis/nubila/pascoei/simulatrix/vulgaris", 
                   "Sericostoma flavicorne/personatum", "Silo nigricornis/piceus", "Simulium dunfellense/urbanum" , "Simulium argyreatum/variegatum"), 
      taxon := word(taxon, 1)]


data2[taxon == "Chaetopterygini/Stenophylacini", taxon := "Limnephilidae"]                         
data2[taxon == "Habroleptoides/Paraleptophlebia", taxon := "Leptophlebiidae"]                 
data2[taxon == "Crangonyx/Niphargus", taxon := "Crangonyctidae"]                         
data2[taxon == "Corduliidae/Libellulidae", taxon := "Odonata"] 
data2[taxon == "Ceratopogoninae/Palpomyiinae", taxon := "Ceratopogonidae"]  
data2[taxon == "Nemoura/Nemurella", taxon := "Nemouridae"]                    
data2[taxon == "Jungiella/Psychoda/Tinearia", taxon := "Psychodidae"] 


data2[, taxon := str_remove(taxon, "\\ sp\\.")]
data2[, taxon := str_remove(taxon, "\\ Gen\\.")]
data2[, taxon := str_remove(taxon, "\\ Lv\\.")]
data2[, taxon := str_remove(taxon, "\\ Ad\\.")]
data2[, taxon := str_remove(taxon, "\\ Agg\\.")]
data2[, taxon := str_remove(taxon, "\\ Gr\\.")]

#- clean up 
rm(data1, samples1, samples2, sites1)
gc()

#- save to or load from file 
saveRDS(data2, paste0("data/original data/peter_haase/auxilliary/01_",Sys.Date(),"_data.rds"))
data2 <- readRDS("data/original data/peter_haase/auxilliary/01_2021-07-30_data.rds")

# taxonomy --------------------------------------------------------------------------
#- first look at taxa 
(TU <- sort(unique(data2$taxon)))
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
fill.taxon.table("Rhyacophila (Hyperrhyacophila)", NA, "Rhyacophila", "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila (Hyporhyacophila)" , NA, "Rhyacophila", "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
#- set clean to true for manually edited entries 
taxontable[, clean := TRUE]
taxontable[phylum == "Animalia", phylum := "Arthropoda"]
fill.taxon.table("Hirundinea", NA,NA, NA,NA, "Hirudinea", "Clitellata", "Annelida")


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

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := .GRP, by = "date"]

#- add leading zeros 
data3[, site_id := case_when(
        nchar(site_id) == 1 ~ paste0("0000", site_id),
        nchar(site_id) == 2 ~ paste0("000", site_id),
        nchar(site_id) == 3 ~ paste0("00", site_id),
        nchar(site_id) == 4 ~ paste0("0", site_id),
        nchar(site_id) == 5 ~ paste0(site_id))]

data3[, date_id := case_when(
        nchar(date_id) == 1 ~ paste0("0000", date_id),
        nchar(date_id) == 2 ~ paste0("000",  date_id),
        nchar(date_id) == 3 ~ paste0("00",   date_id),
        nchar(date_id) == 4 ~ paste0("0",    date_id),
        nchar(date_id) == 5 ~ paste0(        date_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_peter_haase")]

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
        data.set = "peter_haase",
        ecological_state
        
)]

data4 <- data4[site_id != "00546"]
data4 <- data4[!is.na(x.coord)]
data4 <- data4[ecological_state %in% c(1,2)]
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
saveRDS(data4, paste0("data/original data/peter_haase/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/peter_haase/auxilliary/02_2021-07-30_data4.rds")
# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites, zcol = "ecological_state")


#- save sites to file 
saveRDS (sites, paste0("data/original data/peter_haase/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/peter_haase/auxilliary/03_2021-07-26_sites.rds")

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
# #- save to or load from file 
# saveRDS(distance_list, paste0("data/original data/peter_haase/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/peter_haase/auxilliary/04_2021-07-21_distance_to_brt.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "brt_distance" = unlist(distance_list),
        "brt20"    = brt_nn$m_btype20c,
        "brt12"    = brt_nn$m_btype12
)

sites <- distance_table[sites, on = "site_id"]
#sites <- sites[brt_distance <= 500]
mapview(st_as_sf(sites))

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
# saveRDS(distance_list, paste0("data/original data/peter_haase/auxilliary/05_",Sys.Date(),"_distance_to_gloric.rds"))
# #distance_list  <- readRDS("data/original data/peter_haase/auxilliary/05_2021-07-21_distance_to_gloric.rds")
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

# ————————————————————————————————— #
# ——— DISTANCE TO GERMAN TYPES  ——— #
# ————————————————————————————————— #

sites %<>% st_as_sf() %>%  st_transform(crs = st_crs(typology_germany))
nn            <- st_nearest_feature(sites, typology_germany)
tg_nn         <- typology_germany[nn, ]

distance_list <-
  map(.x = 1:nrow(sites),
      .f = ~ as.numeric(st_distance(x = sites[.x, ],
                                    y = tg_nn[.x, ])))
beepr::beep()
#- save to or load from file
saveRDS(distance_list, paste0("data/original data/peter_haase/auxilliary/07_",Sys.Date(),"_distance_to_german_types.rds"))
distance_list  <- readRDS("data/original data/peter_haase/auxilliary/07_2021-08-02_distance_to_german_types.rds")

#- create distance table 
distance_table <- data.table(
  "site_id" = sites$site_id,
  "german.type.distance" = unlist(distance_list),
  "german_type"    = tg_nn$TY_CD_RW
)
sites <- distance_table[sites, on = "site_id"]
#sites <- sites[german.type.distance <= 500]
#mapview(st_as_sf(sites))


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

sites2 <- 
  sites |> 
  rename(illies = NAME, bgr = code, fec.least.impacted = least.impacted) %>%
  select(c("brt20", "brt12", "site_id", "illies", "bgr", "fec.least.impacted", "brt_distance", "german_type")) %>%
  st_drop_geometry() |> 
  setDT()

sites2[, brt12_illies := paste0(brt12, "_", illies)]
sites2[, brt12_bgr := paste0(brt12, "_", bgr)]

data5 <- sites2[data4, on = "site_id"]

#- save to or load from file 
saveRDS(sites2, paste0("data/original data/peter_haase/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,       paste0("data/original data/peter_haase/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites <- readRDS("data/original data/peter_haase/auxilliary/06_2021-07-30_sites2.rds")
data5 <- readRDS("data/original data/peter_haase/auxilliary/07_2021-07-30_data5.rds")

# TEMPORAL AGGREGATION --------------------------------------------------------------
data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness) 

for (i in 1:nrow(sites)) {
  
  #- setup for a new list
  if (i == 1) new.lst <- list()
  
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
      lp.k.dat$sample_events <- lp.k.n
      
      if (lp.k.n == 1) {
        #- drop date
        lp.k.dat[, date := NA]
        lp.k.dat[, gr_sample_id := paste0(
          site_id,
          "_",
          c("spring", "summer", "autumn", "winter")[k],
          "_combined_peter_haase"
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
                                     "_combined_peter_haase")]
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
  if(lp.progress %% 5 == 0) print(lp.progress)
  rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
  rm(i)
  gc()
}

data6 <- rbindlist(new.lst,use.names=TRUE,fill =T )
data6[, sample_events := NULL]
data6[, sampling.events := NULL]

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
saveRDS(data6, paste0("data/original data/peter_haase/",Sys.Date(),"_final.rds"))
data6    <-   readRDS("data/original data/peter_haase/2021-08-02_final.rds")
