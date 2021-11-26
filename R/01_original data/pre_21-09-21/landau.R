# ————————————————————————— #
# ——— Clean Landau data ——— # 
# ————————————————————————— #


# ———————————————————————————————————
# date: 
#       14.07.21
# files in: 
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the raw data
#       from Germany from the server in Landau. 
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
# load data  ------------------------------------------------------------------------

samples          <- fread("data/original data/landau/raw_data/mzb_samples.csv")
sites            <- fread("data/original data/landau/raw_data/mzb_sites.csv")
perlodes         <- readRDS("data/original data/landau/auxilliary/perlodes_results.rds") 
taxontable       <- readRDS("data/original data/2021-08-18_taxontable.rds")
brt              <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies           <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr              <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
typology_germany <- st_read("E://Arbeit/Data/WFD_waterbodytypes_germany/AM_surfaceWaterBody-DE_GDB/AM_surfaceWaterBody-DE.gdb", layer = "AM_riverWaterBody_DE")
fec              <- st_read("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

#- function 
source("~/my documents/R/functions/fill_taxon_table.R")



# PREPARE DATA  ---------------------------------------------------------------------

#- remove rows with zero abundance and  before 2004 (harmonization of European sampling schemes)
samples <- 
    samples |> 
    mutate(year = year(date)) %>%
    filter(year > 2004 & ind_qm > 0)

#- add ecological state 
names(perlodes)[1] <- "sample_id"
samples2 <- perlodes[samples, on = "sample_id"]
samples2[, bio.least.impacted := ÖZK]
samples2 <- samples2[ÖZK < 3, bio.least.impacted := TRUE]
samples2 <- samples2[ÖZK > 2, bio.least.impacted := FALSE]

#- drop columns  
samples2 <- samples2[, c("date", "taxon", "site_id", "ind_qm", "bio.least.impacted") ]
rm(samples)

# In samples the sites with TH in their site_id seem to have mistakes. All TH ..
# sites in the samples data go like TH_TH_xxxx while those in the sites data go
# TH_xxxx. Hence I remove the first TH 
samples2$site_id = str_replace_all(samples2$site_id, "TH_TH_", "TH_")
sites2 = sites[, c("site_id", "stream", "site_name", "geom")]

#fix site coordinates 
#Geometry type PostgreSQL columns They can be converted with sf see
#https://github.com/r-dbi/RPostgres/issues/114 This converts the geom column
#which holds Postgres Geom codes to xy coordinates and also returns the
#projection.
coord <- st_as_sfc(
        structure(
                sites2$geom, 
                class = "WKB"
        ),
        EWKB = TRUE
)
coord2 = st_coordinates(coord) %>% data.frame()
sites3  <- bind_cols(
        sites2,
        coord2,
        EPSG = rep(31463, nrow(sites2))
) 
sites3 = sites3[,-c("geom")]
# join data sets 
data = left_join(samples2, sites3) %>% setDT
rm(coord,coord2,samples2,sites,sites2,sites3)

# fix date column
data[,c("date", "year", "month", "day") := list(ymd(date), year(date), month(date), day(date))]
data[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]
data2 = data

data2 <- data2[, list(
        original_site_name = site_id,
        bio.least.impacted,
        date,
        year,
        season,
        taxon = taxon,
        abundance = ind_qm,
        x.coord = X,
        y.coord = Y,
        EPSG,
        data.set = "landau"
)]

#- taxonomy 
data2[, taxon := str_remove_all(taxon, "\\ Lv\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Ad\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ sp\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ gen\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Gen\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ spec\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ \\(.*\\)")]
data2[str_detect(taxon, "/"), taxon := word(taxon, 1)]


data2[taxon == "Baetis niger/digitatus", taxon := "Baetis"]
data2[taxon == "Hemerodromia/Wiedemannia", taxon := "Empididae"]
data2[taxon == "Leuctra digi/fusc/hipp", taxon := "Leuctra"]
data2[taxon == "Pericomini/Telmatoscopini", taxon := "Psychodidae"]
data2[taxon == "Rhyacophila obliterata/fasciata", taxon := "Rhyacophila"]


data2 <- data2[taxon != "ungÃ¼ltig: 10057"]
data2 <- data2[taxon != "ungÃ¼ltig: 792"]

#- save to or load from file 
saveRDS(data2, paste0("data/original data/landau/auxilliary/01_",Sys.Date(),"_data.rds"))
data2 <- readRDS("data/original data/landau/auxilliary/01_2021-07-23_data.rds")

# taxonomy --------------------------------------------------------------------------
(TU <- sort(unique(data2$taxon)))

#- which ones need to be checked 
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

#for (i in seq_along(TU)){
for (i in 1125:length(TU)) {        
        #- skip problem entry 
        if (data2$data.set == "landau" & i == 1125) next()
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

fill.taxon.table("Acari"                                                       , NA                , NA           , NA             , NA           , NA , "Arachnida", "Arthropoda")                                                       
fill.taxon.table("Acentropinae"                                                , NA                , NA           , "Crambidae"    , "Lepidoptera", NA , "Insecta"  , "Arthropoda")                                               
fill.taxon.table("Allogamus - Gruppe"                                          , NA                , "Allogamus"  , "Limnephilidae", "Trichoptera", NA , "Insecta"  , "Arthropoda")                                          
fill.taxon.table("Anabolia furcata x nervosa"                                  , "Anabolia furcata", "Anabolia"   , "Limnephilidae", "Trichoptera", NA , "Insecta"  , "Arthropoda")                                  
fill.taxon.table("Athripsodes albifrons / bilineatus / commutatus"             , NA                , "Athripsodes", "Leptoceridae", "Trichoptera",  NA , "Insecta"  , "Arthropoda")            
fill.taxon.table("Athripsodes albifrons/commutatus/cinereus"                   , NA                , "Athripsodes", "Leptoceridae", "Trichoptera",  NA , "Insecta"  , "Arthropoda")            
fill.taxon.table("Baetis alpinus - Gruppe / lutheri - Gruppe"                  , NA                , "Baetis"     , "Baetidae"    ,"Ephemeroptera", NA , "Insecta"  , "Arthropoda")            
fill.taxon.table("Baetis alpinus/lutheri/melanonyx/vardarensis"                , NA                , "Baetis"     , "Baetidae"    ,"Ephemeroptera", NA , "Insecta"  , "Arthropoda")            
fill.taxon.table("Baetis buceratus/liebenauae/pentaphlebodes/tracheatus/vernus", NA                , "Baetis"     , "Baetidae"    ,"Ephemeroptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Baetis buceratus/vernus"                                     , NA                , "Baetis"     , "Baetidae"    ,"Ephemeroptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Baetis fuscatus/scambus"                                     , NA                , "Baetis"     , "Baetidae"    ,"Ephemeroptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Baetis lutheri/vardarensis"                                  , NA                , "Baetis"     , "Baetidae"    ,"Ephemeroptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Baetis vernus - Gruppe / buceratus - Gruppe"                 , NA                , "Baetis"     , "Baetidae"    ,"Ephemeroptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Bezzia-Gr."                                                  , NA                , "Bezzia"     , "Ceratopogonidae",   "Diptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Bithynia c.f. leachii leachii"                               , "Bithynia leachii", "Bithynia"   , "Bithyniidae", "Littorinimorpha", NA , "Gastropoda", "Mollusca")
fill.taxon.table("Caenis beskidensis/pseudorivulorum"                          , NA                , "Caenis"     , "Caenidae"    , "Ephemeroptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Caenis horaria/robusta"                                      , NA                , "Caenis"     , "Caenidae"    , "Ephemeroptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Caenis luctuosa/macrura"                                     , NA                , "Caenis"     , "Caenidae"    , "Ephemeroptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Cecidomyiidae genus"                                         , NA               ,"Cecidomyiidae", "Leptoceridae", "Trichoptera"  , NA,  "Insecta"  , "Arthropoda")
fill.taxon.table("Ceraclea albimacula/alboguttata"                             , NA               ,"Ceraclea"     , "Leptoceridae", "Trichoptera"  , NA,  "Insecta"  , "Arthropoda")
fill.taxon.table("Ceratopogoninae / Palpomyiinae"                              , NA               ,NA             , "Ceratopogonidae", "Diptera"   , NA,  "Insecta"  , "Arthropoda")
fill.taxon.table("Ceratopogoninae genus"                                       , NA               ,NA             , "Ceratopogonidae", "Diptera"   , NA,  "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Aquarius najas"                                          , "Aquarius najas" ,"Aquarius"     , "Gerridae"    , "Hemiptera"    , NA,  "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Argyroneta aquatica"                                     , "Argyroneta aquatica", "Argyroneta", "Dictynidae", "Araneae"      , NA,  "Arachnida"  , "Arthropoda")
fill.taxon.table("cf. Cataclysta lemnata"                                      , "Cataclysta lemnata" , "Cataclysta", "Crambidae"           , "Lepidoptera"    , NA, "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Chaetopteryx villosa"                                    , "Chaetopteryx villosa" ,"Chaetopteryx", "Limnephilidae"    , "Trichoptera"    , NA, "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Cheilotrichia"                                           ,  NA                       ,"Cheilotrichia", "Limoniidae"   , "Diptera"        , NA, "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Elmis aenea LATREILLE"                                   , "Elmis aenea"             ,"Elmis"        , "Elmidae"      , "Coleoptera"   , NA, "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Elophila nymphaeata"                                     , "Elophila nymphaeata"     ,"Elophila"     ,  "Crambidae"   , "Lepidoptera", NA , "Insecta"  , "Arthropoda") 
fill.taxon.table("cf. Erpobdella testacea"                                     , "Erpobdella testacea"     ,"Erpobdella"   , "Erpobdellidae", "Arhynchobdellida"  , NA, "Clitellata", "Annelida")
fill.taxon.table("cf. Hydroglyphus geminus"                                    , "Hydroglyphus geminus"    ,"Hydroglyphus" , "Dytiscidae"   , "Coleoptera"   , NA, "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Libellula quadrimaculata"                                , "Libellula quadrimaculata","Libellula"    , "Libellulidae" , "Odonata"  , NA, "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Parachionia picicornis"                                  , "Parachionia picicornis"  ,"Parachionia"  , "Limnephilidae", "Trichoptera", NA , "Insecta"  , "Arthropoda")       

fill.taxon.table("cf. Physella sp. HALDEMAN"                                   , NA                        ,"Physella"      , "Leptoceridae"  , "Trichoptera" , NA,  "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Potamophylax nigricornis"                                , "Potamophylax nigricornis", "Potamophylax" , "Limnephilidae" , "Trichoptera" , NA , "Insecta"     , "Arthropoda")
fill.taxon.table("cf. Proasellus meridianus"                                   , "Proasellus meridianus"   , "Proasellus"   , "Asellidae"     , "Isopoda"     , NA , "Malacostraca",  "Arthropoda")
fill.taxon.table("cf. Procloeon bifidum"                                       , "Procloeon bifidum"       , "Procloeon"    , "Baetidae"      ,"Ephemeroptera", NA , "Insecta"  , "Arthropoda")
fill.taxon.table("cf. Pseudanodonta complanata RAFINESQUE"                     , "Pseudanodonta complanata", "Pseudanodonta", "Unionidae"     , "Unionida"    , NA , "Bivalvia" , "Mollusca")                       
fill.taxon.table("cf. Rhyacophila"                                             , NA                        , "Rhyacophila"  , "Rhyacophilidae", "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Chaetopterygini"                                             , NA                        , NA             , "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Chaetopterygini / Stenophylacini"                            , NA                        , NA             , "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Chaetopterygini Gen. sp."                                    , NA                        , NA             , "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Chaetopterygini Gen. sp./Stenophylacini"                     , NA                        , NA             , "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Chaetopterygini Gen. sp./Stenophylacini Gen. sp."            , NA                        , NA             , "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Chaetopterygini/Stenophylacini"                              , NA                        , NA             , "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Chaetopteryx fusca/villosa"                                  , NA                        , NA             , "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Chaetopteryx villosa / fusca"                                , NA                        , NA             , "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")

fill.taxon.table("Chironomini Gen."                                            ,NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Chironomini genus"                                           ,NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Chironomini non det."                                        ,NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Chironomus / Einfeldia"                                      ,NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Cirripedia"                                                  ,NA, NA, NA            , NA       , NA, "Maxillopoda", "Arthropoda") 
fill.taxon.table("Clitellariinae"                                              ,NA, NA, "Stratiomyidae", "Doptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Coenagrion puella / pulchellum"                              ,NA, "Coenagrion", "Coenagrionidae", "Odonata", NA, "Insecta", "Arthropoda")
fill.taxon.table("Coenagrion puella/pulchellum"                                ,NA, "Coenagrion", "Coenagrionidae", "Odonata", NA, "Insecta", "Arthropoda")
fill.taxon.table("Coenagrion puella/pulchellum-Gr."                            ,NA, "Coenagrion", "Coenagrionidae", "Odonata", NA, "Insecta", "Arthropoda")
fill.taxon.table("Conchapelopia - Gruppe"                                      ,NA, "Conchapelopia", "Choronomidae","Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Copepoda-Copepodid"                                          ,NA, NA, NA            , NA       , "Copepoda", "Hexanauplia", "Arthropoda")
fill.taxon.table("Copepoda-Nauplius"                                           ,NA, NA, NA            , NA       , "Copepoda", "Hexanauplia", "Arthropoda")
fill.taxon.table("Corbicula \\\"\"\\\"\"fluminalis\\\"\"\\\"\""                ,"Corbicula fluminalis", "Corbicula", "Cyrenidae", "Venerida", NA, "Bivalvia", "Mollusca")
fill.taxon.table("Corduliidae / Libellulidae"                                  , NA, NA, NA, "Odonata", NA, "Insecta", "Arthropoda")
fill.taxon.table("Corduliidae/Libellulidae"                                    , NA, NA, NA, "Odonata", NA, "Insecta", "Arthropoda")
fill.taxon.table("Cricotopus / Orthocladius"                                   ,NA, NA, "Chironomidae", "Diptera"   , NA, "Insecta", "Arthropoda") 
fill.taxon.table("Cucujoidea"                                                  ,NA, NA , NA,            "Coleoptera", NA, "Insecta"  , "Arthropoda")
fill.taxon.table("Cyrnus c.f. insolutus"                                       ,"Cyrnus insolutus", "Cyrnus", "Polycentropodidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Dasyheleinae Gen. sp. KIEFFER"                               , NA, NA , "Ceratopogonidae","Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Dikerogammarus haemobaphes / villosus"                       , NA, "Dikerogammarus", "Gammaridae", "Amphipoda", NA, "Malacostraca", "Arthropoda")
fill.taxon.table("Dikerogammarus haemobaphes/villosus"                         , NA, "Dikerogammarus", "Gammaridae", "Amphipoda", NA, "Malacostraca", "Arthropoda")
fill.taxon.table("Dixa maculata / nubilipennis"                                , NA, "Dixa", "Dixidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Donaciinae"                                                  , NA, NA, "Chrysomelidae", "Coleoptera", NA, "Insecta"  , "Arthropoda")
fill.taxon.table("Donaciinae genus"                                            , NA, NA, "Chrysomelidae", "Coleoptera", NA, "Insecta"  , "Arthropoda")
fill.taxon.table("Drusus annulatus / biguttatus"                               , NA, "Drusus", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Drusus annulatus/biguttatus"                                 , NA, "Drusus", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Dugesia lugubris / polychroa"                                , NA, "Dugesia", "Dugesidae", "Tricladida", NA, NA, "Platyhelminthes")
fill.taxon.table("Dugesia lugubris/polychroa"                                  , NA, "Dugesia", "Dugesidae", "Tricladida", NA, NA, "Platyhelminthes")
fill.taxon.table("Dugesia lugubris/polychroa-Gr."                              , NA, "Dugesia", "Dugesidae", "Tricladida", NA, NA, "Platyhelminthes")
fill.taxon.table("Dugesia spec. GIRARD"                                        , NA, "Dugesia", "Dugesidae", "Tricladida", NA, NA, "Platyhelminthes")
fill.taxon.table("Elmis aenea / maugetii"                                      , NA, "Elmis", "Elmidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Elmis aenea / maugetii / rietscheli / rioloides"             , NA, "Elmis", "Elmidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Elmis aenea/maugetii"                                        , NA, "Elmis", "Elmidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Elmis aenea/maugetii/rietscheli/rioloides"                   , NA, "Elmis", "Elmidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Ephemera sp. LINNAEUS"                                       , NA, "Ephemera", "Ephemeridae", "Ephemeroptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Ephydatia/ Eunapius"                                         , NA, NA, "Spongillidae", "Spongilida", NA, "Demospongiae", "Porifera")

fill.taxon.table("Eristalis-Gruppe"                                            , NA, "Eristalis", "Syrphidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Erpobdella c.f. testacea"                                    , "Erpobdella testacea", "Erpobdella", "Erpobdellidae", "Arhynchobdellida", "Hirudinea", "Clitellata", "Annelida")
fill.taxon.table("Erpobdella nigricollis/testacea"                             , NA                  , "Erpobdella", "Erpobdellidae", "Arhynchobdellida", "Hirudinea", "Clitellata", "Annelida")
fill.taxon.table("Eukiefferiella devonica/ilkleyensis"                         , NA, "Eukiefferiella", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Eukiefferiella minor/fittkaui"                               , NA, "Eukiefferiella", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("F: Hydroptilidae Gen. Sp."                                   , NA, NA, "Hydroptilidae", "Trichoptera"  , NA,  "Insecta"  , "Arthropoda")
fill.taxon.table("Gammarus fossarum/pulex-Gr."                                 , NA, "Gammarus", "Gammaridae", "Amphipoda", NA, "Malacostraca", "Arthropoda")
fill.taxon.table("Gammarus fossarum/pulex"                                     , NA, "Gammarus", "Gammaridae", "Amphipoda", NA, "Malacostraca", "Arthropoda")
fill.taxon.table("Gammarus lacustris/varsoviensis"                             , NA, "Gammarus", "Gammaridae", "Amphipoda", NA, "Malacostraca", "Arthropoda")
fill.taxon.table("Glossiphonia c.f complanata"                                 , "Glossiphonia complanata", "Glossiphonia", "Glossiphoniidae", "Rhynchobdellida", NA, "Clitellata", "Annelida")
fill.taxon.table("Glossiphonia c.f concolor"                                   , "Glossiphonia concolor"  , "Glossiphonia", "Glossiphoniidae", "Rhynchobdellida", NA, "Clitellata", "Annelida")
fill.taxon.table("Glossiphonia c.f. concolor"                                  , "Glossiphonia concolor"  , "Glossiphonia", "Glossiphoniidae", "Rhynchobdellida", NA, "Clitellata", "Annelida")
fill.taxon.table("Glossiphonia nebulosa / verrucata"                           , NA                       , "Glossiphonia", "Glossiphoniidae", "Rhynchobdellida", NA, "Clitellata", "Annelida")
fill.taxon.table("Glossiphonia nebulosa/verrucata"                             , NA                       , "Glossiphonia", "Glossiphoniidae", "Rhynchobdellida", NA, "Clitellata", "Annelida")
fill.taxon.table("Glossosoma boltoni / conformis"                              , NA, "Glossosoma", "Glossosomatidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Glyptotendipes glaucus/pallens"                              , NA, "Glyptotendipes", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Gyrinus marinus GYLLENHAL"                                   , "Gyrinus marinus", "Gyrinus", "Gyrinidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Gyrinus natator/substriatus"                                 , NA               , "Gyrinus", "Gyrinidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Halesus digitatus / tesselatus"                              , NA, "Halesus", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Halesus digitatus/radiatus/tesselatus"                       , NA, "Halesus", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Halesus digitatus/tesselatus"                                , NA, "Halesus", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Halesus digitatus/tessulatus"                                , NA, "Halesus", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Halesus radiatus/digitatus"                                  , NA, "Halesus", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Helobdella testacea"                                         , "Helobdella testacea", "Helobdella", "Glossiphoniidae", "Rhynchobdellida", NA, "Clitellata", "Annelida")
fill.taxon.table("Helophorus aequalis / aquaticus"                             , NA, "Helophorus", "Helophoridae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Helophorus aequalis/aquaticus"                               , NA, "Helophorus", "Helophoridae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Helophorus flavipes / obscurus"                              , NA, "Helophorus", "Helophoridae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Helophorus flavipes/obscurus"                                , NA, "Helophorus", "Helophoridae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Helophorus minutus / paraminutus"                            , NA, "Helophorus", "Helophoridae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Hemerodromiinae Gen. sp. MEIGEN"                             , NA, NA, "Empididae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Hirundinea"                                                  , NA, NA, NA, NA, "Hirudinea", "Clitellata", "Annelida")
fill.taxon.table("Hydropsyche botosaneanui/incognita/pellucidula"              , NA, "Hydropsyche", "Hydropsychidae", "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Hydropsyche incognita / pellucidula"                         , NA, "Hydropsyche", "Hydropsychidae", "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Hydropsyche incognita/pellucidula"                           , NA, "Hydropsyche", "Hydropsychidae", "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Hydropsyche pellucidula/incognita"                           , NA, "Hydropsyche", "Hydropsychidae", "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Jungiella / Psychoda / Tinearia"                             , NA, NA, "Psychodidae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Jungiella/Psychoda/Tinearia"                                 , NA, NA, "Psychodidae", "Diptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Leuctra digitata/fusca/hippopus"                             , NA, "Leuctra", "Leuctridae", "Plecoptera", NA, "Insecta", "Arthropoda") 
fill.taxon.table("Limnephilus affinis/incisus"                                 , NA, "Limnephilus", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Limnephilus c.f. auricula"                                   , "Limnephilus auricula", "Limnephilus", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Limnius muelleri / opacus"                                   , NA, "Limnius", "Elmidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Limnius muelleri/opacus"                                     , NA, "Limnius", "Elmidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Limoniidae - Pediciidae"                                     , NA, NA, "Tipulidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Liponeura brevirostris / decipiens / vimmeri"                , NA, "Liponeura", "Blephariceridae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Liponeura brevirostris/decipiens/vimmeri"                    , NA, "Liponeura", "Blephariceridae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Lumbriculidae/Dorydrilidae"                                  , NA, NA, NA, "Lumbriculida", "Oligochaeta", "Clitellata", "Annelida")
fill.taxon.table("Lype c.f. reducta"                                           , "Lype reducta", "Lype", "Psychomyiidae", "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Micropterna / Stenophylax"                                   , NA, NA, "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Micropterna lateralis / sequax"                              , NA, "Micropterna", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Micropterna lateralis/sequax"                                , NA, "Micropterna", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Micropterna sequax / lateralis"                              , NA, "Micropterna", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Micropterna/Stenophylax"                                     , NA, NA, "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Microtendipes chloris / pedellus - Gruppe"                   , NA, "Microtendipes", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Microtendipes chloris/pedellus-Gruppe"                       , NA, "Microtendipes", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Mysidacea"                                                   , NA,NA,NA,NA,NA,"Malacostraca", "Arthropoda")
fill.taxon.table("Mysidacea Fam."                                              , NA,NA,NA,NA,NA,"Malacostraca", "Arthropoda")
fill.taxon.table("Mystacides c.f. azurea"                                      , "Mystacides azurea", "Mystacides", "Leptoceridae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Mystacides longicornis / nigra"                              , NA, "Mystacides", "Leptoceridae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Mystacides longicornis/ nigra"                               , NA, "Mystacides", "Leptoceridae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Mystacides longicornis/nigra"                                , NA, "Mystacides", "Leptoceridae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Naididae / Tubificidae"                                      , NA, NA, NA, "Haplotaxida", NA, "Clitellata", "Annelida")
fill.taxon.table("Naididae/Tubificidae"                                        , NA, NA, NA, "Haplotaxida", NA, "Clitellata", "Annelida")
fill.taxon.table("Nais barbata/pseudobtusa"                                    , NA, "Nais", "Opheliidae", NA, NA, "Polychaeta", "Annelida")
fill.taxon.table("Natarsia nugax/punctata"                                     , NA, "Natarsia", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Nebrioporus depressus / elegans"                             , NA, NA, "Nebrioporus", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Nebrioporus depressus/elegans"                               , NA, NA, "Nebrioporus", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Nematocera Fam."                                             , NA, NA, NA, "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Nematoda-Mermithidae"                                        , NA, NA, NA, NA, NA, NA, "Nematoda")
fill.taxon.table("Nemertini"                                                   , NA, NA, NA, NA, NA, NA, "Nemertea")
fill.taxon.table("Nemoura c.f  cinerea"                                        , "Nemoura cinerea"   , "Nemoura", "Nemouridae", "Plecoptera", NA, "Insecta", "Animalia")
fill.taxon.table("Nemoura c.f.  avicularis"                                    , "Nemoura avicularis", "Nemoura", "Nemouridae", "Plecoptera", NA, "Insecta", "Animalia")
fill.taxon.table("Nemoura/Nemurella"                                           , NA,NA, "Nemouridae", "Plecoptera", NA, "Insecta", "Animalia")
fill.taxon.table("Niphargus / Crangonyx - Gruppe"                              , NA, NA, "Niphargidae", "Amphipoda", NA, "Malacostraca", "Arthropoda")
fill.taxon.table("Notiphilinae"                                                , NA, NA, "Ephydridae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Onychogomphus / Ophiogomphus"                                , NA,NA, "Gomphidae", "Odonata", NA, "Insecta", "Arthropoda")
fill.taxon.table("Onychogomphus/Ophiogomphus"                                  , NA,NA, "Gomphidae", "Odonata", NA, "Insecta", "Arthropoda")
fill.taxon.table("Orectochilinae"                                              , NA, NA, "Gyrinidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Orthocladiinae Gen."                                         , NA,NA , "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Orthocladiini COP"                                           , NA,NA , "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Palpomyiinae"                                                , NA, "Palpomyiinae", "Ceratopogonidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Phryganea bipunctata/grandis"                                , NA, "Phryganea", "Phryganeidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Physella acuta/heterostropha"                                , NA, "Physella", "Physidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Pisidium crassum"                                            , "Pisidium crassum"   , "Pisidium" , "Sphaeriidae", "Sphaeriida", NA, "Bivalvia"  , "Mollusca")
fill.taxon.table("Planorbis c.f. carinatus"                                    , "Planorbis carinatus", "Planorbis", "Planorbidae", NA          , NA, "Gastropoda", "Mollusca")
fill.taxon.table("Polycelis nigra / tenuis"                                    , NA, "Polycelis", "Planariidae", "Tricladida", NA, NA, "Platyhelminthes")
fill.taxon.table("Polycelis nigra/tenuis"                                      , NA, "Polycelis", "Planariidae", "Tricladida", NA, NA, "Platyhelminthes")
fill.taxon.table("Polycentropus c.f.  irroratus"                               , "Polycentropus flavomaculatus", "Polycentropus", "Polycentropodidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Polypedilum bicrenatum / pullum"                             , NA, "Polypedilum", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Potamophylax c.f. rotundipennis"                             , "Potamophylax rotundipennis", "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax cingulatus / latipennis"                        , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax cingulatus / latipennis / luctuosus"            , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax cingulatus/lat."                                , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax cingulatus/latipennis"                          , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax cingulatus/latipennis/luctuosus"                , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax latipennis/cingulatus"                          , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax latipennis/luctuosus"                           , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax latipennis/luctuosus/cingulatus"                , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax luct/ lati"                                     , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Potamophylax nigricornis/rotundipennis"                      , NA, "Potamophylax", "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Proasellus coxalis/meridianus"                               , NA, "Proasellus", "Asellidae", "Isopoda", NA, "Malacostraca", "Arthropoda")
fill.taxon.table("Psectrocladius limbatellus-/sordidellus-Gr."                 , NA, "Psectrocladius", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Psectrocladius limbatellus/sordidellus"                      , NA, "Psectrocladius", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Radix ampla/auricularia"                                     , NA, "Radix", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Radix ampla/balthica/labiata"                                , NA, "Radix", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Radix auricularia / balthica / labiata"                      , NA, "Radix", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Radix balthica / labiata"                                    , NA, "Radix", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Radix balthica/labiata"                                      , NA, "Radix", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Radix c.f.  auricularia"                                     , "Radix auricularia", "Radix", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Radix ovata/peregra"                                         , NA                 , "Radix", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Radix peregra / ovata"                                       , NA                 , "Radix", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Rhantus exsoletus/latitans"                                  , NA, "Rhantus", "Dytiscidae"   , "Coleoptera"   , NA, "Insecta"  , "Arthropoda")
fill.taxon.table("Rhithrogena loyolaea/zelinkai"                               , NA, "Rhithrogena", "Heptageniidae", "Ephemeroptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila-Rhyacophila"                                     , NA, "Rhyacophila", "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila - Hyperrhyacophila"                              , NA, NA           , "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila - Hyporhyacophila"                               , NA, NA           , "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila - Rhyacophila"                                   , NA, "Rhyacophila", "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila dorsalis / nubila"                               , NA, NA           , "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila dorsalis/nubila"                                 , NA, NA           , "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila dorsalis/nubila/pascoei/simulatrix/vulgaris"     , NA, NA           , "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila fasciata/nubila"                                 , NA, NA           , "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila polonica / praemorsa"                            , NA, NA           , "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rhyacophila polonica/praemorsa"                              , NA, NA           , "Rhyacophilidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Rotatoria"                                                   , NA, NA, NA, NA, NA, NA, "Rotatoria")
fill.taxon.table("Scatophagidae"                                               , NA, NA, "Scathophagidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sericostoma flavicorne / personatum"                         , NA, "Sericostoma", "Sericostomatidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sericostoma flavicorne/personatum"                           , NA, "Sericostoma", "Sericostomatidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sericostoma personatum / flavicorne"                         , NA, "Sericostoma", "Sericostomatidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sericostoma personatum/schneideri"                           , NA, "Sericostoma", "Sericostomatidae", "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sialis c.f. nigripes"                                        , "Sialis nigripes", "Sialis", "Sialidae", "Megaloptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sialis fuliginosa/nigripes"                                  , NA               , "Sialis", "Sialidae", "Megaloptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sigara dorsalis / striata"                                   , NA               , "Sialis", "Sialidae", "Megaloptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sigara dorsalis/striata"                                     , NA               , "Sialis", "Sialidae", "Megaloptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sigara falleni/iactans"                                      , NA               , "Sialis", "Sialidae", "Megaloptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sigara falleni/longipalis/iactans-Gr."                       , NA               , "Sialis", "Sialidae", "Megaloptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Silo nigricornis / piceus"                                   , NA, "Silo", "Goeridae",  "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Silo nigricornis/piceus"                                     , NA, "Silo", "Goeridae",  "Trichoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium angustipes / velutinum"                             , NA                   , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium argyreatum/variegatum"                              , NA                   , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium c.f ornatum"                                        , "Simulium ornatum"   , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium c.f. costatum"                                      , "Simulium costatum"  , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium c.f. cryophilum"                                    , "Simulium cryophilum", "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium c.f. noelleri"                                      , "Simulium noelleri"  , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium c.f. ornatum"                                       , "Simulium ornatum"   , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium c.f. pusillum"                                      , "Simulium pusillum"  , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium c.f. reptans"                                       , "Simulium reptans"   , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium c.f. urbanum"                                       , "Simulium urbanum"   , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium c.f. vernum"                                        , "Simulium vernum"    , "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium dunfellense/urbanum"                                , NA, "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium equinum/lineatum/pseudequinum"                      , NA, "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium nigrum/pusillum"                                    , NA, "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium ornatum/trifasciatum"                               , NA, "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium posticatum / rostratum"                             , NA, "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium posticatum/rostratum"                               , NA, "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Simulium tuberosum/vulgare"                                  , NA, "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Siphlonurus aestivalis/armatus"                              , NA, "Simulium", "Simuliidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sphaeridiinae"                                               , NA, NA, "Sphaeriidae", "Sphaeriida", NA, "Bivalvia"  , "Mollusca")
fill.taxon.table("Sphaerium corneum / ovale"                                   , NA, "Sphaerium", "Sphaeriidae", "Sphaeriida", NA, "Bivalvia"  , "Mollusca")
fill.taxon.table("Sphaerium corneum/ovale"                                     , NA, "Sphaerium", "Sphaeriidae", "Sphaeriida", NA, "Bivalvia"  , "Mollusca")
fill.taxon.table("Stagnicola c.f. palustris"                                   , "Stagnicola palustris", "Stagnicola", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Stagnicola sp. JEFFREYS"                                     , NA                    , "Stagnicola", "Lymnaeidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Stenophylacini"                                              , NA, NA, "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Stenophylacini genus"                                        , NA, NA, "Limnephilidae" , "Trichoptera" , NA , "Insecta"  , "Arthropoda")
fill.taxon.table("Sympetrum sanguineum/vulgatum"                               , NA, "Sympetrum", "Libellulidae", "Odonata", NA, "Insecta"  , "Arthropoda")
fill.taxon.table("Sympetrum vulgatum/sanguineum"                               , NA, "Sympetrum", "Libellulidae", "Odonata", NA, "Insecta"  , "Arthropoda")
fill.taxon.table("Tanopodinae"                                                 , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanypodinae Gen"                                             , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanypodinae Gen."                                            , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanypodinae Gen. Sp."                                        , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanypodinae Gen. sp. THIENEMANN & ZAVREL"                    , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanypodinae Gen. Sp. THIENEMANN & ZAVREL"                    , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanypodinae genus"                                           , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanypodini"                                                  , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanytarsini Gen"                                             , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanytarsini Gen."                                            , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanytarsini Gen. Sp."                                        , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanytarsini genus"                                           , NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanytarsus brundini/curticornis"                             , NA, "Tanytarsus", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tanytarsus nigricollis/usmaensis"                            , NA, "Tanytarsus", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Tipula c.f. lateralis"                                       , "Tipula lateralis", "Tipula", "Tipulidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Trochetinae"                                                 , NA, NA , "Erpobdellidae", "Arhynchobdellida"  , NA, "Clitellata", "Annelida")
fill.taxon.table("Turbellaria Fam."                                            , NA, NA, NA, NA, NA, "Turbellaria", "Platyhelminthes")
fill.taxon.table("Tvetenia discoloripes/verralli"                              , NA, "Tvetenia", "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Unio sp. PHILIPSSON"                                         , NA, "Unio", "Unionidae"     , "Unionida"    , NA , "Bivalvia" , "Mollusca") 
fill.taxon.table("Valvata cristata O.F. MÃœLLER"                               , "Valvata cristata", "Valvata", "Valvatidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Zygoptera Fam."                                              , NA, NA, NA, "Odonata", NA, "Insecta", "Arthropoda")
    
taxontable <- rbindlist(list(taxontable, data.table(original_site_name = "Ephydatia/", species = NA, genus =  NA, family = "Spongillidae", order = "Spongilida", subclass = NA, class = "Demospongiae", phylum = "Porifera", kingdom = "Animalia", clean = TRUE)))

taxontable <- taxontable[!original_name %in% c("ungÃ¼ltig: 10057", "ungÃ¼ltig: 792")]
#- set clean to true for manually edited entries 
taxontable[, clean := TRUE]

unique(taxontable$kingdom)
taxontable[kingdom == "Protozoa"]
taxontable[phylum == "Chordata"]

unique(taxontable$phylum)   |> sort()
unique(taxontable$class)    |> sort()
unique(taxontable$subclass) |> sort()
unique(taxontable$order)    |> sort() 
#- save to or load from file 
saveRDS(taxontable, "data/original data/taxontable.rds")
taxontable <- readRDS("data/original data/taxontable.rds")


# prepare data4  --------------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_landau")]

data4 <- data3[, list(
        gr_sample_id,
        original_site_name,
        bio.least.impacted,
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
                                                                  ifelse(!is.na(phylum), phylum,0)))))))]
#- did the command above fail for some rows?
any(data4$lowest.taxon == 0)
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data4 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))

#- save to or load from file 
saveRDS(data4, paste0("data/original data/landau/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/landau/auxilliary/02_2021-07-23_data4.rds")


# sites -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/landau/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/landau/auxilliary/03_2021-07-23_sites.rds")

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
saveRDS(distance_list, paste0("data/original data/landau/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/landau/auxilliary/04_2021-07-23_distance_to_brt.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "brt_distance" = unlist(distance_list),
        "brt20"    = brt_nn$m_btype20c,
        "brt12"    = brt_nn$m_btype12
)

sites <- distance_table[sites, on = "site_id"]
#sites2 <- sites[brt_distance <= 500]
#mapview(st_as_sf(sites2))

# —————————————————————————— #
# ——— DISTANCE TO GLORIC ——— #
# —————————————————————————— #

# sites2  <- st_as_sf(sites2) |> st_transform(crs = st_crs(gloric))
# nn           <- st_nearest_feature(sites2, gloric)
# gloric_nn    <- gloric[nn,]
# 
# distance_list <-
#         map(.x = 1:nrow(sites2),
#             .f = ~ as.numeric(st_distance(x = sites2[.x, ],
#                                           y = gloric_nn[.x, ])))
# beepr::beep()
# #- save to or load from file 
# saveRDS(distance_list, paste0("data/original data/landau/auxilliary/05_",Sys.Date(),"_distance_to_gloric.rds"))
# distance_list  <- readRDS("data/original data/landau/auxilliary/05_2021-07-23_distance_to_gloric.rds")
# 
# #- create distance table 
# distance_table <- data.table(
#         "site_id" = sites2$site_id,
#         "glroic_distance" = unlist(distance_list),
#         "gloric"    = gloric_nn$Kmeans_30
# )
# 
# sites2 <- distance_table[sites2, on = "site_id"]
# sites2 <- sites2[glroic_distance <= 500]
# mapview(st_as_sf(sites2))

# ————————————————————————————————— #
# ——— DISTANCE TO GERMAN TYPES  ——— #
# ————————————————————————————————— #

sites %<>% st_as_sf() %>% st_transform(crs = st_crs(typology_germany))
nn            <- st_nearest_feature(sites, typology_germany)
tg_nn         <- typology_germany[nn, ]

distance_list <-
    map(.x = 1:nrow(sites),
        .f = ~ as.numeric(st_distance(x = sites[.x, ],
                                      y = tg_nn[.x, ])))
beepr::beep()
#- save to or load from file 
saveRDS(distance_list, paste0("data/original data/landau/auxilliary/07_",Sys.Date(),"_distance_to_german_types.rds"))
distance_list  <- readRDS("data/original data/landau/auxilliary/07_2021-07-23_distance_to_german_types.rds")

#- create distance table 
distance_table <- data.table(
    "site_id" = sites$site_id,
    "german.type.distance" = unlist(distance_list),
    "german_type"    = tg_nn$TY_CD_RW
)
setDT(sites)
sites <- distance_table[sites, on = "site_id"]
#sites <- sites2[german.type.distance <= 500]

# ———————————————————— #
# ——— Illies & BGR ——— #
# ———————————————————— #

sites |> 
        st_as_sf() |> 
        st_transform(crs = st_crs(illies)) |> 
        st_join(select(illies, NAME))      |> 
        st_transform(crs = st_crs(bgr))    |>
        st_join(select(bgr, code)) ->
    sites

# ——————————— #
# ——— FEC ——— #
# ——————————— #

sites |>
    st_as_sf() |>
    st_transform(crs = st_crs(fec)) |>
    st_join(select(fec, least.impacted)) -> sites


sites2 <- 
    sites |> 
    rename(illies = NAME,
           fec.least.impacted = least.impacted,
           bgr = code) %>%
    select(c( 
             "brt20", 
             "brt12", 
             "site_id", 
             "illies", 
             "bgr",
             "german_type",
             "fec.least.impacted", 
             "brt_distance")) %>%
    setDT()


#data5 <- data4[site_id %in% sites2$site_id]
data5 <- sites2[data4, on = "site_id"]

#- save to or load from file 
saveRDS(sites2, paste0("data/original data/landau/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,       paste0("data/original data/landau/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites <- readRDS("data/original data/landau/auxilliary/06_2021-07-23_sites2.rds")
data5 <- readRDS("data/original data/landau/auxilliary/07_2021-07-23_data5.rds")

# temporal aggregation  -------------------------------------------------------------

data5[, sample_events := uniqueN(gr_sample_id), by = "site_id"]
table(data5$sample_events)
#- taxa per sample 
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
sites2 %<>% filter(site_id %in% data5$site_id)


for (i in 1:nrow(sites2)) {
    #for (i in 1:10){
    
    #- setup for a new list
    if (i == 1)
        new.lst <- list()
    
    #- subset to focal site
    lp.sub   <- data5[site_id == sites2$site_id[i]]
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
                    "_combined_landau"
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
                                         "_combined_landau")]
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
    
    lp.progress <- i/nrow(sites2) * 100
    print(lp.progress)
    rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
    rm(i)
    gc()
}


data6 <- rbindlist(new.lst,use.names=TRUE )

data6[, richness := .N, by = "gr_sample_id"]

saveRDS(data6, paste0("data/original data/landau/auxilliary/08_",Sys.Date() , "_data6.rds"))
data6 <- readRDS("data/original data/landau/auxilliary/08_2021-07-26_data6.rds")

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


# save to file ----------------------------------------------------------------------
saveRDS (data6, paste0("data/original data/landau/",Sys.Date(),"_final.rds"))   
data6 <- readRDS("data/original data/landau/2021-08-04_final.rds")
