# ———————————————————————————————————————————————— #
# ——— Clean data for Monitoring from Germany   ——— # 
# ———————————————————————————————————————————————— #

# ———————————————————————————————————
# date: 
#       18.08.21
# files in: 
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the raw data
#       provided by the Peter Haase and from the server in Landau. Some samples are 
#       included in both data bases so I reshape them together.
# ———————————————————————————————————

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
#- functions 
source("~/my documents/R/functions/fill_taxon_table.R")
source("R/distance_to_illies_function.R")
source("R/distance_to_bgr_function.R")
# load data  ------------------------------------------------------------------------

#- Data from Hesse 
ph.samples1         <- read_excel("data/original data/peter_haase/raw_data/HE_MZB.xlsx")
ph.samples2         <- read_excel("data/original data/peter_haase/raw_data/HE_MZB2.xlsx", sheet = 2)
ph.sites1           <- read_excel("data/original data/peter_haase/raw_data/HE_STAMM.xlsx") 
ph.sites2           <- read_excel("data/original data/peter_haase/raw_data/HE_STAMM2.xls")
#- Data from all over Germany 
ld.samples          <- fread("data/original data/landau/raw_data/mzb_samples.csv")
ld.sites            <- fread("data/original data/landau/raw_data/mzb_sites.csv")
#- Data from Saxony Anhalt 
sa.samples <- fread("data/original data/gld/raw_data/gld_data/Fliess_Makrozoobenthos_Taxa.csv")
sa.sites   <- fread("data/original data/gld/raw_data/gld_data/data.csv")

taxontable       <- readRDS("data/original data/2021-08-25_taxontable.rds")
brt              <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies           <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr              <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
typology_germany <- st_read("E://Arbeit/Data/WFD_waterbodytypes_germany/AM_surfaceWaterBody-DE_GDB/AM_surfaceWaterBody-DE.gdb", layer = "AM_riverWaterBody_DE")
fec              <- st_read("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# PREPARE DATA  ---------------------------------------------------------------------
#  —————— LANDAU ——————
ld.samples <- 
        ld.samples |> 
        mutate(year = year(date)) %>%
        filter(ind_qm > 0)
#- drop columns  
ld.samples <- ld.samples[, c("date", "taxon", "site_id", "ind_qm") ]

# In samples the sites with TH in their site_id seem to have mistakes. All TH ..
# sites in the samples data go like TH_TH_xxxx while those in the sites data go
# TH_xxxx. Hence I remove the first TH 
ld.samples[, site_id := str_replace_all(site_id, "TH_TH_", "TH_")]
ld.sites  <-  ld.sites[, c("site_id", "stream", "site_name", "geom")]

#fix site coordinates 
#Geometry type PostgreSQL columns They can be converted with sf see
#https://github.com/r-dbi/RPostgres/issues/114 This converts the geom column
#which holds Postgres Geom codes to xy coordinates and also returns the
#projection.
coord <- st_as_sfc(
        structure(
                ld.sites$geom, 
                class = "WKB"
        ),
        EWKB = TRUE
)
coord2 <- 
        coord %>%
        st_coordinates() %>%
        data.frame()
ld.sites2  <- 
        bind_cols(
        ld.sites,
        coord2,
        EPSG = rep(31463, nrow(ld.sites))
) 
ld.sites2  <-  ld.sites2[,-c("geom")]
# join data sets 
ld.data <-  
        left_join(ld.samples, ld.sites2) %>% 
        setDT
rm(coord,coord2,ld.samples,ld.sites,ld.sites2)

# fix date column
ld.data[,c("date", "year", "month") := list(ymd(date), year(date), month(date))]
ld.data[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]

ld.data2 <- ld.data[,
                    list(
                            original_site_name = site_id,
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
ld.data2[, taxon := str_remove_all(taxon, "\\ Lv\\.$")]
ld.data2[, taxon := str_remove_all(taxon, "\\ Ad\\.$")]
ld.data2[, taxon := str_remove_all(taxon, "\\ sp\\.$")]
ld.data2[, taxon := str_remove_all(taxon, "\\ gen\\.$")]
ld.data2[, taxon := str_remove_all(taxon, "\\ Gen\\.$")]
ld.data2[, taxon := str_remove_all(taxon, "\\ spec\\.$")]
ld.data2[, taxon := str_remove_all(taxon, "\\ \\(.*\\)")]
ld.data2[str_detect(taxon, "/"), taxon := word(taxon, 1)]
ld.data2[taxon == "Baetis niger/digitatus", taxon := "Baetis"]
ld.data2[taxon == "Hemerodromia/Wiedemannia", taxon := "Empididae"]
ld.data2[taxon == "Leuctra digi/fusc/hipp", taxon := "Leuctra"]
ld.data2[taxon == "Pericomini/Telmatoscopini", taxon := "Psychodidae"]
ld.data2[taxon == "Rhyacophila obliterata/fasciata", taxon := "Rhyacophila"]
ld.data2 <- ld.data2[taxon != "ungÃ¼ltig: 10057"]
ld.data2 <- ld.data2[taxon != "ungÃ¼ltig: 792"]

#- check on map 
ld.sites  <- 
        ld.data2 |> 
        unique(by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = 31463)
mapview(ld.sites)

#  —————— HESSE  ——————
#- to data.table
setDT(ph.samples1)
setDT(ph.samples2)
setDT(ph.sites1)

ph.samples1 <-
        ph.samples1[,list(
                original_site_name = as.numeric(ID_RS),
                date = dmy(Datum),
                year = as.numeric(Year),
                season = ifelse(
                        Month %in% c("12", "01", "02"),
                        "winter",ifelse(Month %in% c("03", "04", "05"),
                                        "spring",ifelse(Month %in% c("06", "07", "08"), "summer", "autumn"))),
                taxon = Taxon,
                abundance = as.numeric(IZpqm)
        )]

ph.sites1 = ph.sites1[,list(
        original_site_name = as.numeric(ID_RS),
        x.coord = as.numeric(RW_GK3),
        y.coord = as.numeric(HW_GK3),
        EPSG = 31467
)]        

ph.data1 = ph.sites1[ph.samples1, on = "original_site_name"]
nrow(ph.data1) == nrow(ph.samples1)
ph.data1 <- ph.data1[!is.na(taxon)]      

ph.samples2 %<>% rename(ID_PN = "Prodenahme-ID")
ph.samples2[, ID_PN := as.character(ID_PN)]

ph.samples2 <-
        ph.samples2[,
                 list(
                         original_site_name = as.numeric(Site),
                         date = dmy(Datum),
                         taxon = Tax,
                         x.coord = as.numeric(RW),
                         y.coord = as.numeric(HW),
                         EPSG = 31467,
                         abundance = as.numeric(izqm)
                 )]
#- add season 
ph.samples2[, c("year", "season") := list(
        year(date),
        ifelse(month(date) %in% c("12", "01","02"), "winter",
               ifelse(month(date) %in% c("03", "04", "05"), "spring",
                      ifelse(month(date) %in% c("06","07","08"), "summer", "autumn"))))]

#- Combine samples 1 and 2. use.names ensures that columns are matched by names not by position.  
ph.data2 <- rbindlist(list(ph.samples2, ph.data1), use.names = TRUE)

#- taxonomy 
ph.data2[taxon %in% c("Baetis fuscatus/scambus", "Baetis alpinus/lutheri/melanonyx/vardarensis", "Baetis lutheri/vardarensis", "Caenis beskidensis/pseudorivulorum", 
                   "Caenis luctuosa/macrura",      "Chaetopteryx fusca/villosa", "Coenagrion puella/pulchellum-Gr.", "Dikerogammarus haemobaphes/villosus",
                   "Drusus annulatus/biguttatus", "Dugesia lugubris/polychroa", "Ecdyonurus torrentis/venosus", "Elmis aenea/maugetii/rietscheli/rioloides"  , 
                   "Elmis aenea/maugetii",   "Gammarus fossarum/pulex",  "Glossosoma boltoni/conformis",  "Halesus digitatus/tesselatus", 
                   "Halesus digitatus/radiatus/tesselatus",  "Hydropsyche pellucidula/incognita", "Liponeura brevirostris/decipiens/vimmeri", 
                   "Melampophylax mucoreus/nepos", "Micropterna lateralis/sequax", "Mystacides longicornis/nigra", "Naididae/Tubificidae",
                   "Nebrioporus depressus/elegans",  "Polycelis nigra/tenuis",  "Potamophylax cingulatus/latipennis/luctuosus", "Radix balthica/labiata"   , 
                   "Radix ovata/peregra",     "Rhyacophila polonica/praemorsa", "Rhyacophila dorsalis/nubila", "Rhyacophila dorsalis/nubila/pascoei/simulatrix/vulgaris", 
                   "Sericostoma flavicorne/personatum", "Silo nigricornis/piceus", "Simulium dunfellense/urbanum" , "Simulium argyreatum/variegatum"), 
      taxon := word(taxon, 1)]


ph.data2[taxon == "Chaetopterygini/Stenophylacini", taxon := "Limnephilidae"]                         
ph.data2[taxon == "Habroleptoides/Paraleptophlebia", taxon := "Leptophlebiidae"]                 
ph.data2[taxon == "Crangonyx/Niphargus", taxon := "Crangonyctidae"]                         
ph.data2[taxon == "Corduliidae/Libellulidae", taxon := "Odonata"] 
ph.data2[taxon == "Ceratopogoninae/Palpomyiinae", taxon := "Ceratopogonidae"]  
ph.data2[taxon == "Nemoura/Nemurella", taxon := "Nemouridae"]                    
ph.data2[taxon == "Jungiella/Psychoda/Tinearia", taxon := "Psychodidae"] 
ph.data2[, taxon := str_remove(taxon, "\\ sp\\.")]
ph.data2[, taxon := str_remove(taxon, "\\ Gen\\.")]
ph.data2[, taxon := str_remove(taxon, "\\ Lv\\.")]
ph.data2[, taxon := str_remove(taxon, "\\ Ad\\.")]
ph.data2[, taxon := str_remove(taxon, "\\ Agg\\.")]
ph.data2[, taxon := str_remove(taxon, "\\ Gr\\.")]

ph.data.st <- 
        ph.data2 |>
        filter(!is.na(x.coord) & !is.na(y.coord)) |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = ph.data2$EPSG[1]) |> 
        st_transform(crs = 31463) 
ph.coords    <- st_coordinates(ph.data.st)
ph.data.set2 <- 
        ph.data.st |> 
        mutate(x.coord = ph.coords[,1], 
               y.coord = ph.coords[,2],
               EPSG     = 31463,
               data.set = "hesse") |> 
        st_drop_geometry() |> 
        setDT()
    
ph.data.set2 <- ph.data.set2[original_site_name != 14146]

ph.sites <- 
        ph.data.set2 |>
        unique(by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = 31463)
mapview(ph.sites)

# SAXONY ANHALT —————————
sa.samples2 <-
        sa.samples |>
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
        setDT()

sa.samples2[, site_id := .GRP, by = original_site_name]
sa.samples2[, date_id := .GRP, by = date]

#- add leading zeros 
sa.samples2[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
sa.samples2[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]
sa.samples2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_gld")]

sa.sites2 <- 
        sa.sites |> 
        mutate(data.set = "gld", 
               EPSG = 32632) |> 
        select(original_site_name = Mst_Nr_Bio, 
               x.coord = RW,
               y.coord = HW, 
               data.set,
               EPSG
        )

sa.data2 <- sa.sites2[sa.samples2, on = "original_site_name"]    
sa.data2[, c("site_id", "date_id", "gr_sample_id", "DV") := NULL]

sa.data2 <- sa.data2[!taxon %in% c("Salmo trutta f. fario", "Cottus gobio", "Lampetra planeri", 
                             "Barbatula barbatula", "Gasterosteus aculeatus (Binnenform)", "Pungitius pungitius",
                             "Esox lucius", "Cobitis taenia", "Gasterosteus aculeatus", "Perca fluviatilis", "Gobio gobio",
                             "Barbus barbus", "Lota lota", "Tinca tinca", "Ameiurus nebulosus", "Misgurnus fossilis",
                             "Lampetra fluviatilis", "Silurus glanis", "Rutilus rutilus")]

sa.data2[taxon == "Capnia/Zwicknia", taxon := "Capniidae"]
sa.data2[taxon == "Hydroptila sparsa-Gruppe", taxon := "Hydroptila sparsa"]
sa.data2[taxon == "Dixa maculata - Gruppe", taxon := "Dixa maculata"]
sa.data2[taxon == "Leuctra hippopus-Gruppe", taxon := "Leuctra hippopus"]

sa.data.st <- 
        sa.data2 |>
        filter(!is.na(x.coord) & !is.na(y.coord)) |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = sa.data2$EPSG[1]) |> 
        st_transform(crs = 31463) 
sa.coords    <- st_coordinates(sa.data.st)
sa.data.set2 <- 
        sa.data.st |> 
        mutate(x.coord = sa.coords[,1], 
               y.coord = sa.coords[,2],
               EPSG     = 31463,
               data.set = "gld") |> 
        st_drop_geometry() |> 
        setDT()

sa.sites <- 
        sa.data.set2 |>
        unique(by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs =31463)
mapview(sa.sites)

# Combine all  —————————
#- combine the three data sets into one 
data2 <- rbindlist(list(ph.data.set2, ld.data2, sa.data.set2), fill = TRUE)
#- remove all data from the individual data sets 
rm(list = ls()[grepl("^sa\\.", ls())])
rm(list = ls()[grepl("^ph\\.", ls())])
rm(list = ls()[grepl("^dt\\.", ls())])

data2[, new_site_id := .GRP, by = c("x.coord", "y.coord")]

double_sites <- data2[,uniqueN(data.set), by = "new_site_id"]
unique(double_sites$V1)
double_sites = double_sites[V1 == 2]


#- empty vector to hold the original_site_name to remove the sites later. 
rm.id <- c()

for (i in 1:nrow(double_sites)){
        # get all data from focal site 
        i.data <- data2[new_site_id == double_sites$new_site_id[i]]
        #- get a vector with unique dates per data.set 
        #- unique dates Hesse 
        i.dt.he <- i.data[data.set == "hesse", unique(date)]
        i.dt.gr <- i.data[data.set == "landau", unique(date)]
        #- do they have common elements?
        i.intersection <-base::intersect(i.dt.he, i.dt.gr)
        #- When there are no intersections they have no dates in common. 
        #- That means the data sets share sites but not samples. 
        #- We can skip this sample 
        if (length(i.intersection) != 0){
                #- which dates are the culprits 
                i.intersection2<- which(i.dt.he %in% i.dt.gr)
                #- with the date positions (i.intersection2) we can extract the dates 
                i.dates <- i.dt.he[i.intersection2]
                #- i.intersection2 can be either way - maybe it must be i.dt.gr %in% i.dt.he 
                #- This would be the case when length(i.intersection) != length(i.intersection2). 
                #- Let's see 
                if (length(i.intersection) != length(i.intersection2)){
                        i.intersection2 <- which(i.dt.gr %in%  i.dt.he)    
                        i.dates <- i.dt.he[i.intersection2]
                } 
                #- Now we loop over the elements of i.dates 
                for (k in i.dates){
                        #- get the Hesse data on the focal date (i.e. k)
                        k.hes <- i.data[data.set == "hesse" & date == k]
                        #- extract taxon names 
                        k.hes <- k.hes$taxon 
                        #- get the 'all German' date from the focal date (i.e. k)
                        k.ger <- i.data[data.set == "landau" & date == k]
                        k.ger <- k.ger$taxon
                        #- are they equal? 
                        k.ger.l <- length(k.ger)
                        k.hes.l <- length(k.hes)
                        k.overlap <- mean(
                                length(intersect(k.ger, k.hes))/k.ger.l,
                                length(intersect(k.hes, k.ger))/k.hes.l
                        )
                        #- if the overlap between the taxa lists is greater than 75%. 
                        #- save the Hesse original_site_name to remove it in a second step. 
                        if (k.overlap > 0.75){
                                k.add.id <- length(rm.id) + 1
                                rm.id[k.add.id] <- i.data[data.set == "hesse" & date == k, unique(original_site_name)]
                                
                                print(
                                        paste(i, "/", nrow(double_sites), " ——— ", k,  "——— added to rm.id")
                                )
                                
                        } else {
                                print(
                                        paste(i, "/", nrow(double_sites), " ——— ", k,  "——— dates do not overlap")
                                )
                        }
                        rm(list = ls()[grepl("^k.", ls())])
                }
        } else {
                print(
                        paste(i, "/", nrow(double_sites), " ——— no common dates")
                )
        } 
        rm(list = ls()[grepl("^i.", ls())])
}
rm(i,k)
#- remove double sites with rm.id
data2 <- data2[!original_site_name %in% rm.id]
rm(rm.id, double_sites)

sites <- 
        data2 |>
        unique(by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs =31463)
mapview(sites)

data2[,taxon := str_remove(taxon, "-Gruppe")]
data2[taxon == "Polycelis nigra/tenuis", taxon := "Polycelis"]
data2[taxon == "Copepoda - Copepodid", taxon := "Copepoda"]
data2[taxon == "Culiseta (Culiseta)", taxon := "Culiseta"]
data2[taxon == "Dugesia lugubris/polychroa", taxon := "Dugesia"]
data2[taxon == "Glyptotendipes pallens/glaucus", taxon := "Glyptotendipes"]
data2[taxon == "Helophorus minutus/paraminutus", taxon := "Helophorus"]
data2[taxon == "Leptophlebia (Paraleptophlebia)", taxon := "Leptophlebia"]
data2[taxon == "Microtendipes chloris/pedellus", taxon := "Microtendipes"]
data2[taxon == "Orthocladius (Eudactylocladius)", taxon := "Orthocladius"]
data2[taxon %in% c("Pisidium 2","Pisidium 4"), taxon := "Pisidium"]
data2[taxon == "Simulium (Eusimulium) aureum", taxon := "Simulium aureum"]
data2[taxon == "Tipula (Platytipula)", taxon := "Tipula"]
data2[taxon == "Enochrus quadripunctatus/testaceus", taxon := "Enochrus"]
data2[taxon == "Simulium (Nevermannia) crenobium", taxon := "Simulium crenobium"]
data2[taxon == "Simulium (Tetisimulium) bezzii", taxon := "Simulium bezzii"]

TU     <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

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
taxontable[clean == FALSE]

fill.taxon.table("Nemertini", NA, NA, NA, NA, NA, NA, "Nemertea")
fill.taxon.table("Rotatoria", NA, NA, NA, NA, NA, NA, "Rotifera")
fill.taxon.table("Nematoda-Mermithidae", NA,NA, "Mermithidae", "Mermithida", NA, "Adenophorea", "Nematoda")
fill.taxon.table("Telmatoscopini", NA,NA, "Psychodidae", "Diptera", NA, "Insecta", "Arthropoda")
taxontable[, clean := TRUE]

taxontable[phylum == "Artrhopoda", phylum := "Arthropoda"]
taxontable[class == "\tMalacostraca", class := "Malacostraca"]
taxontable[phylum == "Plathyheminthes"]
taxontable[original_name == "Planaria n.d.", c("genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .("Planaria", "Planariidae", "Tricladida", NA, NA, "Platyhelminthes", "Animalia")]
taxontable[original_name == "Acariformes", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(NA, NA, NA, NA, "Acari", "Arachnida", "Arthropoda", "Animalia")]
fill.taxon.table("Lindnerina", NA, "Tipula", "Tipulidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Dugesia spec. GIRARD", NA, "Dugesia", "Dugesiidae", "Tricladida", NA, NA, "Platyhelminthes")


taxontable[kingdom   == "Protozoa"]
taxontable[subclass == "Acari"]
taxontable[genus    == "Dugesia"]

saveRDS(taxontable, paste0("data/original data/",Sys.Date(),"_taxontable.rds"))

# COMBINE DATA SETS -----------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

unique(data3$kingdom)
unique(data3$phylum)
unique(data3$class)
unique(data3$subclass)
unique(data3$order)

data3 <- data3[phylum != "Chordata"]


#- add site and date ids 
data3[, date_id := .GRP, by = "date"]

#- add leading zeros 
data3[, new_site_id := case_when(
        nchar(trunc(new_site_id)) == 1 ~ paste0("0000", new_site_id),
        nchar(trunc(new_site_id)) == 2 ~ paste0("000",  new_site_id),
        nchar(trunc(new_site_id)) == 3 ~ paste0("00",   new_site_id),
        nchar(trunc(new_site_id)) == 4 ~ paste0("0",    new_site_id),
        nchar(trunc(new_site_id)) == 5 ~ paste0(        new_site_id))]
data3[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

#- add gr_sample_id
data3[,gr_sample_id := paste0("site_", new_site_id, "_date_", date_id,"_monitoring_germany")]

data4 <- data3[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
        season,
        site_id = new_site_id,
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
        data.set = "Monitoring data from Germany"
        
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
saveRDS(data4, paste0("data/original data/monitoring_germany/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/monitoring_germany/auxilliary/02_2021-08-26_data4.rds")

# sites -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/monitoring_germany/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/monitoring_germany/auxilliary/03_2021-07-23_sites.rds")

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
saveRDS(distance_list, paste0("data/original data/monitoring_germany/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
#distance_list  <- readRDS("data/original data/landau/auxilliary/04_2021-08-26_distance_to_brt.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "brt_distance" = unlist(distance_list),
        "brt20"    = brt_nn$m_btype20c,
        "brt12"    = brt_nn$m_btype12
)

sites <- distance_table[sites, on = "site_id"]

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
saveRDS(distance_list, paste0("data/original data/monitoring_germany/auxilliary/07_",Sys.Date(),"_distance_to_german_types.rds"))
#distance_list  <- readRDS("data/original data/monitoring_germany/auxilliary/07_2021-07-23_distance_to_german_types.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "german.type.distance" = unlist(distance_list),
        "german_type"    = tg_nn$TY_CD_RW
)
setDT(sites)
sites <- distance_table[sites, on = "site_id"]

# —————————————— #
# ——— Illies ——— #
# —————————————— #
sites3 <- 
        sites |> 
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


# ——————————— #
# ——— FEC ——— #
# ——————————— #

sites5 <- 
        sites4 |>
        st_transform(crs = st_crs(fec)) |>
        st_join(select(fec, least.impacted)) 

#- reshape data 
sites6 <-
        sites5 |> 
        rename(illies = NAME, bgr = code, fec.least.impacted = least.impacted) %>%
        select(c("brt20", "brt12", "site_id", "illies", "bgr", "fec.least.impacted", "brt_distance", "bgr_distance", "illies_distance", "german_type")) %>%
        st_drop_geometry() |> 
        setDT()

#- add combined types BRT + region
sites6[, brt12_illies := paste0(brt12, "_", illies)]
sites6[, brt12_bgr := paste0(brt12, "_", bgr)]

#- join sites with data 
data5 <- sites6[data4, on = "site_id"]

# #- remove data before 2005 
# data5 <- data5[year >=2005]

#- save to or load from file 
saveRDS(sites6, paste0("data/original data/monitoring_germany//auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,  paste0("data/original data/monitoring_germany/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites2 <- readRDS("data/original data/monitoring_germany/auxilliary/06_2021-08-04_sites2.rds")
data5 <- readRDS("data/original data/monitoring_germany/auxilliary/07_2021-08-27_data5.rds")

# Statistics ------------------------------------------------------------------------
uniqueN(sites6$site_id)
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

# — sites 
sites6 |> filter(brt_distance <= 500) |> count()
sites6 |> filter(fec.least.impacted) |> count()
data5  |> filter(year>2004) |> unique(by = "site_id") |> count()
data5  |> filter(year>2004) |> unique(by = "site_id") |>  filter(fec.least.impacted) |> count()
sites6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
data5  |> filter(year>2004) |> unique(by = "site_id") |>  filter(brt_distance <= 500) |> count()
data5  |> filter(year>2004) |> unique(by = "site_id") |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()

# TEMPORAL AGGREGATION  -------------------------------------------------------------

#- save data with samples before 2005 
data5.save <- copy(data5)
data5      <- data5[year>2004] 
data5[, sample_events := uniqueN(gr_sample_id), by = "site_id"]
table(data5$sample_events)
#- taxa per sample 
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
sites6 %<>% filter(site_id %in% data5$site_id)

for (i in 1:nrow(sites6)) {
        #for (i in 1:10){
        
        #- setup for a new list
        if (i == 1)
                new.lst <- list()
        
        #- subset to focal site
        lp.sub   <- data5[site_id == sites6$site_id[i]]
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
        
        lp.progress <- i/nrow(sites6) * 100
        print(lp.progress)
        rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
        rm(i)
        gc()
}


data6 <- rbindlist(new.lst,use.names=TRUE )

data6[, richness := .N, by = "gr_sample_id"]

saveRDS(data6, paste0("data/original data/monitoring_germany/auxilliary/08_",Sys.Date() , "_data6.rds"))
data6 <- readRDS("data/original data/monitoring_germany/auxilliary/08_2021-08-20_data6.rds")


# SUMMARY STATISTICS AGGREGATED -----------------------------------------------------
data6[, uniqueN(gr_sample_id)]
data6[brt_distance < 500, uniqueN(gr_sample_id)]
data6[fec.least.impacted == TRUE, uniqueN(gr_sample_id)]
data6[fec.least.impacted == TRUE & brt_distance < 500, uniqueN(gr_sample_id)]


# save to file ----------------------------------------------------------------------
saveRDS (data5, paste0("data/original data/monitoring_germany/", Sys.Date(), "_final.rds"))
saveRDS (data6, paste0("data/original data/monitoring_germany/",Sys.Date(),"_final_aggregated.rds"))   
data6 <- readRDS("data/original data/landau/2021-08-04_final.rds")


