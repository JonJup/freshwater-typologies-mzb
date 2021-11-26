# ———————————————————————————————————————————————— #
# ——— Clean data for Monitoring from Germany   ——— # 
# ———————————————————————————————————————————————— #

# ———————————————————————————————————
# date created:  18.08.21
# date last modified:  22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data
# provided by the Peter Haase, the GLD and from the server in Landau. Some samples are
# included in both data bases so I reshape them together.
# Temporal aggregation: Yes 
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

# load data  ------------------------------------------------------------------------

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxontable.rds")

#- Data from Hesse 
ph.samples1         <- read_excel("data/01_original_data/monitoring_germany/raw_data/HE_MZB.xlsx")
ph.samples2         <- read_excel("data/01_original_data/monitoring_germany/raw_data/HE_MZB2.xlsx", sheet = 2)
ph.sites1           <- read_excel("data/01_original_data/monitoring_germany/raw_data/HE_STAMM.xlsx") 
ph.sites2           <- read_excel("data/01_original_data/monitoring_germany/raw_data/HE_STAMM2.xls")
#- Data from all over Germany 
ld.samples          <- fread("data/01_original_data/monitoring_germany/raw_data/mzb_samples.csv")
ld.sites            <- fread("data/01_original_data/monitoring_germany/raw_data/mzb_sites.csv")
#- Data from Saxony Anhalt 
sa.samples <- fread("data/01_original_data/monitoring_germany/raw_data/Fliess_Makrozoobenthos_Taxa.csv")
sa.sites   <- fread("data/01_original_data/monitoring_germany/raw_data/data.csv")
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")


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

taxontable <- update_taxonomy(TU)

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
saveRDS(data4, paste0("data/01_original_data/monitoring_germany/auxilliary/02_",Sys.Date(),"_data_before_typology.rds"))
data4 <- readRDS("data/01_original_data/monitoring_germany/auxilliary/")
# sites -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/monitoring_germany/",Sys.Date(),"_final_non_aggregated.rds"))
data5 <- readRDS("data/01_original_data/monitoring_germany/")

# TEMPORAL AGGREGATION  -------------------------------------------------------------
source("R/newest_sample.R")
data5 <- data5[year>2004] 
data6 <- newest_sample(data5)
saveRDS (data6, paste0("data/01_original_data/monitoring_germany/",Sys.Date(),"_final_aggregated.rds"))  

# Statistics ------------------------------------------------------------------------

data6[, uniqueN(gr_sample_id)]
data6[distance <= 500, uniqueN(gr_sample_id)]
data6[least.impacted == TRUE, uniqueN(gr_sample_id)]
data6[distance <= 500 & least.impacted == TRUE, uniqueN(gr_sample_id)]


