# —————————————————————————— #
# ——— Clean Rivpacs data ——— # 
# —————————————————————————— #


# ———————————————————————————————————
# date: 
#       15.07.21
# files in: 
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the raw data
#       provided from Rivpacs. 
# ————————————————

# setup -----------------------------------------------------------------------------

pacman::p_load(
        biotic, 
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

# load data  ------------------------------------------------------------------------

samples    <- read_excel("data/original data/rivpacs/raw_data/Taxa (Raw Data).xlsx") 
sites      <- read_excel("data/original data/rivpacs/raw_data/Sites.xlsx") 
samples2   <- read_excel("data/original data/rivpacs/raw_data/Samples.xlsx")
        
taxontable <- readRDS("data/original data/2021-08-27_taxontable.rds")
brt        <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies     <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr        <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# prepare data  ---------------------------------------------------------------------

setDT(samples)
setDT(samples2)
setDT(sites)


sites <- sites[, c("Site ID", "Latitude", "Longitude")]
samples2 <- samples2[, c("Site ID", "Season", "Sample Date")]
samples <- sites[samples, on = "Site ID"]
samples <- samples2[samples, on = c("Site ID", "Season")]
samples <- rename(samples, record.type = 'Record Type')
samples <- rename(samples, date = 'Sample Date')
#- rows of the other record type just summarize the PA rows.
samples <- samples[record.type == "PA"]

data2 <- 
       samples |> 
        select(c("Site ID", "Season", "NBN Name", "Num Ab (Full)", "Longitude", "Latitude", "date")) |> 
        rename(original_site_name = 'Site ID',
               seasons = Season,
               taxon   = 'NBN Name', 
               abundance = 'Num Ab (Full)',
               x.coord = Longitude,
               y.coord = Latitude) |> 
        mutate( 
               EPSG = 4326, 
               data.set = "rivpacs")


data2[, date := ymd(date)]
data2[, year := year(date)]

#- save to or load from file 
saveRDS(data2, paste0("data/original data/rivpacs/auxilliary/01_",Sys.Date(),"_data.rds"))
data2 <- readRDS("data/original data/rivpacs/auxilliary/01_2021-07-19_data.rds")

# taxonomy --------------------------------------------------------------------------
(TU <- sort(unique(data2$taxon)))


#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

#- manual pre fixes 
data2[, taxon := str_remove(taxon, "\\ group$")]
data2[, taxon := str_remove(taxon, "\\ sp\\.$")]
data2[, taxon := str_remove(taxon, "\\ L\\.$")]
data2[, taxon := str_remove(taxon, "\\ \\(.*\\)")]


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
fill.taxon.table("Diamesinae", NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Pericoma canescens", "Pericoma canescens", "Pericoma",  "Psychodidae", "Diptera", NA, "Insecta", "Arthropoda")
#- set clean to true for manually edited entries 
taxontable[, clean := TRUE]

unique(taxontable$kingdom)
unique(taxontable$phylum)   |> sort()
unique(taxontable$class)    |> sort()
unique(taxontable$subclass) |> sort()

#- save to or load from file 
saveRDS(taxontable, "data/original data/taxontable.rds")
taxontable <- readRDS("data/original data/taxontable.rds")


# data4 -----------------------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

sort(unique(data3$kingdom))
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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_rivpacs")]

data4 <- data3[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
        season = str_to_lower(seasons),
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

#- save to or load from file 
saveRDS(data4, paste0("data/original data/rivpacs/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/rivpacs/auxilliary/02_2021-07-26_data4.rds")


# SITES -----------------------------------------------------------------------------
#- drop old sites
#data4 <- data4[year >= 2000]

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/rivpacs/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/rivpacs/03_2021-07-19_sites.rds")

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
saveRDS(distance_list, paste0("data/original data/rivpacs/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/rivpacs/auxilliary/04_2021-08-27_distance_to_brt.rds")

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

# ——————————— #
# ——— BGR ——— #
# ——————————— #
sites4 <- 
        sites3 |>
        st_transform(crs = st_crs(bgr)) |>
        st_join(select(bgr, code)) 

#- Distance to the closest border of an Biogeographical Region
sites4$bgr_distance <- sapply(1:nrow(sites), distance.to.bgr, sites4)

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
saveRDS(sites6, paste0("data/original data/rivpacs/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,  paste0("data/original data/rivpacs/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites2 <- readRDS("data/original data/rivpacs/auxilliary/06_2021-08-27_sites2.rds")
data5  <- readRDS("data/original data/rivpacs/auxilliary/07_2021-08-27_data5.rds")

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
data5[fec.least.impacted == TRUE & brt_distance <= 500, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE & brt_distance <= 500 & year >= 2004, uniqueN(gr_sample_id)]


# temporal aggregation --------------------------------------------------------------

data5[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]

data5[, sample_events := uniqueN(gr_sample_id), by = "site_id"]
sites <- sites2 
for (i in 1:nrow(sites)){
        if (i == 1) 
                new.lst <- list()
        # if (i %% 250 == 0) 
        #         print(i)

        lp.sub  <- data5[site_id == sites$site_id[i]]
        lp.n    <- unique(lp.sub$sample_events)
        if (lp.n == 1) {
                new.lst[[i]] <- data5[site_id == sites$site_id[i]]
                next()
        }
        lp.tab  <- table(lp.sub$lowest.taxon)
        lp.tab2 <- lp.tab/lp.n
        lp.sub2 <- lp.sub[lowest.taxon %in%  names(lp.tab2)[lp.tab2 >= 0.5]]
        lp.sub2[, abundance := sum(abundance), by = "lowest.taxon"]
        lp.sub2 <- unique(lp.sub2, by = "lowest.taxon")
        lp.sub2[, gr_sample_id := paste0(site_id, "_combined_rivpacs")]
        lp.sub2[, date := NA]
        lp.sub2[, year := ifelse(uniqueN(year) == 1, year, NA)]
        lp.sub2[, season := ifelse(uniqueN(season) == 1, season, NA)]
       
        new.lst[[i]] <- lp.sub2
        rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
        rm(i)
        gc()
}

data6 <- rbindlist(new.lst)

data6[, brt12_illies := paste0(brt12,"_",illies)]
data6[, brt12_bgr   := paste0(brt12,"_",bgr)]
saveRDS (data6, paste0("data/original data/rivpacs/auxilliary/08_",Sys.Date(),"_data6.rds"))   
data6 <- readRDS("data/original data/rivpacs/auxilliary/08_2021-07-")


# SUMMARY STATISTICS AGGREGATED -----------------------------------------------------
data6[, uniqueN(gr_sample_id)]
data6[brt_distance < 500, uniqueN(gr_sample_id)]
data6[fec.least.impacted == TRUE, uniqueN(gr_sample_id)]




# SAVE TO FILE  ---------------------------------------------------------------------
saveRDS (data5, paste0("data/original data/rivpacs/",Sys.Date(),"_final.rds"))   
saveRDS (data6, paste0("data/original data/rivpacs/",Sys.Date(),"_final_aggregated.rds"))   
data6       <- readRDS("data/original data/rivpacs/2021-08-02_final.rds")
