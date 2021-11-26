#  -------------------------------  # 
#  ----- Clean SALAMANDER data ---  # 
#  -------------------------------  #


# ----------------------------
# date: 
#       24.08.21
# files in: 
#
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       In this script I create a harmonized spatial data set from the SALAMANDER data from the Czech Republic
# ----------------------------

# SETUP  -----------------------------------------------------------------------------

pacman::p_load(
        here,
        taxize,
        data.table,
        sf,
        sp,
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
source("R/fill_taxon_table.R")
source("R/distance_to_illies_function.R")
source("R/distance_to_bgr_function.R")

# LOAD DATA  ------------------------------------------------------------------------

data       <- read_excel("data/original data/biodrought/raw_data/Data source CZ for Jonathan_River Typology_fin1.xlsx", sheet = 2) 

taxontable <- readRDS("data/original data/2021-08-24_taxontable.rds")
brt        <- st_read  ("E://Arbeit/Data/broad_river_types/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")
illies     <- st_read  ("E://Arbeit/Data/Illies_freshwater_ecoregions/Ecoregions.shp")
bgr        <- st_read  ("E://Arbeit/Data/eea_bioregions/BiogeoRegions2016.shp")
fec        <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

# PREPARE DATA   ---------------------------------------------------------------------
# extract site data 
data1        <- data[1:6, ]
data1.site   <- data1[6,-1] |> unlist() |> unname()
data1.sample <- data1[3,-1] |> unlist() |> unname()
data1.date   <- c("22.10.2004",	"11.04.2005",	"20.04.2000",	"18.10.2000",	"06.05.1998",	"06.10.1998",	"09.04.1997",	"17.10.1997",	"02.05.1999",	"08.10.1999",	"14.10.2004",	"08.04.2005",	"01.04.2000",	"01.10.2000",	"27.03.1997",	"16.10.1997",	"10.04.1997",	"16.10.1997",	"23.04.1997",	"29.10.1997",	"20.05.1998",	"28.10.1998",	"10.04.1997",	"16.10.1997",	"22.04.2004",	"04.10.2004",	"07.05.2005",	"23.04.1998",	"15.10.1998",	"06.05.1997",	"20.10.1997",	"14.04.1998",	"24.10.1998",	"09.04.1997",	"17.10.1997",	"10.10.2007",	"06.05.1999",	"13.10.1999",	"15.04.1998",	"30.10.1998",	"20.05.1998",	"29.10.1998",	"25.03.1997",	"13.10.1997",	"05.10.1998",	"22.04.1998",	"24.10.1998",	"01.04.1997",	"07.11.1997",	"14.04.1998",	"30.10.1998",	"23.04.1997",	"29.10.1997",	"23.04.1997",	"01.10.1997",	"03.04.1997",	"19.10.1997",	"09.04.1997",	"29.10.1997",	"22.04.1998",	"25.10.1998",	"21.04.1999",	"04.11.1999",	"17.04.2003",	"25.10.2004",	"27.03.1997",	"30.10.1997",	"08.10.2015",	"07.10.2015",	"22.04.1998",	"30.10.1998",	"01.04.1997",	"06.11.1997",	"11.05.2004",	"19.10.2004",	"25.03.1997",	"13.10.1997",	"01.04.1997",	"07.11.1997",	"21.09.2003",	"17.04.2004",	"19.09.2004",	"13.05.2005",	"02.05.1999",	"08.10.1999",	"20.10.2015",	"24.09.2004",	"22.04.2005",	"18.05.1998",	"28.10.1998",	"01.10.1998",	"05.10.2007",	"15.04.1998",	"30.10.1998",	"27.03.1997",	"29.10.1997",	"05.05.1999",	"13.10.1999",	"04.05.2000",	"01.11.2000",	"04.05.2000",	"01.11.2000",	"06.05.1999",	"14.10.1999",	"23.04.1998",	"25.10.1998",	"14.04.2005",	"12.05.2000",	"06.10.2000",	"05.11.2008",	"05.05.1999",	"05.05.1999",	"14.10.1999",	"03.05.2000",	"31.10.2000",	"14.04.1998",	"30.10.1998",	"07.10.2015",	"14.04.1999",	"27.10.1999",	"26.09.2003",	"23.04.1998",	"25.10.1998",	"22.04.1998",	"24.10.1998",	"05.05.1999",	"14.10.1999",	"22.04.1999",	"05.11.1999",	"22.04.1999",	"05.11.1999",	"01.04.1997",	"06.11.1997",	"14.10.2005",	"19.04.2006",	"14.04.1997",	"14.10.1997",	"01.04.1997",	"06.11.1997",	"18.03.1997",	"20.10.1997",	"21.04.1998", "13.10.1998")
data1.coords <- data1[1,-1] |> unlist() |> unname()
data1.coords <- data.table(original = data1.coords)
data1.coords[, x.original := str_extract(string = original, pattern = ".*N")]
data1.coords[, y.original := str_extract(string = original, pattern = "N, .*")    |>  str_remove(pattern = "N,")                               |>  str_trim()]
data1.coords[, x.degree   := str_extract(string = x.original, pattern = ".*°")    |>  str_remove(pattern = "°")                                |>  str_trim()]
data1.coords[, y.degree   := str_extract(string = y.original, pattern = ".*°")    |>  str_remove(pattern = "°")                                |>  str_trim()]
data1.coords[, x.minutes  := str_extract(string = x.original, pattern = "°.*\\.") |>  str_remove(pattern = "°") |> str_remove(pattern = "\\.") |>  str_trim()]
data1.coords[, y.minutes  := str_extract(string = y.original, pattern = "°.*\\.") |>  str_remove(pattern = "°") |> str_remove(pattern = "\\.") |>  str_trim()]
data1.coords[, x.seconds  := str_extract(string = x.original, pattern = "\\..*'") |>  str_remove(pattern = "'") |> str_remove(pattern = "\\.") |>  str_trim()]
data1.coords[, y.seconds  := str_extract(string = y.original, pattern = "\\..*'") |>  str_remove(pattern = "'") |> str_remove(pattern = "\\.") |>  str_trim()]
#- there is a few problematic entry 
data1.coords[str_detect(original, "17°32"), c("y.original", "x.degree", "y.degree", "x.minutes", "y.minutes", "x.seconds", "y.seconds") := 
                     .("17°32'51", "49", "17", "52", "32", "53", "51")]
data1.coords[y.minutes == "21'50",  c("x.minutes", "y.minutes", "x.seconds", "y.seconds") := .("33", "21", "25", "50")]
data1.coords[is.na(y.minutes),  c("x.minutes", "y.minutes", "x.seconds", "y.seconds")     := .("30", "58", "54", "3")]
data1.coords[y.minutes == "45'33",  c("x.minutes", "y.minutes", "x.seconds", "y.seconds") := .("26", "45", "19", "33")]
data1.coords[y.minutes == "55'17",  c("x.minutes", "y.minutes", "x.seconds", "y.seconds") := .("30", "55", "31", "17")]
data1.coords[y.minutes == "19'46",  c("x.minutes", "y.minutes", "x.seconds", "y.seconds") := .("50", "19", "45", "46")]
data1.coords[y.minutes == "49'14",  c("x.minutes", "y.minutes", "x.seconds", "y.seconds") := .("33", "49", "43", "14")]

#- put together new coordinates 
data1.coords[, x.new := paste0(x.degree, "°", x.minutes, "!", x.seconds, "§", "E")]
data1.coords[, y.new := paste0(y.degree, "°", y.minutes, "!", y.seconds, "§", "N")]
data1.coords[, x.new := as.numeric(char2dms(data1.coords$x.new, chd = "°", chm = "!", chs = "§"))]
data1.coords[, y.new := as.numeric(char2dms(data1.coords$y.new, chd = "°", chm = "!", chs = "§"))]

#- check on map 
site <-
        data1.coords |> 
        st_as_sf(coords = c("y.new", "x.new"), crs = 4326) 
mapview(site)

data1 <- data.table(site = data1.site, 
                    sample = data1.sample,
                    date = dmy(data1.date),
                    x.coord = data1.coords$y.new,
                    y.coord = data1.coords$x.new)

data1$year <- year(data1$date)
data1$season <- case_when(
        month(data1$date) %in% c(3, 4, 5) ~ "spring",
        month(data1$date) %in% c(12, 1, 2) ~ "winter",
        month(data1$date) %in% c(6, 7, 8) ~ "summer",
        month(data1$date) %in% c(9, 10, 11) ~ "autumn"
)


data2 <- data[7:nrow(data), ]
names(data2) <- append("taxon", data1$sample)
data2 <- 
        data2 |> 
        pivot_longer(cols = !taxon,
                     names_to = "sample", 
                     values_to = "abundance"
        ) |> 
        filter(!is.na(abundance))


data3 <- left_join(x = data2, 
                   y = data1,
                   by = "sample")

data3$taxon %<>% str_remove("\\ sp\\.")
data3$taxon %<>% str_remove("\\ Gen\\.")
data3$taxon %<>% str_remove("\\ Ad\\.")
data3$taxon %<>% str_remove("\\ Lv\\.")
data3$taxon %<>% str_remove("\\ L\\.")
data3$taxon %<>% str_remove("\\ s\\.\\ lat\\.")
data3$taxon %<>% str_remove("-Gr\\.")
data3$taxon %<>% str_remove("\\ Gr\\.")
data3$taxon %<>% str_remove("\\ agg\\.")

setDT(data3)

data3[taxon == "Tvetenia bavarica/calvescens", taxon := "Tvetenia"]
data3 <- data3[!is.na(taxon)]

TU <- sort(unique(data3$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

data2 <- data3

#- save to or load from file 
saveRDS(data2, paste0("data/original data/Salamander/auxilliary/01_",Sys.Date(),"_data2.rds"))
data2      <- readRDS("data/original data/Salamander/auxilliary/01_2021-08-24_data2.rds")

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
#- error in Nais sp. 
#- identify elements that need to be entered manually 
taxontable[clean == FALSE]
#- manual fixes 

#- save to or load from file 
saveRDS(taxontable, paste0("data/original data/",Sys.Date(),"_taxontable.rds"))
taxontable <- readRDS("data/original data/taxontable.rds")

# COMBINE DATA SETS -----------------------------------------------------------------
setDT(data2)
names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

unique(data3$kingdom)
unique(data3$phylum)   |> sort()
unique(data3$class)    |> sort()
unique(data3$subclass) |> sort()
unique(data3$order) |> sort()
unique(data3$family) |> sort()

#- add site and date ids 
data3[, site_id := .GRP, by = "site"]
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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_biodrought")]



data4 <- data3[, list(
        gr_sample_id,
        original_site_name = site,
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
        abundance = as.numeric(abundance),
        x.coord,
        y.coord,
        EPSG = 4326,
        data.set = "salamander"
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

#- save to or load from file 
saveRDS(data4, paste0("data/original data/Salamander/auxilliary/02_",Sys.Date(),"_data4.rds"))
data4 <- readRDS("data/original data/Salamander/auxilliary/02_2021-08-24_data4.rds")


# SITES -----------------------------------------------------------------------------

#- extract individual sites and turn into spatial object 
sites <- unique(data4, by = "site_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

#- save sites to file 
saveRDS (sites, paste0("data/original data/Salamander/auxilliary/03_",Sys.Date(),"_sites.rds"))
sites <- readRDS("data/original data/Salamander/auxilliary/03_2021-08-24_sites.rds")

#   - - - - - - - - - - - - - - - - - - - - - - -#
#   - - -DISTANCE TO BRT   - - -#
#   - - - - - - - - - - - - - - - - - - - - - - -#
sites <- sites[, c("site_id", "geometry")]

sites  <- st_transform(sites, crs = st_crs(brt))
nn     <- st_nearest_feature(sites, brt)
brt_nn <- brt[nn,]

distance_list <-
        map(.x = 1:nrow(sites),
            .f = ~ as.numeric(st_distance(x = sites[.x, ],
                                          y = brt_nn[.x, ])))
beepr::beep()
#- save to or load from file
saveRDS(distance_list, paste0("data/original data/Salamander/auxilliary/04_",Sys.Date(),"_distance_to_brt.rds"))
distance_list  <- readRDS("data/original data/Salamander/auxilliary/04_2021-08-24_distance_to_brt.rds")

#- create distance table 
distance_table <- data.table(
        "site_id" = sites$site_id,
        "brt_distance" = unlist(distance_list),
        "brt20"    = brt_nn$m_btype20c,
        "brt12"    = brt_nn$m_btype12
)

sites2 <- distance_table[sites, on = "site_id"]

#   - - - - - - - - - - - - - -#
#   - - -Illies   - - -#
#   - - - - - - - - - - - - - -#
sites3 <- 
        sites2 |> 
        st_as_sf() |> 
        st_transform(crs = st_crs(illies)) |> 
        st_join(select(illies, NAME)) 

#- Distance to the closest border of an Illies Freshwater ecoregion
sites3$illies_distance <- sapply(1:nrow(sites), distance.to.illies, sites3)
beepr::beep()
#   - - - - - - - - - - -#
#   - - -BGR   - - -#
#   - - - - - - - - - - -#
sites4 <- 
        sites3 |>
        st_transform(crs = st_crs(bgr)) |>
        st_join(select(bgr, code)) 

#- Distance to the closest border of an Biogeographical Region
sites4$bgr_distance <- sapply(1:nrow(sites), distance.to.bgr, sites4)
beepr::beep()
#   - - - - - - - - - - - - - -#
#   - - -FEC      - - -#
#   - - - - - - - - - - - - - -#
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
sites6[, brt12_bgr := paste0(brt12, "_", bgr)]

#- join sites with data 
data5 <- sites6[data4, on = "site_id"]

#- save to or load from file 
saveRDS(sites6, paste0("data/original data/Salamander/auxilliary/06_", Sys.Date(), "_sites2.rds"))
saveRDS(data5,  paste0("data/original data/Salamander/auxilliary/07_", Sys.Date(), "_data5.rds"))

sites6 <- readRDS("data/original data/Salamander/auxilliary/06_2021-08-24_sites2.rds")
data5  <- readRDS("data/original data/Salamander/auxilliary/07_2021-08-24_data5.rds")

# SUMMARY STATISTICS ----------------------------------------------------------------

summary(data5$year)
sites6 |> nrow()
sites6 |> filter(brt_distance <= 500) |> nrow()
sites6 |> filter(fec.least.impacted) |> nrow()
data5  |> filter(year > 2004) |> unique(by = "site_id") |> nrow()
data5  |> filter(year > 2004) |> unique(by = "site_id") |> filter(brt_distance <= 500) |> nrow()
data5  |> filter(year > 2004) |> unique(by = "site_id") |> filter(fec.least.impacted) |> nrow()
sites6 |> filter(brt_distance <= 500 & brt_distance) |> nrow()
data5  |> filter(year > 2004) |> unique(by = "site_id") |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> nrow()

data5[, uniqueN(gr_sample_id)]
data5[brt_distance <= 500, uniqueN(gr_sample_id)]
data5[fec.least.impacted == TRUE, uniqueN(gr_sample_id)]
data5[year >= 2005, uniqueN(gr_sample_id)]
data5[brt_distance <= 500 & year >= 2005, uniqueN(gr_sample_id)]
data5[year >=2005 & fec.least.impacted == TRUE, uniqueN(gr_sample_id)]
data5[brt_distance <= 500 & fec.least.impacted == TRUE, uniqueN(gr_sample_id)]
data5[brt_distance <= 500 & fec.least.impacted == TRUE & year >= 2005, uniqueN(gr_sample_id)]


# TEMPORAL AGGREGATION --------------------------------------------------------------

data5[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
data5[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]

unique(data5$sampling.events)
table(data5$richness)

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data5, paste0("data/original data/Salamander/",Sys.Date(),"_final.rds"))       
