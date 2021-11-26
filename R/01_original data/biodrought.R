#  -------------------------------  # 
#  ----- Clean BIODROUGHT data ---  # 
#  -------------------------------  #


# ----------------------------
# date created: 24-08-21
# date last modified: 05-10-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the BIODROUGHT data from the Czech Republic provided by Petr Paril.
# Temporal aggregation: Yes 
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
source("R/get_last_date.r")

# LOAD DATA  ------------------------------------------------------------------------

data       <- read_excel("data/01_original_data/biodrought/raw_data/Data source CZ for Jonathan_River Typology_corrected2.xlsx", sheet = 3) 
coords     <- read_excel("data/01_original_data/biodrought/raw_data/Data source CZ for Jonathan_River Typology_corrected2.xlsx", sheet = 2)
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

# PREPARE DATA   ---------------------------------------------------------------------
#  — — — extract site data 
data1 <- data[1:5, -c(1:2)]
# - site names 
data1.site <- data1[2,] |> unlist() |> unname()
# - sample names 
data1.sample <- data1[1,] |> unlist() |> unname()
# - there is a small mistake in the sample names (see Email Petr Paril 24.08.2021)
data1.sample[6:11] %<>% str_replace("CZ_1", "CZ_2")
# - dates add manually from excel 
data1.date <- c("17.10.2012",	"10.04.2013",	"16.10.2013",	"02.04.2014",	"27.09.2014",	"25.04.2012",	"24.10.2012",	"12.04.2013",	"08.10.2013",	"28.03.2014",	"27.09.2014",	"11.10.2012",	"19.04.2013",	"15.10.2013",	"29.03.2014",	"28.09.2014",	"03.05.2012",	"11.10.2012",	"18.04.2013",	"15.10.2013",	"29.03.2014",	"28.09.2014",	"17.05.2012",	"23.10.2012",	"18.04.2013",	"11.10.2013",	"06.04.2014",	"02.05.2012",	"17.10.2012",	"11.04.2013",	"17.10.2013",	"01.04.2014",	"27.09.2014",	"11.10.2012",	"18.04.2013",	"15.10.2013",	"29.03.2014",	"28.09.2014",	"12.04.2013",	"08.10.2013",	"28.03.2014",	"27.09.2014",	"25.04.2012",	"24.10.2012",	"12.04.2013",	"08.10.2013",	"28.03.2014",	"27.09.2014",	"16.05.2012",	"24.10.2012",	"18.04.2013",	"11.10.2013",	"05.04.2014",	"16.10.2012",	"17.04.2013",	"10.10.2013",	"08.04.2014",	"02.10.2014",	"26.10.2012",	"25.04.2013",	"07.10.2013",	"30.03.2014",	"29.09.2014",	"23.05.2012",	"11.04.2013",	"18.10.2013",	"02.04.2014",	"28.09.2014",	"14.05.2012",	"17.10.2012",	"29.04.2013",	"07.10.2013",	"30.03.2014",	"29.09.2014",	"26.10.2012",	"29.04.2013",	"23.10.2013",	"30.03.2014",	"29.09.2014",	"19.04.2013",	"15.10.2013",	"29.03.2014",	"28.09.2014",	"18.10.2012",	"11.04.2013",	"17.10.2013",	"01.04.2014",	"27.09.2014",	"16.05.2012",	"23.10.2012",	"18.04.2013",	"11.10.2013",	"06.04.2014",	"16.05.2012",	"24.10.2012",	"19.04.2013",	"11.10.2013",	"05.04.2014",	"11.05.2012",	"26.10.2012",	"25.04.2013",	"07.10.2013",	"30.03.2014",	"29.09.2014",	"15.05.2012",	"17.10.2012",	"10.04.2013",	"16.10.2013",	"01.04.2014",	"27.09.2014",	"16.10.2012",	"17.04.2013",	"10.10.2013",	"08.04.2014",	"02.10.2014",	"18.10.2012",	"11.04.2013",	"17.10.2013",	"02.04.2014",	"28.09.2014")

# — — — coordinates 
# - site name from coordinate table. 
coord.site  <- coords[1,-1] |> unlist() |> unname()
# - raw coordinates 
coord.coord <- coords[4,-1] |> unlist() |> unname()
# - decompose into constituent parts 
data.coord <- data.table(site = coord.site, 
                         original = coord.coord)

data.coord[, x.original := str_extract(string = original, pattern = ".*,")      |> str_remove(pattern = "N") |> str_remove(",")             |> str_trim()]
data.coord[, y.original := str_extract(string = original, pattern = ",.*")      |> str_remove(pattern = "E") |> str_remove(",")             |> str_trim()]                       
data.coord[, x.degree   := str_extract(string = x.original, pattern = ".*°")    |> str_remove(pattern = "°")                                |>  str_trim()]
data.coord[, y.degree   := str_extract(string = y.original, pattern = ".*°")    |> str_remove(pattern = "°")                                |>  str_trim()]
data.coord[, x.minutes  := str_extract(string = x.original, pattern = "°.*\\'") |> str_remove(pattern = "°") |> str_remove(pattern = "\\'") |>  str_trim()]
data.coord[, y.minutes  := str_extract(string = y.original, pattern = "°.*\\'") |> str_remove(pattern = "°") |> str_remove(pattern = "\\'") |>  str_trim()]
data.coord[, x.seconds  := str_extract(string = x.original, pattern = "'.*\"") |> str_remove(pattern = "'") |> str_remove(pattern = "\\.") |>  str_trim()]
data.coord[, y.seconds  := str_extract(string = y.original, pattern = "'.*\"") |> str_remove(pattern = "'") |> str_remove(pattern = "\\.") |>  str_trim()]
data.coord[is.na(x.seconds), x.seconds := 0]
data.coord[is.na(y.seconds), y.seconds := 0]

# - check 
unique(data.coord$x.minutes)
unique(data.coord$y.minutes)
unique(data.coord$x.seconds)
# - reassemble 
#- put together new coordinates 
data.coord[, x.new := paste0(x.degree, "°", x.minutes, "!", x.seconds, "§", "E")]
data.coord[, y.new := paste0(y.degree, "°", y.minutes, "!", y.seconds, "§", "N")]
data.coord[, x.new := as.numeric(char2dms(data.coord$x.new, chd = "°", chm = "!", chs = "§"))]
data.coord[, y.new := as.numeric(char2dms(data.coord$y.new, chd = "°", chm = "!", chs = "§"))]

#- check on map 
site <-
        data.coord |> 
        st_as_sf(coords = c("y.new", "x.new"), crs = 4326) 
mapview(site)

#- save in new table 
data1 <- data.table(site = data1.site, 
                    sample = data1.sample,
                    date = dmy(data1.date),
                    EPSG = 4326, 
                    data.set = "biodrought")
#- reformat sites and join to data
data.coord2 <- data.coord[,c("site","x.new", "y.new")]
names(data.coord2) <- c("site", "y.coord", "x.coord")
data1        <- data.coord2[data1, on = "site"]
#- add year and season
data1$year   <- year(data1$date)
data1$season <- case_when(
        month(data1$date) %in% c(3, 4, 5) ~ "spring",
        month(data1$date) %in% c(12, 1, 2) ~ "winter",
        month(data1$date) %in% c(6, 7, 8) ~ "summer",
        month(data1$date) %in% c(9, 10, 11) ~ "autumn"
)
#  — — — extract species data 
data2 <- # subset to species data 
        data[7:nrow(data), -c(1)] |> 
        # remove NA entries 
        filter(state != "NA")
#- rename columns 
names(data2) <- append("taxon", data1$sample)
# reshape data 
data2 <- 
        data2 |> 
        pivot_longer(cols = !taxon,
                     names_to = "sample", 
                     values_to = "abundance"
                 ) |> 
        filter(!is.na(abundance))
#- check abundance values 
data2$abundance |> table()

#- join site and species data 
data3 <- left_join(x = data2,
                   y = data1,
                   by = "sample")

#- clean taxon names
remove <- c("\\ sp\\.", "\\ Gen\\.", "\\ Ad\\.", "\\ Lv\\.", "\\ L\\.", "\\ s\\.\\ lat\\.", "-Gr\\.", "\\ Gr\\.", "\\ agg\\.")
data3$taxon %<>% str_remove_all(paste(remove, collapse = "|"))

setDT(data3)
#- replace ambiguous taxa with higher level taxas  
data3[taxon == "Clinocera/Wiedemannia", taxon := "Empididae"]
data3[taxon == "Cricotopus patens/flavocinctus", taxon := "Cricotopus"]
data3[taxon == "Eukiefferiella clypeata/pseudomontana", taxon := "Eukiefferiella"]
data3[taxon == "Henlea ventriculosa/nasuta", taxon := "Henlea"]
data3[taxon == "Henlea/Fridericia", taxon := "Enchytraeidae"]
data3[taxon == "Heterotrissocladius grimshawi/scutellatus", taxon := "Heterotrissocladius"]
data3[taxon == "Hydracarina, Acari, Acarina", taxon := "Trombidiformes"]
data3[taxon == "Hydroporinae, Dytiscidae", taxon := "Dytiscidae"]
data3[taxon == "Nanocladius parvulus/rectinervis", taxon := "Nanocladius"]
data3[taxon == "Tvetenia bavarica/calvescens", taxon := "Tvetenia"]
data3[taxon == "Trichodrilus allobrogum/moravicus", taxon := "Trichodrilus"]
data3[taxon == "Rhyacophila fasciata/obliterata/pascoei/vulgaris", taxon := "Rhyacophila"]
data3[taxon == "Rhithrogena iridina/picteti", taxon := "Rhithrogena"]

#- remove non-macorinvertebrate entries 
data3 <- data3[!taxon %in% c("Salamandra salamandra", "Pisces")]
data3 <- data3[!is.na(taxon)]

TU <- sort(unique(data3$taxon))
#- which ones need to be checked 
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])
#- rename file 
data2 <- data3

rm(remove, data3, new_tu)

# TAXONOMY --------------------------------------------------------------------------

taxontable <- jjmisc::update_taxonomy(TU)

# COMBINE DATA SETS -----------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

unique(data3$kingdom)
unique(data3$phylum)   |> sort()
unique(data3$class)    |> sort()
unique(data3$subclass) |> sort()
unique(data3$order)    |> sort()
unique(data3$family)   |> sort()

data3[order == "Tricladia", order := "Tricladida"]

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
        data.set = "biodrought"
)]

data4$abundance |> table()

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
#- check abundance 
data4$abundance |> table()

#- save to or load from file 
saveRDS(data4, paste0("data/01_original_data/biodrought/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))
data4 <- readRDS("data/01_original_data/biodrought/auxilliary/01_2021-09-21_data_before_typologies.rds")
# TYPOLOGIES -----------------------------------------------------------------------------

data5 <- jjmisc::add_typologies(data4)
#- save to or load from file 
saveRDS(data5, paste0("data/01_original_data/biodrought/", Sys.Date(), "_final_non_aggregated.rds"))
data5 <- readRDS("data/01_original_data/biodrought/2021-10-05_final_non_aggregated.rds")
# TEMPORAL AGGREGATION --------------------------------------------------------------
source("R/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/01_original_data/biodrought/",Sys.Date(),"_final_aggregated.rds"))       

# Statistics ------------------------------------------------------------------------
uniqueN(data6$site_id)
uniqueN(data5$gr_sample_id)

summary(data5$year)


data6 |> filter(distance <= 500)    |> distinct(gr_sample_id) |> count()
data6 |> filter(least.impacted) |> distinct(gr_sample_id) |> count()
data6 |> filter(least.impacted & distance <= 500) |> distinct(gr_sample_id) |> count()


# aggregated 
uniqueN(data6$gr_sample_id)
data6[distance <= 500, uniqueN(gr_sample_id)]
data6[least.impacted == TRUE, uniqueN(gr_sample_id)]
data6[least.impacted == TRUE & distance <= 500, uniqueN(gr_sample_id)]

