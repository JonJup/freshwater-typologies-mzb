#  -------------------------------  # 
#  ----- Clean SALAMANDER data ---  # 
#  -------------------------------  #


# ----------------------------
# date created: 24.08.21
# date last modified: 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the SALAMANDER
# data from the Czech Republic
# Temporal aggregation: No 
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

# LOAD DATA  ------------------------------------------------------------------------

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxatable.rds")

data       <- read_excel("data/01_original_data/Salamander/raw_data/Data source CZ for Jonathan_River Typology_fin1.xlsx", sheet = 2) 
taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")

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
# TAXONOMY --------------------------------------------------------------------------

taxontable <- update_taxonomy(TU)

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
saveRDS(data4, paste0("data/01_original_data/Salamander/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))
# TYPOLOGIES -----------------------------------------------------------------------------
data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/Salamander/",Sys.Date(),"_final_non_aggregated.rds"))
# TEMPORAL AGGREGATION --------------------------------------------------------------
#--> not necessary 
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

