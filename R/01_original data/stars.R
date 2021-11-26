# ——————————————————————————— #
# ——— Clean star MZB data ——— #
# ——————————————————————————— #
# date created: 14.07.21
# date last modified: 22-09-21
# Project: Evaluating European Broad River Types for Macroinvertebrates
# Purpose: In this script I create a harmonized spatial data set from the raw data 
# provided from STAR by Christian Feld.  
# The STAR Project took place 2002 and 2003. I assign 2002 to all samples. 
# Temporal aggregation: No 
# ———————————————————————————————————————————————————————— #


# setup -----------------------------------------------------------------------------
pacman::p_load(
        here,
        taxize,
        biotic,
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

most_recent_date <- jjmisc::most_recent_date(folder = "data/01_original_data", file = "taxatable.rds")

samples_mountains <- read_excel("data/01_original_data/star/raw_data/STAR_Invertebrates.xls", sheet = 1) 
samples_lowland   <- read_excel("data/01_original_data/star/raw_data/STAR_Invertebrates.xls", sheet = 2, skip = 1) 
sites             <- read_excel("data/01_original_data/star/raw_data/STAR_Metadata_all_Sites.xls", skip = 3) 

taxontable <- readRDS(paste0("data/01_original_data/",most_recent_date,"_taxontable.rds"))
typologies <- readRDS("data/all_typologies.rds")


# prepare data ----------------------------------------------------------

#- transform to data.table 
setDT(sites)
setDT(samples_mountains)
setDT(samples_lowland)

## --  First of: sites. Without this one I cannot finish the other two. 
## -- remove diatom date columns 
sites[,(11:13) := NULL]
## - rename season columns 
names(sites)[7:10] <- c("spring", "summer", "autumn", "winter")
sites[, samplings := as.numeric(!(is.na(spring))) + 
              as.numeric(!(is.na(summer))) +
              as.numeric(!(is.na(autumn))) + 
              as.numeric(!(is.na(winter))) ]     
sites2 = sites[, list(
        site = Site_Number,
        spring,
        summer,
        autumn,
        winter,
        samplings,
        pre_pristine = `pre-classification`
)]

coord  = data.table(site = numeric(1000), 
                    x.coord = numeric(1000), 
                    y.coord = numeric(1000))
# add coordinates to sites 
for (i in c("data/01_original_data/star/raw_data/STAR_Lowlands_Abiotics", 
            "data/01_original_data/star/raw_data/STAR_Mountain_Abiotics", 
            "data/01_original_data/star/raw_data/STAR_Northern_Abiotics")) {
        
        
        file = paste0(i, ".xls")
        sheets = ifelse(i == "data/01_original_data/star/raw-data/STAR_Lowlands_Abiotics", 3, 
                        ifelse(i == "data/01_original_data/star/raw_data/STAR_Mountain_Abiotics", c(3,4), 2))
        
        for (k in sheets) {
                
                data = read_excel(file, sheet = k)
                start.row = min(which(coord$site == 0))  
                for (j in 3:ncol(data)) {
                        
                        
                        coord[start.row + j - 3, site := data[1,j]]
                        coord[start.row + j - 3, x.coord := data[18,j]]
                        coord[start.row + j - 3, y.coord := data[19,j]]
                        
                        
                }
                
        }
        
}


coords2 = unique(coord, by = "site")

## -- visual check show that this worked for most sites. But there are problems,
## -- most likely wrong decimals @ the following stations: 
## -- 683, 680, 675, 681. 

## -- lets have a look --> as expected, no commas. I will divide by 100 
coords2[site %in% c(683, 680, 675, 681)]
coords2[site %in% c(683, 680, 675, 681), c("x.coord", "y.coord") := list(x.coord/100, y.coord/100)]
## -- nope now we are in Africa 
## -- Ok this did not work. Lets see what plausible coordinates would be. 
## -- They are completely different ... I will have to set them manually. 

coords2[site == 674, c("x.coord", "y.coord") := list(-2.888444, 52.381850)]
coords2[site == 675, c("x.coord", "y.coord") := list(-3.206763, 52.039439)]
coords2[site == 676, c("x.coord", "y.coord") := list(-2.913480, 52.482101)]
coords2[site == 677, c("x.coord", "y.coord") := list(-2.703280, 51.814505)]
coords2[site == 678, c("x.coord", "y.coord") := list(-3.581886, 51.502358)]
coords2[site == 679, c("x.coord", "y.coord") := list(-2.828132, 52.430387)]
coords2[site == 680, c("x.coord", "y.coord") := list(-3.195477, 51.586230)]
coords2[site == 681, c("x.coord", "y.coord") := list(-3.186786, 51.626170)]
coords2[site == 682, c("x.coord", "y.coord") := list(-2.218578, 53.344198)]
coords2[site == 683, c("x.coord", "y.coord") := list(-1.832904, 52.466588)]
coords2[site == 892, c("x.coord", "y.coord") := list(-2.149270, 53.419834)]
coords2[site == 893, c("x.coord", "y.coord") := list(-2.643953, 53.743315)]

# Now: Mountains. 
# The data for mountians does not include sampling codes. I will only be able to
# obtain dates for locations that were only sampled once. Most were sampled
# several times.

samples_mountains2 = melt.data.table(samples_mountains, 
                                     id.vars = "TAXON_NAME", 
                                     measure.vars = 3:88,
                                     variable.name = "site",
                                     value.name = "abundance")

samples_mountains2 = samples_mountains2[!(is.na(TAXON_NAME)) & abundance != 0]
# turn factor to numeric for  join 
samples_mountains2$site = as.numeric(as.character(samples_mountains2$site))

data_mountains = samples_mountains2[sites2, 
                                    on = "site"]
data_mountains = data_mountains[!(is.na(TAXON_NAME))]

# now I can add the seasons for all locations with only one sampling event. 
data_mountains[samplings == 1, season := autumn]

data_mountains = data_mountains[,list(
        site,
        season,
        taxon = TAXON_NAME,
        abundance,
        pre_pristine
)]

# add rownames as row 
samples_lowland2 = melt.data.table(samples_lowland,
                                   id.vars = "TAXON_NAME", 
                                   measure.vars = 3:75,
                                   variable.name = "sampling_code",
                                   value.name = "abundance")
# remove zero abundance and missing taxon names 
samples_lowland2 = samples_lowland2[!(is.na(TAXON_NAME)) & abundance != 0]

for (i in 1:nrow(samples_lowland2)) {
        
        extended_code = paste(samples_lowland2$sampling_code[i], "ISM0 0CM0", sep = "-")
        # extended_code = "D0400681-ISM0 0CM0"
        
        
        # season_var = ifelse( extended_code %in% sites2$spring, "spring",
        #              ifelse( extended_code %in% sites2$summer, "summer",
        #              ifelse( extended_code %in% sites2$autumn, "autumn", 
        #              ifelse( extended_code %in% sites2$autumn, "winter", "failure"))))       
        
        season_var = ifelse(str_detect(sites2$spring, extended_code), "spring",
                            ifelse( str_detect(sites2$summer, extended_code), "summer",
                                    ifelse( str_detect(sites2$autumn, extended_code), "autumn", 
                                            ifelse( str_detect(sites2$winter, extended_code), "winter", "failure"))))     
        
        season_var %<>% na.omit %>% .[1]
        
        if (is.na(season_var)) next()
        
        samples_lowland2$season[i] <- season_var
        
        site = sites2$site[which(grepl(extended_code, sites2[[season_var]]))]
        
        samples_lowland2$site[i] <- site
}

samples_lowlands = samples_lowland2[,list(
        site, 
        season,
        taxon = TAXON_NAME,
        abundance
)]

data_lowlands = left_join(samples_lowlands,
                          sites2,
                          by = "site") %>% 
        select(site, season,
               taxon, abundance, pre_pristine) %>% 
        setDT



#- combine both 
data2 <- rbindlist(list(data_lowlands, data_mountains))
data2  <- coords2[data2, on = "site"]

#- Subset to least impacted sites 
#data2 <- data2[pre_pristine %in% c("high", "good")]
data2 <- data2[!is.na(x.coord) & !is.na(y.coord)]
data2[, original_site_name := site]
data2[, c("site", "pre_pristine") := NULL]
data2[, EPSG := 4326]

#- taxonomy 
data2[, taxon := str_remove_all(taxon, "\\ sp\\.$")] 
data2[, taxon := str_remove_all(taxon, "\\ Gen\\.$")] 
data2[, taxon := str_remove_all(taxon, "\\ Lv\\.$")] 
data2[, taxon := str_remove_all(taxon, "\\ Ad\\.$")] 
data2[, taxon := str_remove_all(taxon, "\\ s\\.\\ str\\.$")] 
data2[, taxon := str_remove_all(taxon, "\\ ssp\\.$")] 
data2[, taxon := str_remove_all(taxon, "\\ sp\\.$")] 
data2[, taxon := str_remove_all(taxon, "\\-Gr\\.$")] 
data2[, taxon := str_remove_all(taxon, "\\ COP$")] 

data2[taxon == "Baetis alpinus/lutheri/melanonyx/vardarensis", taxon := "Baetis"]
data2[taxon == "Baetis fuscatus/scambus", taxon := "Baetis"]
data2[taxon == "Caenis horaria/robusta", taxon := "Caenis"]
data2[taxon == "Caenis luctuosa/macrura", taxon := "Caenis"]
data2[taxon == "Chaetopterygini/Stenophylacini", taxon := "Limnephilidae"]
data2[taxon == "Glossosoma boltoni/conformis", taxon := "Glossosoma"]
data2[taxon == "Halesus digitatus/tesselatus", taxon := "Halesus"]
data2[taxon == "Liponeura cinerascens cinerascens", taxon := "Liponeura cinerascens"]
data2[taxon == "Mystacides longicornis/nigra", taxon := "Mystacides"]
data2[taxon == "Naididae/Tubificidae", taxon := "Naididae"]
data2[taxon == "Nebrioporus depressus/elegans", taxon := "Nebrioporus"]
data2[taxon == "Onychogomphus forcipatus forcipatus", taxon := "Onychogomphus forcipatus"]
data2[taxon == "Oulimnius troglodytes/tuberculatus", taxon := "Oulimnius"]
data2[taxon == "Perla marginata/pallida", taxon := "Perla"]
data2[taxon == "Philopotamus montanus montanus", taxon := "Philopotamus montanus"]
data2[taxon == "Pisidium casertanum casertanum", taxon := "Pisidium casertanum"]
data2[taxon == "Plectrocnemia geniculata geniculata", taxon := "Plectrocnemia geniculata"]
data2[taxon == "Potamophylax cingulatus/latipennis/luctuosus", taxon := "Potamophylax"]
data2[taxon == "Potamophylax cingulatus cingulatus", taxon := "Potamophylax cingulatus"]
data2[taxon == "Radix balthica/labiata", taxon := "Radix"]
data2[taxon == "Rhyacophila dorsalis/nubila", taxon := "Rhyacophila"]
data2[taxon == "Rhyacophila dorsalis dorsalis", taxon := "Rhyacophila dorsalis"]

TU <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
(TU <- TU[new_tu])

# TAXONOMY --------------------------------------------------------------------------
taxontable <- update_taxonomy(TU)

# COMBINE TAXA AND DATA -------------------------------------------------------------

names(data2)[which(names(data2) == "taxon")] <- "original_name"
data3 <- taxontable[data2, on = "original_name"]

#- add site and date ids 
data3[, site_id := .GRP, by = "original_site_name"]
data3[, date_id := .GRP, by = "season"]

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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_star")]

data4 <- data3[, list(
        gr_sample_id,
        original_site_name,
        date = as.Date(NA),
        year = 2002,
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
        data.set = "star"
        
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
saveRDS(data4, paste0("data/01_original_data/star/auxilliary/01_",Sys.Date(),"_data_before_typologies.rds"))

# TYPOLOGIES -----------------------------------------------------------------------------

data5 <- add_typologies(data4)
saveRDS(data5, paste0("data/01_original_data/star/",Sys.Date(),"_final_non_aggregated.rds")) 

# SUMMARY STATISTICS ----------------------------------------------------------------
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

sites6 |> filter(brt_distance <= 500) |> count()
sites6 |> filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(fec.least.impacted) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |>  filter(brt_distance <= 500) |> count()
data5 |> filter(year>2004) |> unique(by = "site_id") |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()
sites6 |> filter(fec.least.impacted) |> filter(brt_distance <= 500) |> count()        

        