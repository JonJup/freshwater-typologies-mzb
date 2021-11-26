pacman::p_load(sf, data.table, dplyr, magrittr, stringr, readxl, tidyr, lubridate)

#  —————— REMARK  ———————— #
# Both data sets MMI Toruõ 2015.xls and MMI Toruõ 2016.xls do not have any sites for which I have coordinates.
# FWD.../ formatka MMI_MinskMaz_2014_PL.xls  ——> no sites with coordinates 
# FWD.../formatka MMI_PL2013.xls             ——> no data 
# FWD.../Ostroleka_2015_formatka MMI_PL.xls  ——> no sites with coordinates

all.data <- list()

# 01.data: mzb/2016.xlsx -----------------------------------------
work <- read_excel("data/original data/monitoring_poland/raw_data/MZB_PL/mzb/2016.xlsx")
date.id <- which(names(work) == "DATA POBORU\r\n PRÓBKI")
abun.id <- which(names(work) == "LICZBA \r\nOSOBNIKÓW")

work <- data.table(original_site_name   = work$`KOD PPK`,
                   sample_name          = work$`NAZWA PPK`,
                   y.coord            = work$`DŁ. GEOGR PPK`,
                   x.coord            = work$`SZER. GEOGR PPK`, 
                   taxon              = work$`TAKSON NAZWA`,
                   date               = ymd_hm(pull(work[,date.id])),
                   EPSG               = 4326,
                   abundance          = pull(work[,abun.id]),
                   data.set = "raw1"
)

work[,c("year", "month") := .(year(date), month(date))]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]

all.data[[1]] <- work
rm(work)
# 02.data: mzb/2017.xlsx ------------------------------------------------------------
work <- read_excel("data/original data/monitoring_poland/raw_data/MZB_PL/mzb/2017.xlsx")

work <- data.table(original_site_name        = work$`KOD PPK`,
                   sample_name        = work$`NAZWA PPK`,
                   y.coord            = work$`DŁ. GEOGR PPK`,
                   x.coord            = work$`SZER. GEOGR PPK`, 
                   taxon              = work$`TAKSON NAZWA`,
                   date               = ymd_hm(pull(work[,date.id])),
                   EPSG               = 4326,
                   abundance          = pull(work[,abun.id]),
                   data.set = "raw2"
)

work <- work[!is.na(date)]

work[,c("year", "month") := .(year(date), month(date))]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]



all.data[[2]] <- work
rm(work)
# 03.data: mzb/2018.xlsx ------------------------------------------------------------
work <- read_excel("data/original data/monitoring_poland/raw_data/MZB_PL/mzb/2018.xlsx")
work <- data.table(original_site_name = work$`KOD PPK`,
                   sample_name        = work$`NAZWA PPK`,
                   y.coord            = work$`DŁ. GEOGR PPK`,
                   x.coord            = work$`SZER. GEOGR PPK`, 
                   taxon              = work$`TAKSON NAZWA`,
                   date               = ymd_hm(pull(work[,date.id])),
                   EPSG               = 4326,
                   abundance          = pull(work[,abun.id]),
                   data.set = "raw3"
)
work[,c("year", "month") := .(year(date), month(date))]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]

all.data[[3]] <- work
rm(work)
# 04.data: mzb/2019.xlsx ------------------------------------------------------------
work <- read_excel("data/original data/monitoring_poland/raw_data/MZB_PL/mzb/2019.xlsx")
work <- data.table(original_site_name = work$`KOD PPK`,
                   sample_name        = work$`NAZWA PPK`,
                   y.coord            = work$`DŁ. GEOGR PPK`,
                   x.coord            = work$`SZER. GEOGR PPK`, 
                   taxon              = work$`TAKSON NAZWA`,
                   date               = ymd_hm(pull(work[,date.id])),
                   EPSG               = 4326,
                   abundance          = pull(work[,abun.id]),
                   data.set = "raw4"
)
work[,date := round_date(date, "day")]
work <- work[!is.na(abundance)]
work[,c("year", "month") := .(year(date), month(date))]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
all.data[[4]] <- work
rm(work)
rm(abun.id, date.id)

# collect coordinates ---------------------------------------------------------------

coords.obj <- 
        rbindlist(all.data) |>  
        unique(by = "original_site_name") |> 
        select(c("original_site_name", "x.coord", "y.coord", "sample_name"))


# 05.data: mzb/makrobentos_województwami 2009.xlsx -sheet1--------------------------------------------------------------------------

for (i in 1:15){
        
        if (i == 1) storage <- list()
        
        work <- read_excel("data/original data/monitoring_poland/raw_data/MZB_PL/mzb/makrobentos_województwami 2009.xlsx", sheet = i)
        work2 <- work[-(1:9), -c(1:2)]
        names(work2) <- append("taxon", unlist(work[6,-c(1:3)]))
        if (all(is.na(names(work2)) | names(work2) == "taxon")) {
                next()
        }
        if (any(is.na(names(work2)))) {
                i.na.name.id <- which(is.na(names(work2)))
                work2 <- work2[,-i.na.name.id]
        }
        work3 <-
                pivot_longer(
                        data = work2,
                        cols = !taxon,
                        names_to = "original_site_name",
                        values_to = "abundance"
                ) |>
                filter(!is.na(abundance)) |>
                filter(!is.na(taxon)) |>
                filter(original_site_name %in% coords.obj$original_site_name) |>
                setDT()
        i.date.var <-
                data.table(original_site_name = unlist(work[c(6),-c(1:3)]),
                           date               = unlist(work[c(4),-c(1:3)]))
        work3 <- i.date.var[work3, on = "original_site_name"]
        work3 <- coords.obj[work3, on = "original_site_name"]
        
        storage[[i]] <- work3
        rm(list = ls()[grepl("^i\\.", ls())])
}

work <- rbindlist(storage)
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw5"]
all.data[[5]] <- work
rm(work, storage, i, work2, work3)

# 06.data: mzb/makrobentos_województwami_uzupeênienia.xls--------------------------------------------------------------------------

for (i in 1:10){
        
        if (i == 1) storage <- list()
        
        i.raw.data <- read_excel("data/original data/monitoring_poland/raw_data/MZB_PL/mzb/makrobentos_województwami_uzupeênienia.xls", sheet = i)
        i.raw.data.2 <- i.raw.data[-(1:9), -c(1:2)]
        i.names <-  append("taxon", unlist(i.raw.data[6,-c(1:3)]))
        if (any(is.na(i.names))) i.names[which(is.na(i.names))] <- "unknown"
        names(i.raw.data.2) <- i.names
        
        if (sum(names(i.raw.data.2) == "unknown") == (ncol(i.raw.data.2)-1) | all(names(i.raw.data.2) == "taxon")) {
                next()
        }
        if (any(names(i.raw.data.2) == "unknown")) {
                i.na.name.id <- which(names(i.raw.data.2) == "unknown")
                i.raw.data.2 <- i.raw.data.2[,-i.na.name.id]
        }
        i.raw.data.3 <- 
                pivot_longer(data = i.raw.data.2, cols = !taxon, names_to = "original_site_name", values_to = "abundance") |> 
                filter(!is.na(abundance)) |>
                #filter(value != "x")  |> 
                filter(!is.na(taxon)) |> 
                filter(original_site_name %in% coords.obj$original_site_name) |> 
                setDT()
        i.date.var <-
                data.table(original_site_name = unlist(i.raw.data[c(6),-c(1:3)]),
                           date               = unlist(i.raw.data[c(4),-c(1:3)]))
        i.raw.data.3 <- i.date.var[i.raw.data.3, on = "original_site_name"]
        i.raw.data.3 <- coords.obj[i.raw.data.3, on = "original_site_name"]
        
        storage[[i]] <- i.raw.data.3
        rm(list = ls()[grepl("^i\\.", ls())])
}
work <- rbindlist(storage)
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw6"]
all.data[[6]] <- work
rm(work, storage, i)

# 07.data: mzb/raport20191104161227.xls -------------------------------------------------------------------------
#  ——> there are two types of formating in the sheets of this workbook. The first type is in sheets 1 to 4 and the second in 5 to 16. 
#      I need different scripts to clean the two hence they are separated in data 7 and 8 

for (i in 1:4){
        
        if (i == 1)
                storage <- list()
        
        i.raw.data <-
                read_excel(
                        "data/original data/monitoring_poland/raw_data/MZB_PL/mzb/raport20191104161227.xls",
                        sheet = i,
                        skip = 2
                )
        i.raw.data %<>%
                rename(sample_name = 'Nazwa PPK') %>%
                filter(!is.na(sample_name)) %>%
                select(!c('MS CD', 'WB LOCATION', "Nazwa JCWP", "Region CD", 'Typ cieku'))
        i.raw.data %<>%
                filter(sample_name %in% coords.obj$sample_name)
        names(i.raw.data)[str_detect(names(i.raw.data), "^Data")] <-
                "date"
        
        
        if (nrow(i.raw.data) == 0)
                next()
        
        i.raw.data2 <-
                pivot_longer(
                        data = i.raw.data,
                        cols = !c(sample_name, date),
                        names_to = "taxon",
                        values_to = "abundance"
                ) |>
                filter(!is.na(abundance)) |>
                #filter(value != "x")  |>
                filter(!is.na(taxon)) |>
                setDT()
        
        i.raw.data2[, date := ymd_hms(date)]
        
        i.raw.data3 <- coords.obj[i.raw.data2, on = "sample_name"]
        i.raw.data3[, taxon := str_remove_all(taxon, "^\\d+ :")]
        i.raw.data3[, taxon := str_trim(taxon)]
        
        storage[[i]] <- i.raw.data3
        rm(list = ls()[grepl("^i\\.", ls())])
}

work <- rbindlist(storage)
work[, EPSG := 4326]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12,1,2) ~ "winter")]
work[, data.set := "raw7"]
all.data[[7]] <- work
rm(work, storage, i)

# 08.date: mzb/raport20191104161227.xls --------------------------------------------------------------------------

for (i in 5:16){
        if (i == 5)
                storage <- list()
        i.raw.data <-
                read_excel(
                        "data/original data/monitoring_poland/raw_data/MZB_PL/mzb/raport20191104161227.xls",
                        sheet = i,
                        skip = 1,
                        n_max = 6
                )
        i.raw.data2 <- data.table(sample_name = unlist(i.raw.data[2,-c(1)]))
        i.raw.data2$date <- unlist(i.raw.data[6, -1])
        i.raw.data2[, date := ymd_hms(date)]
        i.raw.data2[is.na(sample_name), sample_name := "unknown"]
        
        #- are there duplicates in the sample names? 
        i.dup.n <- nrow(i.raw.data2[duplicated(sample_name) & sample_name != "unknown"])
        i.raw.data2[duplicated(sample_name) & sample_name != "unknown", sample_name := paste0(sample_name, "_dup", 1:i.dup.n)]
        
        i.raw.data <-
                read_excel(
                        "data/original data/monitoring_poland/raw_data/MZB_PL/mzb/raport20191104161227.xls",
                        sheet = i,
                        skip = 8
                )
        names(i.raw.data) <- append("taxon", i.raw.data2$sample_name)
        i.raw.data2 <- i.raw.data2[!is.na(sample_name)]
        i.raw.data %<>%
                select(!unknown) %>%
                pivot_longer(cols = !taxon, values_to = "abundance") %>%
                filter(!is.na(abundance)) %>%
                filter(abundance != 0) %>%
                rename(sample_name = name)
        setDT(i.raw.data)
        
        #- fix taxon names. Before this they are of type ANOSTRACA : Branchipodidae : Branchipus shaefferi
        i.raw.data[, taxon := str_remove(taxon, ".* :")]
        i.raw.data <- i.raw.data2[i.raw.data, on = "sample_name"]
        
        #- remove duplication suffix 
        i.raw.data[grepl("_dup\\d+$", sample_name),  sample_name := str_remove(sample_name, "_dup\\d+$")]
        
        i.raw.data <- i.raw.data[sample_name %in% coords.obj$sample_name]
        
        i.raw.data <- coords.obj[i.raw.data, on = "sample_name"]
        storage[[i - 4]] <- i.raw.data
}

work <- rbindlist(storage)
work[, EPSG := 4326]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12,1,2) ~ "winter")]
work[, data.set := "raw8"]
all.data[[8]] <- work
rm(work, storage,i)


# 09.data: mzb/ZaêÑcznik 2 Lista stwierdzonych taksonów.xlsx -------------------------------------------------------------------------

for (i in 1:12){
        
        #- create a list as storage object to save result of each iteration 
        if (i == 1)
                storage <- list()
        #- read in sheet i 
        i.raw.data <-
                read_excel(
                        "data/original data/monitoring_poland/raw_data/MZB_PL/mzb/ZaêÑcznik 2 Lista stwierdzonych taksonów.xlsx",
                        sheet = i
                )
        #- extract site names 
        i.original_site_name <- unlist(i.raw.data[7,-c(1:3)])
        #- extract dates 
        i.dates                 <- unlist(i.raw.data[5, -c(1:3)])
        #- combined names and dates in new data.table 
        i.raw.data2 <- data.table(original_site_name = i.original_site_name, 
                                  date                 = i.dates)
        
        #- NAs in the pivoting variable cause a warning. Therefore rename NA to unknown. 
        i.raw.data2[is.na(original_site_name), original_site_name := "unknown"]
        if (all(i.raw.data2$original_site_name == "unknown")) next()
        #- Are there duplicates in the sample names? 
        #- Count duplicates 
        i.dup.n <- nrow(i.raw.data2[duplicated(original_site_name) & original_site_name != "unknown"])
        #- append suffix to duplicates if present.
        if (i.dup.n > 0 ){
                i.raw.data2[duplicated(original_site_name) & original_site_name != "unknown", original_site_name := paste0(original_site_name, "_dup", 1:i.dup.n)]        
        }
        #- read in data again. This time for taxa. 
        i.raw.data <-
                read_excel(
                        "data/original data/monitoring_poland/raw_data/MZB_PL/mzb/ZaêÑcznik 2 Lista stwierdzonych taksonów.xlsx",
                        sheet = i,
                        skip = 11
                )
        #- remove column with higher order taxa 
        i.raw.data <- i.raw.data[,-c(1:2)]
        #- rename columns. first column is the taxon. Subsequent ones are a sampling event each. 
        names(i.raw.data) <- append("taxon", i.raw.data2$original_site_name)
        #- drop NA sites form data.raw2 
        i.raw.data2 <- i.raw.data2[original_site_name != "unknown"]
        #- drop NA sites from data.raw 
        if ("unknown" %in% names(i.raw.data)) i.raw.data %<>% select(!unknown)
        
        #- reshape data.raw 
        i.raw.data %<>%
                apply(2, as.character) %>%
                as.data.frame() %>%
                pivot_longer(cols = !taxon, 
                             values_to = "abundance",
                             names_to = "original_site_name") %>%
                filter(!is.na(abundance) & !is.na(taxon)) %>%
                filter(abundance != "0") %>%
                setDT()
        
        #- add sites and date info 
        i.raw.data <- i.raw.data2[i.raw.data, on = "original_site_name"]
        
        #- fix dates 
        #- in the first sheet there is a "r." at the end of the data sting 
        i.raw.data[str_detect(date, "r\\."), date := str_remove(date, "r\\.")]
        i.raw.data[, date := dmy(date)]
        
        
        #- remove duplication suffix 
        i.raw.data[grepl("_dup\\d+$", original_site_name),  original_site_name := str_remove(original_site_name, "_dup\\d+$")]
        #- drop sites I do not have coordinates for 
        i.raw.data <- i.raw.data[original_site_name %in% coords.obj$original_site_name]
        #- ... if this removed all data skip to next iteration
        if (nrow(i.raw.data) == 0) next()
        #- add coordinates 
        i.raw.data <- coords.obj[i.raw.data, on = "original_site_name"]
        storage[[i]] <- i.raw.data
        rm(list = ls()[grepl(pattern = "^i\\.", ls())])
        rm(i)
        gc()
}
work <- rbindlist(storage)
work[, EPSG := 4326]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw9"]

all.data[[9]] <- work
rm(work, storage)


# 10.data: MMI makrobentos\MMI makrobentos\MMI Bydgoszcz 2012.xls ------------------------------------------------------------------------
#- When you open this workbook in excel, the focal sheet is on position one. However, to read it I have to enter sheet two for some reason. 
work <-
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2012.xls",
                sheet = 2
        )
#- extract date and site varaibles 
date.var <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name, 
                    date = date.var)
#- There is fifteen sites and plenty of trailing NA rows. 
work2 <- work2[1:15,]
work <-
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2012.xls",
                sheet = 2, 
                skip = 11
        )
#- subset to sites (incliding taxon column)
work <- work[,1:16]
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(!is.na(abundance))) %>%
        filter(!is.na(abundance) & !is.na(taxon)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw10"]

all.data[[10]] <- work
rm(work, original_site_name, date.var, work2)


# 11.data: MMI makrobentos\MMI makrobentos\MMI Bydgoszcz 2013.xls --------------------------------------------------------------------------
work <-
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2013.xls",
                sheet = 2
        )
#- extract date and site varaibles 
date.var <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name, 
                    date = date.var)
#- There is eleven sites and plenty of trailing NA rows. 
work2 <- work2[1:11,]
work2[date %in% c("14.06.13", "13.05.13"), date := str_replace(date, "\\.13$", "\\.2013")]
work <-
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2013.xls",
                sheet = 2, 
                skip = 11
        )
#- subset to sites (incliding taxon column)
work <- work[,1:12]
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance))  %>%
        filter(!is.na(abundance) &  !is.na(taxon)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw11"]

all.data[[11]] <- work
rm(work, original_site_name, date.var, work2)


# 12.data: MMI makrobentos\MMI makrobentos\MMI Bydgoszcz 2014.xls -------------------------------------------------------------------------

work <-
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2014.xls",
                sheet = 2
        )
#- extract date and site varaibles 
date.var           <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name, 
                    date = date.var)
#- There is eight sites and plenty of trailing NA rows. 
work2 <- work2[1:8,]
work2[str_detect(date, "\\.14$"), date := str_replace(date, "\\.14$", "\\.2014")]
work <-
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2014.xls",
                sheet = 2, 
                skip = 11
        )
#- subset to sites (including taxon column)
work <- work[,1:9]
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw12"]

all.data[[12]] <- work
rm(work, original_site_name, date.var, work2)


# 13.data: MMI makrobentos\MMI makrobentos\MMI Bydgoszcz 2015.xls--------------------------------------------------------------------------
work <-
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2015.xls",
                sheet = 2
        )
#- extract date and site varaibles 
date.var <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
#- There is five sites and plenty of trailing NA rows.
work2 <- work2[1:5, ]
work <- 
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2015.xls",
                sheet = 2, 
                skip = 11
        )
#- subset to sites (including taxon column)
work <- work[,1:6]
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(
        month %in% c(3:5) ~ "spring",
        month %in% c(6:8) ~ "summer",
        month %in% c(9:11) ~ "autumn",
        month %in% c(12:2) ~ "winter"
)]
work[, data.set := "raw13"]

all.data[[13]] <- work
rm(work, original_site_name, date.var, work2)


# 14.data: MMI makrobentos\MMI makrobentos\MMI Bydgoszcz 2016.xls ---------------------------------------------------------------------------
work <-
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2016.xls",
                sheet = 2
        )
#- extract date and site varaibles
date.var <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
#- There is six sites and plenty of trailing NA rows.
work2 <- work2[1:6, ]
work <-
        read_excel(
                "data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/MMI Bydgoszcz 2016.xls",
                sheet = 2,
                skip = 11
        )
#- subset to sites (including taxon column)
work <- work[,1:7]
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw14"]

all.data[[14]] <- work
rm(work, original_site_name, date.var, work2)



# 15.data: MMI makrobentos\MMI makrobentos\MMI Toruõ 2012.xls -------------------------------------------------------------------------

data.set <- "MMI Toruõ 2012.xls"
work <-
        read_excel(
                paste0("data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/",data.set),
                sheet = 3
        )
#- extract date and site varaibles 
date.var           <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
#- There is four sites and plenty of trailing NA rows. 
n.sites <- 4
work2 <- work2[1:n.sites,]
work <-
        read_excel(
                paste0("data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/",data.set),
                sheet = 3,
                skip = 11
        )
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+1)]
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw15"]

all.data[[15]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set)


# 16.data: MMI makrobentos\MMI makrobentos\MMI Toruõ 2013.xls -------------------------------------------------------------------------

data.set <- "MMI Toruõ 2013.xls"
work <-
        read_excel(
                paste0("data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/",data.set),
                sheet = 3
        )
#- extract date and site varaibles 
date.var           <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
#- There is two sites and plenty of trailing NA rows. 
n.sites <- 2
work2 <- work2[1:n.sites,]
work <-
        read_excel(
                paste0("data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/",data.set),
                sheet = 3,
                skip = 11
        )
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+1)]
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw16"]

all.data[[16]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set)


# 17.data: MMI makrobentos\MMI makrobentos\MMI Toruõ 2014.xls  -------------------------------------------------------------------------
data.set <- "MMI Toruõ 2014.xls"
work <-
        read_excel(
                paste0("data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/",data.set),
                sheet = 3
        )
#- extract date and site varaibles 
date.var           <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table 
work2  <- data.table(original_site_name = original_site_name,
                     date = date.var)
#- There is six sites and plenty of trailing NA rows. 
n.sites <- 6
work2 <- work2[1:n.sites,]
work <-
        read_excel(
                paste0("data/original data/monitoring_poland/raw_data/MZB_PL/MMI makrobentos/MMI makrobentos/",data.set),
                sheet = 3,
                skip = 11
        )
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+1)]
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw17"]

all.data[[17]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set)


# 18.data: FWD.../ Ciechanów_2015_formatka MMI_PL.xls --------------------------
folder   <- "FW [Fwd Re [Fwd [Fwd FW Udost®pnienie danych makrobezkr®gowcowych]]]"
data.set <- "Ciechanów_2015_formatka MMI_PL.xls"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL//", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
#- There is two sites and plenty of trailing NA rows. 
n.sites <- 2
work2 <- work2[1:n.sites,]
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw18"]
all.data[[18]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 19.data: FWD.../ formatka MMI_Ciechanów2013.xls --------------------------
folder   <- "FW [Fwd Re [Fwd [Fwd FW Udost®pnienie danych makrobezkr®gowcowych]]]"
data.set <- "formatka MMI_Ciechanów2013.xls"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL//", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:4)])
original_site_name <- unlist(work[6, -c(1:4)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
#- There is two sites and plenty of trailing NA rows. 
n.sites <- 2
work2 <- work2[1:n.sites,]
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw19"]
all.data[[19]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)



# 20.data: FWD.../ formatka MMI_Ostroleka_2014_PL.xls --------------------------
folder   <- "FW [Fwd Re [Fwd [Fwd FW Udost®pnienie danych makrobezkr®gowcowych]]]"
data.set <- "formatka MMI_Ostroleka_2014_PL.xls"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL//", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
#- There is one site and plenty of trailing NA rows. 
n.sites <- 1
work2 <- work2[1:n.sites,]
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw20"]
all.data[[20]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 21.data: FWD.../ formatka MMI_Ostroleka2013.xls --------------------------
folder   <- "FW [Fwd Re [Fwd [Fwd FW Udost®pnienie danych makrobezkr®gowcowych]]]"
data.set <- "formatka MMI_Ostroleka2013.xls"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL//", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
work2[date == "24.09.2103", date := "24.09.2013"]
#- There is one site and plenty of trailing NA rows. 
n.sites <- 3
work2 <- work2[1:n.sites,]
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw21"]
all.data[[21]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 22.data: 2011/ wyniki MMI 2011r  zweryfikowane- Warszawa.xls --------------------------
folder   <- "2011"
data.set <- "wyniki MMI 2011r  zweryfikowane- Warszawa.xls"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL//", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
#- There is one site and plenty of trailing NA rows. 
n.sites <- 11
work2 <- work2[1:n.sites,]
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw22"]
all.data[[22]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 23.data: 2011/ formatka Nowy Sacz 2012 MMI_PL_wer2 wyniki_Radom_ 2011.xls --------------------------
folder   <- "2011"
data.set <- "formatka Nowy Sacz 2012 MMI_PL_wer2 wyniki_Radom_ 2011.xls"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL//", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
#- There is one site and plenty of trailing NA rows. 
n.sites <- 7
work2 <- work2[1:n.sites,]
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := str_remove(date, "r\\.$")]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw23"]
all.data[[23]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 24.data: 2011/Wyniki MMI2011r Ciechanów/formatka 2011C_PL_wer2.xlsx --------------------------
folder   <- "2011/Wyniki MMI2011r Ciechanów"
data.set <- "formatka 2011C_PL_wer2.xlsx"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL//", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
work2
work2 <- work2[!is.na(date)]
n.sites <- nrow(work2)
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := str_remove(date, "r\\.$")]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw24"]
all.data[[24]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 25.data: Wyniki MMI2011r Ciechanów/formatka MinskM_2011_PL_wer2.xlsx --------------------------------------------------------------------------------
folder   <- "2011/Wyniki MMI2011r Ciechanów"
data.set <- "formatka MinskM_2011_PL_wer2.xlsx"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL/", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
work2 <- work2[!is.na(date)]
work2
n.sites <- nrow(work2)
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := str_remove(date, "r\\.$")]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw25"]
all.data[[25]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 26.data: Wyniki MMI2011r Ciechanów/formatka O_2011_wer2.xlsx --------------------------------------------------------------------------------
folder   <- "2011/Wyniki MMI2011r Ciechanów"
data.set <- "formatka O_2011_wer2.xlsx"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL/", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
work2 <- work2[!is.na(date)]
work2
n.sites <- nrow(work2)
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := str_remove(date, "r\\.$")]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw26"]
all.data[[26]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 27.data: 07_MMI 2012/Ciechanów - wyniki MMI 2012.xlsx --------------------------------------------------------------------------------
folder   <- "07_MMI 2012"
data.set <- "Ciechanów - wyniki MMI 2012.xlsx"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL/", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
work2 <- work2[!is.na(date)]
work2
n.sites <- nrow(work2)
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := str_remove(date, "r\\.$")]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw27"]
all.data[[27]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 28.data: 07_MMI 2012/Ciechanów -wyniki MMI(Ostroê®ka) 2012.xlsx --------------------------------------------------------------------------------
folder   <- "07_MMI 2012"
data.set <- "Ciechanów -wyniki MMI(Ostroê®ka) 2012.xlsx"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL/", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
work2 <- work2[!is.na(date)]
work2
n.sites <- nrow(work2)
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := str_remove(date, "r\\.$")]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw28"]
all.data[[28]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)

# 29.data: 07_MMI 2012/Radom- wyniki MMI 2012r.xls --------------------------------------------------------------------------------
folder   <- "07_MMI 2012"
data.set <- "Radom- wyniki MMI 2012r.xls"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL/", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
work2 <- work2[!is.na(date)]
work2
n.sites <- nrow(work2)
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := str_remove(date, "r\\.$")]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw29"]
all.data[[29]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)
# 30.data: 07_MMI 2012/Warszawa -wyniki MMI 2012.xls --------------------------------------------------------------------------------
folder   <- "07_MMI 2012"
data.set <- "Warszawa -wyniki MMI 2012.xls"
file.name <- paste0("data/original data/monitoring_poland/raw_data/MZB_PL/", folder, "/",data.set)
work <- read_excel(file.name, sheet = 3)
#- extract date and site variables 
date.var <- unlist(work[4, -c(1:2)])
original_site_name <- unlist(work[6, -c(1:2)])
#- combine in data.table 
work2 <- data.table(original_site_name = original_site_name,
                    date = date.var)
work2 <- work2[!is.na(date)]
work2
n.sites <- nrow(work2)
work <- read_excel(file.name, sheet = 3, skip = 11)
#- subset to sites (including taxon column)
work <- work[,1:(n.sites+2)]
work %<>% select(!BMWP_PL)
names(work) <- append("taxon", work2$original_site_name)
work %<>%
        apply(2,as.character) %>%
        as.data.frame() %>%
        pivot_longer(cols = !taxon, names_to = "original_site_name", values_to = "abundance") %>%
        mutate(abundance = str_trim(abundance)) %>%
        filter(!is.na(abundance)) %>%
        filter(!abundance %in% c("0", "0.0", "0.000000")) %>%
        filter(!is.na(taxon)) %>%
        filter(taxon != "RAZEM") %>%
        setDT()

work <- work2[work, on = "original_site_name"]
work <- work[original_site_name %in% coords.obj$original_site_name]
work
work <- coords.obj[work, on = "original_site_name"]
work[, EPSG := 4326]
work[, date := str_remove(date, "r\\.$")]
work[, date := dmy(date)]
work[, year := year(date)]
work[, month := month(date)]
work[, season := case_when(month %in% c(3:5) ~ "spring", month %in% c(6:8) ~ "summer", month %in% c(9:11) ~ "autumn", month %in% c(12:2) ~ "winter")]
work[, data.set := "raw30"]
all.data[[30]] <- work
rm(work, original_site_name, date.var, work2, n.sites, data.set, folder, file.name)



# combine data  ---------------------------------------------------------------------

all.data <- lapply(all.data, function(x) x[, abundance := as.character(abundance)])
all.data <- lapply(all.data, function(x) x[, date := as.Date(date)])
data2 <- rbindlist(all.data, fill =T)
#- check for NAs 
#apply(data2, 2, anyNA)

#- check that spatial locations make sense 
#sites <- data2 |> unique(by = "original_site_name") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1])
#mapview::mapview(sites)# mapview::mapview(sites)

#- check taxa names 
data2[, taxon := str_remove(taxon, "\\ IMAGO\\ \\+\\ LARWY")]
data2[, taxon := str_remove(taxon, "\\ -\\ IMAGO")]
data2[, taxon := str_remove(taxon, "\\ -\\ LARWY")]
data2[, taxon := str_remove(taxon, "\\ -\\ larwy")]
data2[, taxon := str_remove(taxon, "\\ -\\ POCZWARKI")]
data2[, taxon := str_remove(taxon, "\\ -\\ PIJAWKI")]
data2[, taxon := str_remove(taxon, "\\ -\\ IMAGINES")]  
data2[, taxon := str_remove(taxon, "-\\ IMAGINES")]  
data2[, taxon := str_remove(taxon, "-\\ imagines")]  
data2[, taxon := str_remove(taxon, "\\ -\\ WAŻKI")]
data2[, taxon := str_remove(taxon, "\\ \\-\\ WAŻKI")]
data2[, taxon := str_remove(taxon, "\\ -$")]
data2[, taxon := str_remove(taxon, "\\ \\(.*\\)")]
data2[, taxon := str_remove(taxon, "\\ sp\\.")]
data2[, taxon := str_remove(taxon, "\\ spp\\.")]
data2 <- data2[taxon != "suma"]
data2[, taxon := str_trim(taxon)]
data2[taxon == "Anabolia laevis/furcata", taxon := "Anabolia"]
data2[taxon == "Astacidae / Cambaridae", taxon := "Decapoda"]
data2[taxon == "Athripsodes bilineatus/A. albifrons/A. cinereus", taxon := "Athripsodes"]
data2[taxon == "Capnidae / Leuctridae", taxon := "Plecoptera"]
data2[taxon == "Corduliidae/Libellulidae", taxon := "Odonata"]
data2[taxon == "Dugesia lugubris / polychlora", taxon := "Dugesia"]
data2[taxon == "Leptocerus tineiformis/Leptocerus interrputus", taxon := "Leptocerus"]
data2[taxon == "Mystacides longicornis/Mystacides nigra", taxon := "Mystacides"]
data2[taxon == "Oecetis testacea/Oecetis furva", taxon := "Oecetis"]
data2[taxon == "Onychogomphus / Ophiogomphus", taxon := "Gomphidae"]
data2[taxon == "Onychogomphus / Ophiogomphus sp.", taxon := "Gomphidae"]
data2[taxon == "Somatochlora metallica / flavomaculata", taxon := "Somatochlora"]
data2[taxon == "Diamesinae+Prodiamesinae", taxon := "Chironomidae"]

TU     <- sort(unique(data2$taxon))
new_tu <- which(!TU %in% taxontable$original_name)
TU <- TU[new_tu]
