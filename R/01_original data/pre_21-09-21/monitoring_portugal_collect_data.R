
combine_data <- list()

# 1. Dados_ARH_Alg_Invertebrados_2015.xlsx ------------------------------------------------------------------------------

data  <- read_excel("data/original data/monitoring_portugual/raw_data/Dados_ARH_Alg_Invertebrados_2015.xlsx")
sites <- read_excel("data/original data/monitoring_portugual/raw_data/Dados_ARH_Alg_Invertebrados_2015.xlsx", sheet = 2)
data2 <- pivot_longer(data, cols = !c("Estação", "Local", "Data"), names_to = "taxon", values_to = "abundance")
data2 <- filter(data2, !is.na(abundance))
unique(data2$Data)
data3 <- left_join(data2, 
                   sites, 
                   by = c("Estação" = "Cod_est_ficheiro-original"))

sites2 <- unique(data3, by = "Estação")
sites2 %<>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
mapview(sites2)

data.out <- data.table(original_site_name = data3$Estação, 
                      date = as.Date(NA), 
                      season = "spring",
                      year = 2015, 
                      taxon = data3$taxon, 
                      abundance = data3$abundance, 
                      EPSG = 4326, 
                      x.coord = data3$Longitude,
                      y.coord = data3$Latitude,
                      data.set = "raw1")

combine_data[[1]] <- data.out

# 2.Dados_ARHAlent_diatom+invert_2010.xlsx --------------------------------------------

data  <- read_excel("data/original data/monitoring_portugual/raw_data/Dados_ARHAlent_diatom+invert_2010.xlsx")

header <- data[1:4,]
header <- header[,-c(1:2)]
site_name <- names(header)
site_name <- site_name[-1]
site_name <- site_name[1:26]

original_site_names <- 
        header[1,-1] |> 
        unlist() |> 
        unname() |> 
        na.exclude()
x.coord <- 
        header[2,-1] |> 
        unlist() |> 
        unname() |> 
        na.exclude() |> 
        as.numeric()
y.coord <- 
        header[3,-1] |> 
        unlist() |> 
        unname() |> 
        na.exclude() |> 
        as.numeric()
date <-
        c(
                "15.05.2010",
                "15.05.2010",
                "15.05.2010",
                "31.05.2010",
                "15.05.2010",
                "05.05.2010",
                "05.05.2010",
                "14.05.2010",
                "16.05.2010",
                "05.06.2010",
                "05.06.2010",
                "30.05.2010",
                "03.06.2010",
                "05.06.2010",
                "30.05.2010",
                "30.05.2010",
                "03.06.2010",
                "03.06.2010",
                "30.05.2010",
                "30.05.2010",
                "31.05.2010",
                "03.06.2010",
                "03.06.2010",
                "03.06.2010",
                "03.06.2010",
                "03.06.2010"
        )

#- CRS found via http://projfinder.com/
header2 <- data.table(original_site_names, x.coord, y.coord, date, site_name)
sites <- st_as_sf(header2, coords=c("x.coord", "y.coord"), crs = 20790)
sites %<>% st_transform(crs = 4326)
coords.new <- st_coordinates(sites)
header2$x.coord <- coords.new[,1]
header2$y.coord <- coords.new[,2]
header2$season = "spring"

taxa <- data[-c(1:5), -c(1:2)]
taxa %<>% 
        rename(taxon = "Nome da estação") %>%
        dplyr::select(!starts_with("...")) %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
          header2, 
          by = "site_name")

data.out <- data.table(original_site_name = data$original_site_names,
                       date = dmy(data$date), 
                       season = data$season, 
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "raw2")
combine_data[[2]] <- data.out


# 3.RH1_MACROINVERTEBRADOS.xlsx -------------------------------------------------------

data  <- read_excel("data/original data/monitoring_portugual/raw_data/RH1_MACROINVERTEBRADOS.xlsx")

site_name <- names(data)
site_name <- site_name[-1]

original_site_names <- 
        data[5,-1] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[6,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[7,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
date <- 
        c(
                "05.06.2017",
                "05.06.2017",
                "05.06.2017",
                "06.06.2017",
                "06.06.2017",
                "07.06.2017",
                "06.06.2017",
                "06.06.2017",
                "06.06.2017",
                "06.06.2017",
                "07.06.2017",
                "07.06.2017",
                "07.06.2017",
                "07.06.2017",
                "05.06.2017",
                "05.06.2017",
                "05.06.2017",
                "05.06.2017",
                "05.06.2017",
                "06.06.2017",
                "06.06.2017"
        )

#- CRS found via http://projfinder.com/
data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = dmy(date), year = 2017, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
data2$season <- "summer"

taxa <- data[-c(1:12), ]
taxa %<>% 
        rename(taxon = "COD_RH") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data.out <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       season = data$season, 
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "raw3",
                       year = 2017)
combine_data[[3]] <- data.out

# 4.RH2_MACROINVERTEBRADOS.xlsx -------------------------------------------------------

data  <- read_excel("data/original data/monitoring_portugual/raw_data/RH2_MACROINVERTEBRADOS.xlsx")

site_name <- names(data)
site_name <- site_name[-1]

original_site_names <- 
        data[5,-1] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[6,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[7,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
date <-
        c(
                "01.06.2017",
                "01.06.2017",
                "31.05.2017",
                "31.05.2017",
                "01.06.2017",
                "01.06.2017",
                "30.05.2017",
                "01.06.2017",
                "31.05.2017",
                "31.05.2017",
                "31.05.2017",
                "31.05.2017",
                "31.05.2017",
                "29.05.2017",
                "29.05.2017",
                "29.05.2017",
                "29.05.2017",
                "29.05.2017",
                "29.05.2017",
                "30.05.2017",
                "30.05.2017",
                "30.05.2017",
                "30.05.2017",
                "30.05.2017",
                "25.05.2017",
                "29.05.2017",
                "30.05.2017",
                "25.05.2017",
                "22.05.2017",
                "30.05.2017"
        )


#- CRS found via http://projfinder.com/
data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = dmy(date), year = 2017, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
data2$season <- "summer"

taxa <- data[-c(1:12), ]
taxa %<>% 
        rename(taxon = "COD_RH") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data.out <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       season = data$season, 
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "raw4",
                       year = 2017)
combine_data[[4]] <- data.out

# 5.RH3_MACROINVERTEBRADOS.xlsx -------------------------------------------------------

data  <- read_excel("data/original data/monitoring_portugual/raw_data/RH3_MACROINVERTEBRADOS.xlsx")

site_name <- names(data)
site_name <- site_name[-1]

original_site_names <- 
        data[5,-1] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[6,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[7,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
date <- 
        c("24.04.2017",	"24.05.2017",	"24.05.2017",	"23.05.2017",	"24.05.2017",	"26.04.2017",	"22.05.2017",	"15.05.2017",	"15.05.2017",	"17.05.2017",	"15.05.2017",	"15.05.2017",	"15.05.2017",	"20.04.2017",	"20.04.2017",	"03.05.2017",	"09.05.2017",	"20.04.2017",	"20.04.2017",	"03.05.2017",	"18.04.2017",	"02.05.2017",	"18.04.2017",	"18.04.2017",	"18.04.2017",	"19.04.2017",	"18.04.2017",	"04.05.2017",	"04.05.2017",	"04.05.2017",	"10.05.2017",	"10.05.2017",	"10.05.2017",	"10.05.2017",	"10.05.2017",	"10.05.2017",	"24.04.2017",	"10.05.2017",	"18.04.2017",	"18.04.2017",	"19.04.2017",	"19.04.2017",	"19.04.2017",	"19.04.2017",	"20.04.2017",	"26.04.2017",	"08.05.2017",	"20.04.2017",	"17.05.2017",	"03.05.2017",	"26.04.2017",	"17.05.2017",	"08.05.2017",	"08.05.2017",	"08.05.2017",	"10.05.2017",	"03.05.2017",	"11.05.2017",	"10.05.2017",	"25.04.2017",	"09.05.2017",	"09.05.2017",	"09.05.2017",	"04.05.2017",	"09.05.2017",	"25.04.2017",	"26.04.2017",	"25.04.2017",	"09.05.2017",	"08.05.2017",	"08.05.2017",	"08.05.2017",	"09.05.2017",	"03.05.2017",	"08.05.2017",	"02.05.2017",	"04.05.2017",	"03.05.2017",	"18.05.2017",	"02.05.2017",	"25.04.2017",	"18.05.2017",	"03.05.2017",	"03.05.2017",	"18.05.2017",	"18.05.2017",	"03.05.2017",	"02.05.2017",	"02.05.2017",	"04.05.2017",	"04.05.2017",	"24.04.2017",	"11.05.2017",	"25.05.2017",	"11.05.2017",	"24.04.2017",	"25.05.2017",	"24.04.2017",	"25.05.2017",	"24.04.2017",	"17.05.2017",	"24.04.2017",	"24.05.2017",	"24.05.2017",	"17.05.2017",	"24.05.2017",	"16.05.2017",	"23.05.2017",	"17.05.2017",	"25.05.2017",	"16.05.2017",	"23.05.2017",	"23.05.2017",	"24.05.2017",	"24.05.2017",	"23.05.2017",	"16.05.2017",	"23.05.2017",	"15.05.2017",	"16.05.2017",	"22.05.2017",	"16.05.2017",	"16.05.2017",	"16.05.2017",	"25.04.2017",	"26.04.2017",	"11.05.2017",	"02.05.2017",	"18.04.2017",	"02.05.2017",	"25.04.2017",	"23.05.2017",	"23.05.2017",	"22.05.2017",	"22.05.2017",	"22.05.2017",	"22.05.2017",	"22.05.2017")

#- CRS found via http://projfinder.com/
data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = dmy(date), year = 2017, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
data2$season <- "summer"

taxa <- data[-c(1:12), ]
taxa %<>% 
        rename(taxon = "COD_RH") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data.out <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       season = data$season, 
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "raw5",
                       year = 2017)
combine_data[[5]] <- data.out

# 6.RH4A_MACROINVERTEBRADOS.xlsx -------------------------------------------------------

data  <- read_excel("data/original data/monitoring_portugual/raw_data/RH4A_MACROINVERTEBRADOS.xlsx")

site_name <- names(data)
site_name <- site_name[-1]

original_site_names <- 
        data[5,-1] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[6,-1] |> 
        unlist() |> 
        str_replace(",", "\\.") |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[7,-1] |> 
        unlist() |> 
        str_replace(",", "\\.") |> 
        unname() |> 
        as.numeric()
date <- 
        c("23.05.2017",	"22.05.2017",	"23.05.2017",	"11.05.2017",	"11.05.2017",	"11.05.2017",	"22.05.2017",	"18.05.2017",	"18.05.2017",	"05.05.2017",	"02.05.2017",	"08.05.2017",	"10.05.2017",	"12.04.2017",	"12.04.2017",	"12.04.2017",	"04.05.2017",	"03.05.2017",	"03.05.2017",	"02.05.2017",	"02.05.2017",	"08.05.2017",	"19.04.2017",	"19.04.2017",	"04.05.2017",	"19.04.2017",	"19.04.2017",	"04.05.2017",	"20.04.2017",	"20.04.2017",	"20.04.2017",	"21.04.2017",	"11.04.2017",	"11.04.2017",	"13.04.2017",	"10.04.2017",	"11.04.2017",	"18.04.2017",	"13.04.2017",	"13.04.2017",	"10.04.2017",	"10.04.2017",	"18.04.2017",	"18.04.2017",	"06.04.2017",	"06.04.2017",	"06.04.2017",	"07.04.2017",	"07.04.2017",	"05.04.2017",	"05.04.2017",	"03.04.2017",	"05.04.2017",	"04.04.2017",	"04.04.2017",	"04.04.2017",	"04.04.2017",	"03.04.2017",	"03.04.2017",	"04.05.2017",	"21.04.2017",	"20.04.2017",	"06.04.2017",	"18.04.2017",	"10.04.2017",	"07.04.2017",	"05.05.2017",	"23.05.2017",	"18.05.2017",	"05.05.2017",	"22.05.2017",	"22.05.2017",	"03.05.2017",	"08.05.2017",	"08.05.2017",	"10.05.2017",	"12.04.2017",	"11.04.2017",	"25.05.2017",	"19.05.2017",	"25.05.2017",	"25.05.2017",	"02.05.2017",	"03.05.2017",	"02.05.2017",	"21.04.2017",	"20.04.2017",	"19.04.2017")

#- CRS found via http://projfinder.com/
data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = dmy(date), year = 2017, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
data2$season <- "summer"

taxa <- data[-c(1:12), ]
taxa %<>% 
        rename(taxon = "COD_RH") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data %<>% filter(!is.na(x.coord))

sites <- st_as_sf(data, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
data.out <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       season = data$season, 
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "raw6",
                       year = 2017)
combine_data[[6]] <- data.out

# 7.RH5A_MACROINVERTEBRADOS.xlsx -------------------------------------------------------

data  <- read_excel("data/original data/monitoring_portugual/raw_data/RH5A_MACROINVERTEBRADOS.xlsx")

site_name <- names(data)
site_name <- site_name[-1]

original_site_names <- 
        data[5,-1] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[6,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[7,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
date <- 
        c(
                "25.05.2017",	"01.06.2017",	"01.06.2017",	"30.05.2017",	"30.03.2017",	"12.04.2017",	"19.04.2017",	"09.05.2017",	"13.04.2017",	"21.04.2017",	"18.04.2017",	"08.05.2017",	"21.04.2017",	"28.04.2017",	"28.04.2017",	"09.05.2017",	"09.05.2017",	"09.05.2017",	"18.05.2017",	"10.05.2017",	"19.05.2017",	"05.06.2017",	"02.06.2017",	"28.04.2017",	"28.04.2017",	"29.03.2017",	"03.04.2017",	"19.05.2017",	"03.04.2017",	"23.05.2017",	"23.05.2017",	"29.03.2017",	"29.03.2017",	"10.04.2017",	"19.05.2017",	"28.03.2017",	"10.04.2017",	"10.04.2017",	"24.04.2017",	"24.04.2017",	"11.04.2017",	"14.05.2017",	"02.05.2017",	"31.03.2017",	"31.03.2017",	"31.03.2017",	"07.04.2017",	"05.06.2017",	"01.06.2017",	"31.03.2017",	"29.03.2017",	"15.05.2017",	"05.06.2017",	"11.04.2017",	"31.03.2017",	"06.04.2017",	"03.05.2017",	"12.04.2017",	"03.04.2017",	"01.10.2017",	"01.04.2017",	"18.04.2017",	"02.06.2017",	"15.05.2017",	"22.05.2017",	"04.04.2017",	"04.04.2017",	"04.04.2017",	"08.05.2017",	"07.04.2017",	"29.05.2017",	"23.05.2017",	"24.05.2017",	"03.04.2017",	"05.04.2017",	"02.06.2017",	"20.04.2017",	"15.05.2017",	"07.04.2017",	"06.04.2017",	"12.04.2017",	"29.05.2017",	"31.05.2017",	"01.06.2017",	"03.04.2017",	"19.05.2017",	"05.06.2017",	"02.06.2017",	"25.05.2017",	"04.05.2017",	"22.05.2017",	"02.05.2017",	"28.03.2017",	"28.03.2017",	"05.04.2017",	"09.05.2017",	"08.05.2017",	"29.05.2017",	"18.04.2017",	"03.05.2017",	"30.05.2017",	"04.05.2017",	"03.05.2017",	"04.05.2017",	"11.04.2017",	"10.04.2017",	"25.05.2017",	"07.04.2017",	"24.05.2017",	"30.03.2017",	"21.04.2017",	"09.05.2017",	"27.04.2017",	"27.04.2017",	"06.05.2017",	"12.04.2017",	"20.04.2017",	"24.04.2017",	"19.04.2017",	"19.04.2017",	"03.05.2017",	"05.04.2017",	"31.03.2017",	"06.05.2017",	"22.05.2017",	"29.05.2017",	"30.03.2017",	"24.04.2017",	"06.05.2017",	"17.05.2017",	"02.05.2017",	"26.04.2017",	"23.05.2017",	"18.04.2017",	"01.06.2017",	"30.05.2017",	"15.05.2017",	"03.04.2017",	"31.05.2017",	"05.06.2017",	"27.04.2017",	"12.04.2017",	"30.03.2017",	"08.05.2017",	"18.05.2017",	"26.04.2017",	"18.05.2017",	"20.04.2017",	"17.05.2017",	"17.05.2017",	"13.04.2015",	"13.04.2017",	"20.04.2017",	"30.05.2017",	"05.06.2017",	"22.05.2017",	"16.05.2017",	"23.05.2017",	"30.03.2017",	"19.04.2017",	"04.04.2017",	"16.05.2017",	"28.03.2017",	"02.05.2017",	"02.05.2017",	"06.05.2017",	"28.03.2017",	"26.04.2017",	"26.04.2017",	"22.05.2017",	"03.05.2017",	"18.05.2017",	"13.04.2017",	"04.04.2017",	"30.05.2017",	"13.04.2017",	"01.04.2017",	"17.04.2017",	"29.03.2017",	"16.05.2017",	"10.04.2017",	"31.05.2017",	"30.03.2017",	"28.04.2017",	"20.04.2017",	"05.04.2017",	"16.05.2017",	"25.05.2017",	"31.05.2017",	"31.05.2017")
                
        )

#- CRS found via http://projfinder.com/
data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = dmy(date), year = 2017, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
data2$season <- "summer"

taxa <- data[-c(1:12), ]
taxa %<>% 
        rename(taxon = "COD_RH") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data %<>% filter(original_site_names != "ASSE_2")

data.out <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       season = data$season, 
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "raw7",
                       year = 2017)
combine_data[[7]] <- data.out


# 8. RH6_MACROINVERTEBRADOS.xlsx -------------------------------------------------------

data  <- read_excel("data/original data/monitoring_portugual/raw_data/RH6_MACROINVERTEBRADOS.xlsx")

site_name <- names(data)
site_name <- site_name[-1]

original_site_names <- 
        data[5,-1] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[6,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[7,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
date <- 
        c("16.05.2017",	"16.05.2017",	"17.05.2017",	"16.05.2017",	"30.03.2017",	"30.03.2017",	"30.03.2017",	"18.05.2017",	"18.05.2017",	"17.05.2017",	"17.05.2017",	"17.05.2017",	"17.05.2017",	"30.03.2017",	"30.03.2017",	"06.04.2017",	"30.03.2017",	"30.03.2017",	"06.04.2017",	"06.04.2017",	"18.05.2017",	"02.05.2017",	"18.05.2017",	"18.05.2017",	"07.04.2017",	"18.05.2017",	"07.04.2017",	"18.05.2017",	"07.04.2017",	"07.04.2017",	"07.04.2017",	"06.04.2017",	"06.04.2017",	"06.04.2017",	"03.05.2017",	"03.05.2017",	"02.05.2017",	"02.05.2017",	"02.05.2017",	"07.04.2017",	"02.05.2017",	"24.04.2017",	"24.04.2017",	"03.05.2017",	"03.05.2017",	"27.04.2017",	"27.04.2017",	"26.04.2017",	"27.04.2017",	"27.04.2017",	"27.04.2017",	"27.04.2017",	"26.04.2017",	"26.04.2017",	"24.04.2017",	"24.04.2017",	"03.05.2017",	"22.05.2017",	"22.05.2017",	"26.04.2017",	"26.04.2017",	"26.04.2017",	"23.05.2017",	"28.04.2017",	"28.04.2017",	"28.04.2017",	"28.04.2017",	"28.04.2017",	"28.04.2017",	"22.05.2017",	"22.05.2017",	"24.05.2017",	"23.05.2017",	"23.05.2017",	"24.05.2017",	"23.05.2017",	"23.05.2017",	"23.05.2017",	"19.05.2017",	"19.05.2017",	"19.05.2017",	"24.05.2017",	"24.05.2017",	"24.05.2017",	"19.05.2017",	"19.05.2017")


#- CRS found via http://projfinder.com/
data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = dmy(date), year = 2017, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
data2$season <- "summer"

taxa <- data[-c(1:12), ]
taxa %<>% 
        rename(taxon = "COD_RH") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data.out <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       season = data$season, 
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "raw8",
                       year = 2017)
combine_data[[8]] <- data.out
# 9. RH7_MACROINVERTEBRADOS.xlsx -------------------------------------------------------

data  <- read_excel("data/original data/monitoring_portugual/raw_data/RH7_MACROINVERTEBRADOS.xlsx")

site_name <- names(data)
site_name <- site_name[-1]

original_site_names <- 
        data[5,-1] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[6,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[7,-1] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
date <-
        c("23.03.2017",	"15.05.2017",	"15.05.2017",	"11.05.2017",	"09.05.2017",	"10.05.2017",	"15.05.2017",	"15.05.2017",	"11.05.2017",	"11.05.2017",	"09.05.2017",	"10.05.2017",	"10.05.2017",	"10.05.2017",	"08.05.2017",	"08.05.2017",	"08.05.2017",	"08.05.2017",	"31.03.2017",	"31.03.2017",	"31.03.2017",	"31.03.2017",	"03.04.2017",	"31.03.2017",	"03.04.2017",	"03.04.2017",	"08.05.2017",	"04.05.2017",	"04.05.2017",	"04.05.2017",	"04.05.2017",	"04.05.2017",	"08.05.2017",	"09.05.2017",	"04.05.2017",	"05.04.2017",	"04.04.2017",	"04.04.2017",	"04.04.2017",	"04.04.2017",	"04.04.2017",	"10.04.2017",	"05.04.2017",	"05.04.2017",	"03.04.2017",	"10.04.2017",	"05.04.2017",	"05.04.2017",	"05.04.2017",	"20.04.2017",	"20.04.2017",	"12.04.2017",	"20.04.2017",	"10.04.2017",	"10.04.2017",	"10.04.2017",	"10.04.2017",	"12.04.2017",	"12.04.2017",	"26.05.2017",	"11.04.2017",	"11.04.2017",	"11.04.2017",	"25.05.2017",	"12.04.2017",	"24.04.2017",	"25.05.2017",	"25.05.2017",	"18.04.2017",	"21.04.2017",	"18.04.2017",	"21.04.2017",	"25.05.2017",	"18.04.2017",	"18.04.2017",	"26.05.2017",	"24.04.2017",	"29.03.2017",	"19.04.2017",	"19.05.2017",	"21.04.2017",	"26.05.2017",	"26.05.2017",	"26.05.2017",	"29.03.2017",	"29.03.2017",	"29.03.2017",	"19.04.2017",	"19.04.2017",	"19.04.2017",	"23.03.2017",	"22.03.2017",	"22.03.2017",	"22.03.2017",	"22.03.2017",	"23.03.2017")

        )                

)

#- CRS found via http://projfinder.com/
data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = dmy(date), year = 2017, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
data2$season <- "summer"

taxa <- data[-c(1:12), ]
taxa %<>% 
        rename(taxon = "COD_RH") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data.out <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       season = data$season, 
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                       data.set = "raw9",
                       year = 2017)
combine_data[[9]] <- data.out
# END -------------------------------------------------------------------------------


data <- rbindlist(combine_data, fill = T)
data[, data.set := "monitoring_protugal"]
