# ----------------------------- #
### --- Combine data sets --- ### 
# ----------------------------- #

# --------------- #
# files in:
#       -> individual mzb data sets | multiple files. Each containing on referd to in Table 1 of the publication.
# files out:
#       <- 01_all_mzb_combined.rds  | combined macroinvertebrate observations 
# Purpose:
#       Combine individual macroinvertebrate data sets into one harmonized data set.   
# --------------- #

# Setup -------------------------------------------------------------------
pacman::p_load(
        data.table,
        dplyr,
        magrittr,
        sf
)

## -- file directories 
dir = list(ind = here("~/01_Uni/02_getreal/02_wp2/data/originals_processed/"))

# load data ---------------------------------------------------------------

set01 <- readRDS(file.path(dir$ind, "ld_mzb.rds"))
set02 <- readRDS(file.path(dir$ind, "mi_mzb.rds"))
set04 <- readRDS(file.path(dir$ind, "na_mzb.rds"))
set05 <- readRDS(file.path(dir$ind, "pb_mzb1.rds"))
set06 <- readRDS(file.path(dir$ind, "pb_mzb2.rds"))
set08 <- readRDS(file.path(dir$ind, "hd_mzb.rds"))
set09 <- readRDS(file.path(dir$ind, "rp_mzb.rds"))
set10 <- readRDS(file.path(dir$ind, "cf_stars_mzb.rds"))
set11 <- readRDS(file.path(dir$ind, "cf_wiser_mzb.rds"))
set12 <- readRDS(file.path(dir$ind, "sk_mzb.rds"))
set13 <- readRDS(file.path(dir$ind, "kh_mzb.rds"))
set14 <- readRDS(file.path(dir$ind, "pp_mzb.rds"))
set15 <- readRDS(file.path(dir$ind, "ep_mzb.rds"))
set16 <- readRDS(file.path(dir$ind, "ph_mzb.rds"))


# prepare data ------------------------------------------------------------

## -- vector with all names 
ch_files <- ls()[grepl("^set", ls())]

## -- convert all objects to a spatial format supported in the sf package 
for (i in 1:length(ch_files)) {
        y = get(ch_files[i])
        x = copy(y)
        if (!"sf" %in% class(x))
                x %<>% st_as_sf()
        assign(x = ch_files[i],
               value = x)
        rm(x, y)
        gc()
}

## -- Manual Fixes 
set11 %<>% 
        mutate(x_coord = st_coordinates(set11)[,1],
               y_coord = st_coordinates(set11)[,2]) %>%
        mutate(x_coord = replace(x_coord, x_coord == 95689139, 9.5689139)) %>% 
        mutate(x_coord = replace(x_coord, x_coord == 89851591 , 8.9851591 )) %>% 
        mutate(x_coord = replace(x_coord, x_coord == 101171628 , 10.1171628 )) %>% 
        mutate(y_coord = replace(y_coord, y_coord == 559224938 , 55.9224938 )) %>% 
        mutate(y_coord = replace(y_coord, y_coord == 560607186 , 56.0607186 )) %>% 
        mutate(y_coord = replace(y_coord, y_coord == 566543278 , 56.6543278 )) %>% 
        st_drop_geometry() %>% 
        st_as_sf(coords = c("x_coord", "y_coord"), crs = 4326)
set16 %<>% 
        filter(site_id != "00546")
       
## -- transform all data sets to a common coordinate reference system.  

for(i in seq_along(ch_files)) {
        if (i == 1) all_files <- list()
        y = get(ch_files[i])
        x = copy(y)
        if (st_crs(x) != st_crs(set01)){
                x %<>% st_transform(crs = st_crs(set01))
        }
        setDT(x)
        all_files[[i]] = x
        rm(x)
}

# give all data sets a common structure
all_files %<>%
        # keep only relevant columns
        map(.f = ~ .x[, c(
                "gr_sample_id",
                "date",
                "species",
                "genus",
                "family",
                "order",
                "subclass",
                "class",
                "phylum",
                "abundance",
                "pristine",
                "data.set",
                "geometry"
        )])  %>%
        # Data sets where date = NA have a different class in that column than those with actual dates.
        # To avoid having to check each one separately I made sure each date is of class date.
        map(.f = ~ .x[, date := as.Date(date)])
        
## -- combine data sets 
set_all <- rbindlist(all_files)


## -- Visual Checks 1 - write files to hardrive which can be opened in other GIS software (e.g. QGIS).  
set_all %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf() %>%
        st_write("test.gpkg")

## -- Visual Checks 2 - Plot individual data sets directly in R. Can take a while.        
list(all_files[[2]], all_files[[3]]) %>%
        rbindlist() %>%
        st_as_sf() %>%
        tm_shape() + tm_dots(col = "data.set")


## -- remove unneccesary files 
rm(list = setdiff(ls(), c("set_all", "dir")));gc()

# Create Lists  -----------------------------------------------------------
## -- What phyla and classes are present 
unique(set_all$phylum) %>% sort
unique(set_all$class)  %>% sort

## -- find species without genus 
species_without_genus <- set_all[!is.na(species) & is.na(genus), unique(species)]

## -- inset first word of species name as genus
for (i in seq_along(species_without_genus)) 
        set_all[species == species_without_genus[i], genus := word(species_without_genus[i],1)]



# Manual taxonomic fixes  -----------------------------------------------------------

set_all[family   == "Aeolosomatidae",       `:=` (class    = "Polychaeta")]
set_all[genus    == "Bothrioneurum",        `:=` (family   = "Naididae", order = "Haplotaxida", subclass = NA, class = "Clitella", phylum = "Annelida", kingdom = "Animalia")   ]
set_all[order    == "Diplostraca",          `:=` (subclass = "Phyllopoda")]
set_all[family   == "Dorydrilidae",         `:=` (order    = "Haplotaxida", subclass = NA, class = "Clitellata")]
set_all[genus    == "Dugesia",              `:=` (class    = "Turbellaria")]
set_all[order    == "Enchytraeida",         `:=` (class    = "Clitellata")]
set_all[subclass == "Hirudinea",            `:=` (class    = "Clitellata")]
set_all[genus    == "Gammarus",             `:=` (family   = "Gammaridae", order = "Amphipoda", subclass = "Eumalacostraca", class = "Malacostraca", phylum = "Arthropoda")]
set_all[genus    == "Leptocerus",           `:=` (family   = "Leptoceridae", order    = "Trichoptera")]
set_all[family   == "Libellulinae",         `:=` (family   = "Libellulidae", order    = "Odonata")]
set_all[genus    == "Lumbricus",            `:=` (family   = "Lumbricidae", order = "Crassiclitellata")]
set_all[family   == "Lymnaeidae",           `:=` (subclass = "Heterobranchia")]
set_all[order    == "Mytilida",             `:=` (subclass = "Pteriomorphia")]
set_all[subclass == "Oligochaeta",          `:=` (genus    = NA)]
set_all[subclass == "Oligochaeta",          `:=` (family   = NA)]
set_all[subclass == "Oligochaeta",          `:=` (order    = NA)]
set_all[genus    == "Oxygastra",            `:=` (family   = "Synthemistidae")]
set_all[family   == "Physidae",             `:=` (subclass = "Heterobranchia")]
set_all[genus    == "Piguetiella",          `:=` (family   = "Randiellidae")]
set_all[genus    == "Piguetiella",          `:=` (order    = "Enchytraeida")]
set_all[genus    == "Piguetiella",          `:=` (class    = "Clitellata")]
set_all[genus    == "Sperchon",             `:=` (family   = "Sperchonidae")]
set_all[genus    == "Sperchon",             `:=` (order    = "Trombidiformes")]
set_all[genus    == "Sperchon",             `:=` (subclass = "Acari")]
set_all[genus    == "Sperchon",             `:=` (class    = "Arachnida")]
set_all[genus    == "Sperchon",             `:=` (phylum   = "Arthropoda")]
set_all[order    == "Tricladida",           `:=` (class    = "Turbellaria")]
set_all[family   == "Crustaceae",           `:=` (family   = NA)]
set_all[species  == "Notonectidae",         `:=` (species  = NA, genus = NA)]
set_all[genus    == "Hydraenida",           `:=` (genus    = NA)]
set_all[family   == "Hydrachnidiae",        `:=` (family   = "Hydrachnidae")]
set_all[family   == "Stratiomyiidae",       `:=` (family   = "Stratiomyidae")]
set_all[order    == "Passeriformes",        `:=` (species  = NA, genus = NA, family = NA)]
set_all[family   == "Cordulegasteridae",    `:=` (family = "Cordulegastridae")]
set_all[family   == "Helophorida",          `:=` (family = "Helophoridae")]
set_all[family   == "Helophoridae",         `:=` (order  = "Coleoptera")]
set_all[family   == "Psychomyidae",         `:=` (family = "Psychomyiidae")]
set_all[genus    == "Colgmia",              `:=` (genus  = "Clogmia")]
set_all[species  == "Peregriana peregra",   `:=` (species = "Radix peregra", genus = "Radix")]
set_all[class    == "Enopla Schultze 1851", `:=` (class    = "Enopla")]
set_all[order    == "Venerida",             `:=` (subclass = "Heterodonta")]
set_all[order    == "Ectobranchia",         `:=` (subclass = "Caenogastropoda")]
set_all[class    == "Bivalva",              `:=` (class    = "Bivalvia")]
set_all[order    == "Capitellida",          `:=` (subclass = "Scolecida")]
set_all[order    == "Capitellida",          `:=` (class    = "Polychaeta")]
set_all[family   == "Acroloxidae",          `:=` (subclass = NA)]
set_all[family   == "Acroloxidae",          `:=` (class    = "Gastropoda")]
set_all[family   == "Acroloxidae",          `:=` (phylum   = "Mollusca")]
set_all[family   == "Dytiscidae",           `:=` (order    = "Coleoptera")]
set_all[family   == "Planorbidae",          `:=` (order    = NA)]
set_all[family   == "Planorbidae",          `:=` (subclass = "Heterobranchia")] 
set_all[subclass == "Heterobranchia",       `:=` (class    = "Gastropoda")] 
set_all[class    == "Gastropoda",           `:=` (phylum   = "Mollusca")]
set_all[family   == "Sphaeriidae",          `:=` (order    = "Sphaeriida")]
set_all[order    == "Sphaeriida",           `:=` (subclass = NA)]
set_all[order    == "Sphaeriida",           `:=` (class    = "Bivalvia")]
set_all[class    == "Bivalvia",             `:=` (phylum   = "Mollusca")]
set_all[genus    == "Dugesia",              `:=` (family   = "Dugesiidae")]
set_all[genus    == "Dugesia",              `:=` (order    = "Tricladida")]
set_all[phylum   == "Platyhelminthes",      `:=` (phylum   = "Plathelminthes")]
set_all[phylum   == "Nematomorpha",         `:=` (phylum   = "Nematomorpha")]
set_all[order    == "Arhynchobdellida",     `:=` (subclass = "Hirudinea")]
set_all[order    == "Rhynchobdellida",      `:=` (subclass = "Hirudinea")]
set_all[order    == "Isopoda",              `:=` (subclass = "Eumalacostraca")]
set_all[order    == "Amphipoda",            `:=` (subclass = "Eumalacostraca")]
set_all[order    == "Spongillida",          `:=` (subclass = "Heteroscleromorpha")]
set_all[order    == "Littorinimorpha",      `:=` (subclass = "Caenogastropoda")]
set_all[class    == "Clitella",             `:=` (class     = "Clitellata")]
set_all[order    == "Decapoda",             `:=` (subclass = "Eumalacostraca")]
set_all[order    == "Architaenioglossa",    `:=` (subclass = "Caenogastropoda")]
set_all[order    == "Terebellida",          `:=` (class    = "Polychaeta")]
set_all[order    == "Mysida",               `:=` (subclass = "Eumalacostraca")]
set_all[order    == "Trombidiformes",       `:=` (subclass = "Acari")]
set_all[order    == "Trombidiformes",       `:=` (class    = "Arachnida")]
set_all[order    == "Anthoathecata",        `:=` (subclass = "Hydroidolina")]
set_all[order    == "Stylommatophora",      `:=` (subclass = "Heterobranchia")]
set_all[order    == "Cladocera",            `:=` (subclass = "Phyllopoda")]
set_all[order    == "Cyclopoida",           `:=` (subclass = "Copepoda")]
set_all[order    == "Arguloida",            `:=` (subclass = "Branchiura")]
set_all[subclass == "Branchiura",           `:=` (class    = "Maxillopoda")]
set_all[order    == "Mermithida",           `:=` (subclass = "Dorylaimia")]
set_all[subclass == "Dorylaimia",           `:=` (class    = "Enoplea")]
set_all[order    == "Monostilifera",        `:=` (subclass = "Hoplonemertea")]
set_all[subclass == "Hoplonemertea",        `:=` (class    = "Enopla")]
set_all[order    == "Araneae",              `:=` (class    = "Arachnida")]
set_all[order    == "Calanoida",            `:=` (subclass = "Copepoda")]
set_all[family   == "Capniidae",            `:=` (order    = "Plecoptera")]
set_all[order    == "Plecoptera",           `:=` (subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
set_all[family   == "Valvatidae",           `:=` (order = NA, subclass = NA,  class = "Gastropoda", phylum = "Mollusca", kingdom = "Animalia")]
set_all[family   == "Grynidae",             `:=` (family := "Gyrinidae")]
set_all[genus    == "Stylaria",             `:=` (family = "Naididae", order = "Haplotaxida", subclass = NA, class = "Clitellata", phylum = "Annelida", kingdom = "Animalia")]
set_all[genus    == "Micronecta",           `:=` (family = "Corixidae", order = "Hemiptera", subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
set_all[genus    == "Stylodrilus",          `:=` (family = "Lumbriculidae", order = "Lumbriculida", subclass = NA, class = "Clitellata", kingdom = "Animalia")]
set_all[genus    == "Propappus",            `:=` (family = "Propappidae", order = "Enchytraeida", subclass = NA, class = "Clitellata", phylum = "Annelida", kingdom = "Animalia")]
set_all[genus    == "Brachionus",           `:=` (family = "Brachionidae", order = "Ploima", subclass = NA, class = "Monogononta", phylum = "Rotifera", kingdom = "Animalia")]
set_all[genus    == "Anisus",               `:=` (family = "Planorbidae", order = NA, subclass = NA, class = "Gastropoda", phylum = "Mollusca", kingdom = "Animalia")]
set_all[genus    == "Hydaticus",            `:=` (family = "Dytiscidae", order = "Coleoptera", class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
set_all[order    == "Ploima",               `:=` (subclass = NA, class = "Monogononta", phylum = "Rotifera", kingdom = "Animalia")]
set_all[family   == "Planorbidae",          `:=` (order = NA, subclass = NA, class = "Gastropoda", phylum = "Mollusca", kingdom = "Animalia")]
set_all[genus    == "Hydracarina",          `:=` (genus = NA, family = NA)]
set_all[phylum   == "Platyhelminthes",      `:=` (phylum := "Plathelminthes")]
set_all[phylum   == "Nematomorpha",         `:=` (phylum := "Nematophora")]
set_all[order    == "Arhynchobdellida",     `:=` (subclass := "Hirudinea")]
set_all[order    == "Rhynchobdellida",      `:=` (subclass := "Hirudinea")]
set_all[order    == "Isopoda",              `:=` (subclass := "Eumalacostraca")]
set_all[order    == "Amphipoda",            `:=` (subclass := "Eumalacostraca")]
set_all[order    == "Spongillida",          `:=` (subclass := "Heteroscleromorpha")]
set_all[order    == "Littorinimorpha",      `:=` (subclass := "Caenogastropoda")]
set_all[class    == "Clitella",             `:=` (class := "Clitellata")]
set_all[order    == "Decapoda",             `:=` (subclass := "Eumalacostraca")]
set_all[order    == "Architaenioglossa",    `:=` (subclass := "Caenogastropoda")]
set_all[order    == "Terebellida",          `:=` (class := "Polychaeta")]
set_all[order    == "Mysida",               `:=` (subclass := "Eumalacostraca")]
set_all[order    == "Trombidiformes",       `:=` (subclass := "Acari")]
set_all[order    == "Trombidiformes",       `:=` (class := "Arachnida")]
set_all[order    == "Anthoathecata",        `:=` (subclass := "Hydroidolina")]
set_all[order    == "Stylommatophora",      `:=` (subclass := "Heterobranchia")]
set_all[order    == "Cladocera",            `:=` (subclass := "Phyllopoda")]
set_all[order    == "Cyclopoida",           `:=` (subclass := "Copepoda")]
set_all[order    == "Arguloida",            `:=` (subclass := "Branchiura")]
set_all[subclass == "Branchiura",           `:=` (class := "Maxillopoda")]
set_all[order    == "Mermithida",           `:=` (subclass := "Dorylaimia")]
set_all[subclass == "Dorylaimia",           `:=` (class := "Enoplea")]
set_all[order    == "Monostilifera",        `:=` (subclass := "Hoplonemertea")]
set_all[subclass == "Hoplonemertea",        `:=` (class := "Enopla")]
set_all[order    == "Araneae",              `:=` (class := "Arachnida")]
set_all[order    == "Calanoida",            `:=` (subclass := "Copepoda")]
set_all[family   == "Capniidae",            `:=` (order := "Plecoptera")]
set_all[order    == "Plecoptera",           `:=` (subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
set_all[family   == "Valvatidae",           `:=` (order = NA, subclass = NA,  class = "Gastropoda", phylum = "Mollusca", kingdom = "Animalia")]
set_all[family   == "Grynidae",             `:=` (family = "Gyrinidae")]
set_all[genus    == "Stylaria",             `:=` (family = "Naididae", order = "Haplotaxida", subclass = NA, class = "Clitellata", phylum = "Annelida", kingdom = "Animalia")]
set_all[genus    == "Micronecta",           `:=` (family = "Corixidae", order = "Hemiptera", subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
set_all[genus    == "Stylodrilus",          `:=` (family = "Lumbriculidae", order = "Lumbriculida", subclass = NA, class = "Clitellata", kingdom = "Animalia")]
set_all[genus    == "Propappus",            `:=` (family = "Propappidae", order = "Enchytraeida", subclass = NA, class = "Clitellata", phylum = "Annelida", kingdom = "Animalia")]
set_all[genus    == "Brachionus",           `:=` (family = "Brachionidae", order = "Ploima", subclass = NA, class = "Monogononta", phylum = "Rotifera", kingdom = "Animalia")]
set_all[genus    == "Anisus",               `:=` (family = "Planorbidae", order = NA, subclass = NA, class = "Gastropoda", phylum = "Mollusca", kingdom = "Animalia")]
set_all[genus    == "Hydaticus",            `:=` (family = "Dytiscidae", order = "Coleoptera", class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
set_all[order    == "Ploima",               `:=` (subclass = NA, class = "Monogononta", phylum = "Rotifera", kingdom = "Animalia")]
set_all[family   == "Planorbidae",          `:=` (order = NA, subclass = NA, class = "Gastropoda", phylum = "Mollusca", kingdom = "Animalia")]

## --  new data set without duplicates 
set_all <- unique(set_all , by = c("gr_sample_id", "species", "genus", "family", "order", "subclass", "class", "phylum"))


# save to file ------------------------------------------------------------
saveRDS(set_all, "data/01_all_mzb_combined.rds")

