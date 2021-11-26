# Purpose: Store all the manual fixes to the taxontable 
# date created: 22-09-21
# date last modfied: 22-09-21

pacman::p_load(data.table)
source("R/fill_taxon_table.R")


# AQEM Romania ----------------------------------------------------------------------

fill.taxon.table("Nais sp.", NA, "Nais", "Naididae", "Haplotaxida", NA, "Clitellata", "Annelida")
fill.taxon.table("Clinocerinae sp.", NA, NA, "Empididae", "Diptera", NA, "Insecta", "Artrhopoda")
fill.taxon.table("Empididae - larva", NA, NA, "Empididae", "Diptera", NA, "Insecta", "Artrhopoda")
fill.taxon.table("Fam. Ceratopogonidae", NA, NA, "Ceratopogonidae", "Diptera", NA, "Insecta", "Artrhopoda")

taxontable[original_name == "Fam. Dixidae", family := "Dixidae"]
taxontable[original_name == "Fam. Dolichopodidae", family := "Dolichopodidae"]
taxontable[original_name == "Fam. Dryopidae",      family := "Dryopidae"]
taxontable[original_name == "Fam. Empididae",      family := "Empididae"]
taxontable[original_name == "Fam. Enchytraeidae",  family := "Enchytraeidae"]
taxontable[original_name == "Fam. Ephydridae",     family := "Ephydridae"]
taxontable[original_name == "Fam. Limoniidae",     family := "Limoniidae"]
taxontable[original_name == "Fam. Psychodidae",    family := "Psychodidae"]
taxontable[original_name == "Fam. Rhagionidae",    family := "Rhagionidae"]
taxontable[original_name == "Fam. Stratiomyidae",  family := "Stratiomyidae"]
taxontable[original_name == "Fam. Tipulidae",      family := "Tipulidae"]
taxontable[original_name == "Nematoda  Gen.sp..",  phylum := "Nematoda"]
taxontable[original_name == "Orthocladiinae Gen. sp.", family := "Chironomidae"]
taxontable[original_name == "Orthocladiinae Gen.sp.",  family := "Chironomidae"]
taxontable[original_name == "Prostigmata sp.",         order := "Trombidiformes"    ]
taxontable[original_name == "sfam. Chironominae",      family := "Chironomidae"  ]
taxontable[original_name == "sfam. Hemerodromiinae",   family := "Empididae" ]
taxontable[original_name == "Trib Eriopterini",        family :=  "Limoniidae"]
taxontable[original_name == "Pericoma sp.",            genus := "Pericoma"]
taxontable[original_name == "Dugesia sp.",             genus := "Dugesia"]
taxontable[original_name == "Fridericia sp.",          genus := "Fridericia"]
taxontable[original_name == "Isotoma sp.", genus := "Isotoma"]
taxontable[original_name == "Limonia sp.", genus := "Loxostege"]
taxontable[original_name == "Oligochaeta varia", class := "Clitellata"]
taxontable[original_name == "Ormosia sp.", family := "Limoniidae"]

# ——— GENUS ——— #
taxontable[genus %in% c("Loxostege"), family := "Crambidae"]
taxontable[genus %in% c("Dugesia"), family := "Dugesiidae"]
taxontable[genus %in% c("Fridericia"), family := "Enchytraeidae"]
taxontable[genus %in% c("Isotoma") , family := "Isotomidae"]
taxontable[genus %in% c("Pericoma"), family := "Psychodidae"]

# ——— FAMILY ——— #
taxontable[family %in% c("Isotomidae")     , class := "Collembola"]
taxontable[family %in% c("Dryopidae")     , order := "Coleoptera"]
taxontable[family %in% c("Dixidae","Dolichopodidae", "Empididae", "Ephydridae",
                         "Limoniidae", "Psychodidae", "Rhagionidae", "Stratiomyidae",
                         "Tipulidae", "Chironomidae") , order := "Diptera"]
taxontable[family %in% c("Enchytraeidae") , order := "Haplotaxida"]
taxontable[family %in% c("Crambidae") , order := "Lepidoptera"]
taxontable[family %in% c("Dugesiidae"), order := "Tricladia"]
# ——— ORDER ——— #
taxontable[order %in% c("Diptera", "Coleoptera", "Lepidoptera", ""), c("subclass", "class") := .(NA, "Insecta")]
taxontable[order %in% c("Trombidiformes"), class := "Arachnida"]
taxontable[order %in% c("Haplotaxida"), class := "Clitellata"]
taxontable[order %in% c("Tricladia"), phylum := "Platyhelminthes"]
# ——— CLASS ——— #        
taxontable[class %in% c("Insecta", "Arachnida", "Collembola"), phylum := "Arthropoda"]
taxontable[class %in% c("Clitellata"), phylum := "Annelida"]
# ——— PHYLUM ——— #
taxontable[phylum %in% c("Arthropoda", "Nematoda", "Annelida", "Platyhelminthes"), kingdom := "Animalia"]

# biodrought ------------------------------------------------------------------------
fill.taxon.table("Amphinemurinae", NA, NA, "Nemouridae", "Plecoptera", NA, "Insecta", "Artrhopoda")
taxontable[order == "Tricladia", order := "Tricladida"]


# ebro basin ------------------------------------------------------------------------
[original_name == "Copepoda", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := 
         .(NA,NA,NA,NA,"Copepoda", "Maxillopoda", "Arthropoda", "Animalia")]
taxontable[original_name == "Ferrissiidae", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := 
                   .(NA,NA,"Planorbidae",NA,NA, "Gastropoda", "Mollusca", "Animalia")]
taxontable[original_name == "Hirudidae", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := 
                   .(NA,NA,"Hirudinidae","Arhynchobdellida",NA, "Clitellata", "Annelida", "Animalia")]
taxontable[original_name == "Filo Bryozoa", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := 
                   .(NA,NA,NA,NA,NA,NA,"Bryozoa", "Animalia")]
taxontable[original_name == "Filo Nematoda", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := 
                   .(NA,NA,NA,NA,NA,NA,"Nematoda", "Animalia")]
taxontable[original_name == "Filo Nematomorpha", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := 
                   .(NA,NA,NA,NA,NA,NA,"Nematomorpha", "Animalia")]

# ecosurv ---------------------------------------------------------------------------
taxontable[original_name == "Agabus neglectus (Erichson 1837)"    ,`:=` (species = "Ilybius neglectus"   , genus = "Ilybius"     , family = "Dytiscidae"     , order = "Coleoptera"       , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Baetis muticus (Linnaeus 1758)"      ,`:=` (species = "Alainites muticus"   , genus = "Alainites"   , family = "Baetidae"       , order = "Ephemeroptera"    , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Baetis niger (Linnaeus 1761)"        ,`:=` (species = "Nigrobaetis niger"   , genus = "Nigrobaetis" , family = "Baetidae"       , order = "Ephemeroptera"    , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]                                             
taxontable[original_name == "Baetis tricolor Tshernova 1928"      ,`:=` (species = "Labiobaetis tricolor", genus = "Labiobaetis" , family = "Baetidae"       , order = "Ephemeroptera"    , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]                                                                             
taxontable[original_name == "Fagotia esperi (Ferussac 1823)"      ,`:=` (species = "Esperiana esperi"    , genus = "Esperiana"   , family = "Melanopsidae"   , order = NA                 , subclass = NA, class = "Gastropoda", phylum = "Mollusca"  , kingdom = "Animalia")]                                                   
taxontable[original_name == "Gomphus flavipes (Charpentier 1825)" ,`:=` (species = "Gomphus pulchellus"  , genus = "Gomphus"     , family = "Gomphidae"      , order = "Odonata"          , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]                                                   
taxontable[original_name == "Gyraulus crista (Linnaeus 1758)"     ,`:=` (species = "Armiger crista"      , genus = "Armiger"     , family = "Planorbidae"    , order = NA                 , subclass = NA, class = "Gastropoda", phylum = "Mollusca"  , kingdom = "Animalia")]                                                                                                     
taxontable[original_name == "Habrophlebia lauta Eaton 1884"       ,`:=` (species = "Habrophlebia lauta"  , genus = "Habrophlebia", family = "Leptophlebiidae", order = "Ephemeroptera"    , subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]                                                 
taxontable[original_name == "Pisidium casertanum (Poli 1791)"     ,`:=` (species = "Euglesa casertana"   , genus = "Euglesa"     , family = "Sphaeriidae"    , order = "Sphaeriida"       , subclass = NA, class = "Bivalvia"  , phylum = "Mollusca"  , kingdom = "Animalia")]     


# koutajoki -------------------------------------------------------------------------
taxontable[original_name == "Cymatinae",`:=` (species = NA, genus = NA, family = "Corixidae", order = "Hemiptera", subclass = NA, class = "Insecta"   , phylum = "Arthropoda", kingdom = "Animalia")]


# monitoring czech republic ---------------------------------------------------------
fill.taxon.table("Acari, Acarina", NA, NA, NA, NA, NA, "Arachnida", "Arthropoda")

# monitoing dutch -------------------------------------------------------------------
taxontable[original_name == "Adephaga",       `:=` (species = NA, genus = NA           , family = NA                , order = "Coleoptera"    , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Anisoptera",     `:=` (species = NA, genus = NA           , family = NA                , order = "Odonata"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Aquarius",       `:=` (species = NA, genus = "Aquarius"   , family = "Gerridae"        , order = "Hemiptera"     , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Brachycera",     `:=` (species = NA, genus = NA           , family = NA                , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Caridea",        `:=` (species = NA, genus = NA           , family = NA                , order = "Decapoda"      , subclass = NA           , class = "Malacostraca", phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Chironomidae",   `:=` (species = NA, genus = NA           , family = "Chironomidae"    , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Chironomini",    `:=` (species = NA, genus = NA           , family = "Chironomidae"    , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Colymbetinae",   `:=` (species = NA, genus = NA           , family = "Dytiscidae"      , order = "Coleoptera"    , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Cyclorrhapha",   `:=` (species = NA, genus = NA           , family = NA                , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Dugesia",        `:=` (species = NA, genus = "Dugesia"    , family = "Dugesiidae"      , order = "Seriata"       , subclass = NA           , class = "Turbellaria" , phylum = "Platyhelminthes", kingdom = "Animalia")]
taxontable[original_name == "Graptodytes",    `:=` (species = NA, genus = NA           , family = "Dytiscidae"      , order = "Coleoptera"    , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Heteroptera",    `:=` (species = NA, genus = NA           , family = NA                , order = "Hemiptera"     , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Hexatominae",    `:=` (species = NA, genus = NA           , family = "Tipulidae"       , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Hirudinea",      `:=` (species = NA, genus = NA           , family = NA                , order = NA              , subclass = "Hirudinea"  , class = "Clitellata"  , phylum = "Annelida"      , kingdom = "Animalia")]
taxontable[original_name == "Hydroporinae",   `:=` (species = NA, genus = NA           , family = "Dytiscidae"      , order = "Coleoptera"    , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Lepidostoma",    `:=` (species = NA, genus = "Lepidostoma", family = "Lepidostomatidae", order = "Trichoptera"   , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Limonia",        `:=` (species = NA, genus = NA           , family = "Limoniidae"      , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Nais"          , `:=` (species = NA, genus = "Nais"       , family = "Naididae"        , order = "Haplotaxida"   , subclass = NA           , class = "Clitellata"  , phylum = "Annelida"      , kingdom = "Animalia")]
taxontable[original_name == "Normandia"     , `:=` (species = NA, genus = "Normandia"  , family = "Elmidae"         , order = "Coleoptera"    , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Ormosia",        `:=` (species = NA, genus = NA           , family = "Limoniidae"      , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Orthocladiinae", `:=` (species = NA, genus = NA           , family = "Chironomidae"    , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Pentaneurini",   `:=` (species = NA, genus = NA           , family = "Chironomidae"    , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Pericoma"   ,    `:=` (species = NA, genus = "Pericoma"   , family = "Psychodidae"     , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Polinicinae",    `:=` (species = NA, genus = NA           , family = "Naticidae"       , order = NA              , subclass = NA           , class = "Gastropoda"  , phylum = "Mollusca"      , kingdom = "Animalia")]
taxontable[original_name == "Prostigmata",    `:=` (species = NA, genus = NA           , family = NA                , order = "Trombidiformes", subclass = "Acari"      , class = "Arachnida"   , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Rhyacodrilinae", `:=` (species = NA, genus = NA           , family = "Naididae"        , order = "Haplotaxida"   , subclass = "Oligochaeta", class = "Clitellata"  , phylum = "Annelida"      , kingdom = "Animalia")]
taxontable[original_name == "Shineriella",    `:=` (species = NA, genus = NA           , family = "Chironomidae"    , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Stylaria"   ,    `:=` (species = NA, genus = "Stylaria"   , family = "Naididae"        , order = "Haplotaxida"   , subclass = NA           , class = "Clitellata"  , phylum = "Annelida"      , kingdom = "Animalia")]
taxontable[original_name == "Sympetrinae",    `:=` (species = NA, genus = NA           , family = "Libellulidae"    , order = "Odonata"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Syrocax"    ,    `:=` (species = NA, genus = NA           , family = "Psychodidae"     , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Tanypodinae",    `:=` (species = NA, genus = NA           , family = "Chironomidae"    , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Tanytarsini",    `:=` (species = NA, genus = NA           , family = "Chironomidae"    , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Tinearia"   ,    `:=` (species = NA, genus = "Psychoda"   , family = "Psychodidae"     , order = "Diptera"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]
taxontable[original_name == "Zygoptera"  ,    `:=` (species = NA, genus = NA           , family = NA                , order = "Odonata"       , subclass = NA           , class = "Insecta"     , phylum = "Arthropoda"    , kingdom = "Animalia")]

# monitoring Germany ----------------------------------------------------------------
fill.taxon.table("Nemertini", NA, NA, NA, NA, NA, NA, "Nemertea")
fill.taxon.table("Rotatoria", NA, NA, NA, NA, NA, NA, "Rotifera")
fill.taxon.table("Nematoda-Mermithidae", NA,NA, "Mermithidae", "Mermithida", NA, "Adenophorea", "Nematoda")
fill.taxon.table("Telmatoscopini", NA,NA, "Psychodidae", "Diptera", NA, "Insecta", "Arthropoda")
taxontable[phylum == "Artrhopoda", phylum := "Arthropoda"]
taxontable[class == "\tMalacostraca", class := "Malacostraca"]
taxontable[phylum == "Plathyheminthes"]
taxontable[original_name == "Planaria n.d.", c("genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .("Planaria", "Planariidae", "Tricladida", NA, NA, "Platyhelminthes", "Animalia")]
taxontable[original_name == "Acariformes", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(NA, NA, NA, NA, "Acari", "Arachnida", "Arthropoda", "Animalia")]
fill.taxon.table("Lindnerina", NA, "Tipula", "Tipulidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Dugesia spec. GIRARD", NA, "Dugesia", "Dugesiidae", "Tricladida", NA, NA, "Platyhelminthes")

# monitoring greece -----------------------------------------------------------------
fill.taxon.table("Belostomatidae", NA, NA, "Belostomatidae ", "Hemiptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Potamonidae",    NA, NA, "Potamonidae"    , "Decapoda", NA, "	Malacostraca", "Artrhopoda")

# monitoring poland -----------------------------------------------------------------
taxontable[original_name == "Chronomini" , c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(NA, NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda", "Animalia")]
taxontable[original_name == "CIRRIPEDIA", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(NA, NA, NA, NA, NA, "Maxillopoda", "Arthropoda", "Animalia")]
taxontable[original_name == "HETEROPTERA", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(NA, NA, NA, "Hemiptera", NA, "Insecta", "Arthropoda", "Animalia")]                                                                      
taxontable[original_name == "HIRUDINEA"  , c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(NA, NA, NA, NA, "Hirudinea", "Clitellata", "Annelida", "Animalia")]                                                                   
taxontable[original_name == "MYSIDACEA"  , c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(NA, NA, NA, NA, NA, "Malacostraca", "Arthropoda", "Animalia")]                                                              
taxontable[original_name == "PLANIPENNIA", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(NA, NA, NA, "Neuroptera", NA, "Insecta", "Arthropoda", "Animalia")]                                                            
taxontable[original_name == "PLECOPTERA - WIDELNICE" , c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") :=   .(NA, NA, NA, "Plecoptera", NA, "Insecta", "Arthropoda", "Animalia")]                                                                     
taxontable[original_name == "TURBELLARIA" ,            c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") :=   .(NA, NA, NA, NA, NA, "Turbellaria", "Platyhelminthes", "Animalia")] 

# monitoring Portugal ---------------------------------------------------------------

fill.taxon.table("Ferrissidae", NA, NA, "Planorbidae ", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Nemathelmintha",    NA, NA, "Potamonidae"    , "Decapoda", NA, "	Malacostraca", "Artrhopoda")
taxontable <- taxontable[!original_name %in% c("Nemathelmintha","Iptis_final" )]
# monitoring Romania ----------------------------------------------------------------
fill.taxon.table("Bivalve", NA, NA, NA, NA, NA, "Bivalvia", "Mollusca")
fill.taxon.table("Hirudinaea", NA, NA, NA, NA, NA, "Clitellata", "Annelida")
fill.taxon.table("Taeniopterys", NA, "Taeniopteryx", "Taeniopterygidae", "Plecoptera", NA, "Insecta", "Arthropoda")
taxontable <- taxontable[class != "Bacillariophyceae"]


# Monitoring Portugal 2 -------------------------------------------------------------
fill.taxon.table("HIRUDINEA n.d",  NA, NA, NA, NA, "Hirudinea", "Clitellata", "Annelida")
fill.taxon.table("Planaria n.d.",  NA, NA, NA, NA,          NA, "Turbellaria", "Plathyheminthes")
fill.taxon.table("So. Anisoptera", NA, NA, NA, "Odonata",       NA, "Insecta"    , "Arthropoda")


# monitoring Spain ------------------------------------------------------------------
fill.taxon.table("Aciculata"        , NA, NA, NA , "Eunicida", NA, "Polycheta", "Annelida")
fill.taxon.table("Actinedida"      , NA, NA, NA, NA, "Acari", "Arachnida", "Arthropoda")
fill.taxon.table("Archaeogastropoda", NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Basommatophora"   , NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Canalipalpata"    , NA, NA, NA, NA, NA, "Polycheta", "Annelida")
fill.taxon.table("CLADOCERA"        , NA, NA, NA, NA, NA, "Branchiopoda", "Arthropoda")
fill.taxon.table("COPEPODA"         , NA, NA, NA, NA, NA, "Hexanauplia", "Arthropoda")
fill.taxon.table("Ferrissidae"      , NA, NA, "Planorbidae", NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Heterostropha"    , NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Larainae"         , NA, NA, "Elmidae", "Coleoptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Mesogastropoda"   , NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Neotaenioglossa"  , NA, NA, NA, NA,      NA, "Gastropoda", "Mollusca")
fill.taxon.table("Seriata"          , NA, NA, NA, "Seriata", NA, "Turbellaria", "Platyhelminthes")

# monitoring UK  --------------------------------------------------------------------
fill.taxon.table("Coelenterata", NA, NA, NA, NA, NA, NA, "Colenterata")
fill.taxon.table("Limnophila nemoralis", "Dicranophragma nemorale", "Dicranophragma", "Limoniidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Microturbellaria", NA, NA, NA, NA, NA, "Turbellaria", "Platyhelminthes")
fill.taxon.table("Pericoma nubila", "Pericoma nubila", "Pericoma", "Psychodidae", "Diptera", NA, "Insecta", "Arthropoda")

# naiades ---------------------------------------------------------------------------
fill.taxon.table("Agapetus-synagapetus"              , NA                 ,        NA, "Glossosomatidae", "Trichoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Anophelinae"                       , NA                 ,        NA,       "Culicidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Asellus (Asellus) aquaticus"       , "Asellus aquaticus", "Asellus",       "Asellidae",     "Isopoda",  NA, "Malacostraca", "Arthropoda" )
fill.taxon.table("Conchostraca"                      , NA                 ,        NA,                NA,            NA,  NA, "Branchiopoda", "Arthropoda" )
fill.taxon.table("Corynoneurinae"                    , NA                 ,        NA,    "Chironomidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Dasyheleinae"                      , NA                 ,        NA, "Ceratopogonidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Dicosmoecinae"                     , NA                 ,        NA,   "Limnephilidae", "Trichoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Drusini"                           , NA                 ,        NA,   "Limnephilidae", "Trichoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Dysticinae"                        , NA                 ,        NA,      "Dytiscidae",  "Coleoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Eriopterini"                       , NA                 ,        NA,      "Limoniidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Forcypomyiinae"                    , NA                 ,        NA, "Ceratopogonidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Hexatomini"                        , NA                 ,        NA,      "Limoniidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Leptoconopinae"                    , NA                 ,        NA, "Ceratopogonidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Limnebiidae"                       , NA                 ,        NA,     "Hydraenidae",  "Coleoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Limoniini"                         , NA                 ,        NA,      "Limoniidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Pediciini"                         , NA                 ,        NA,      "Limoniidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Pericarides"                       , NA                 ,        NA,                NA,            NA,  NA, "Malacostraca", "Arthropoda" )
fill.taxon.table("Planipennia"                       , NA                 ,        NA,                NA,  "Neuroptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Podonominae"                       , NA                 ,        NA,    "Chironomidae",     "Diptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Prosimuliinae"                     , NA                 ,        NA,      "Simuliidae",     "Diptera",  NA,      "Insecta", "Arthropoda")  
fill.taxon.table("Prosobranchia"                     , NA                 ,        NA,                NA,            NA,  NA,   "Gastropoda", "Mollusca")
fill.taxon.table("Silo-lithax"                       , NA                 ,        NA,        "Goeridae", "Trichoptera",  NA,      "Insecta", "Arthropoda")
fill.taxon.table("Simuliini"                         , NA                 ,        NA,      "Simuliidae",     "Diptera",  NA,      "Insecta", "Arthropoda")  
fill.taxon.table("Stenophylacini-chaetopterygini"    , NA                 ,        NA,   "Limnephilidae", "Trichoptera",  NA,      "Insecta", "Arthropoda" )
fill.taxon.table("Tubificinae avec soies capillaires", NA                 ,        NA,        "Naididae", "Haplotaxida",  NA,   "Clitellata", "Annelida")
fill.taxon.table("Tubificinae sans soies capillaires", NA                 ,        NA,        "Naididae", "Haplotaxida",  NA,   "Clitellata", "Annelida")
fill.taxon.table("Unionacea"                         , NA                 ,        NA,                NA,    "Unionida",  NA,     "Bivalvia", "Mollusca")
fill.taxon.table("Elodea"          , NA, NA, "Scirtidae", "Coleoptera",  NA,      "Insecta", "Arthropoda" )

taxontable[original_name == "Nematoda  Gen.sp..", c("class", "phylum") := .(NA, "Nematoda")]

taxontable[order == "Ansotraca", order := "Anostraca"]
taxontable[order == "Doptera", order := "Diptera"]

# PEPE BARQUIN ----------------------------------------------------------------------
fill.taxon.table("Orthocladinae", NA,NA, "Chironomidae", "Diptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Planipenne"   , NA,NA, NA, "Neuroptera", NA, "Insecta", "Arthropoda")
fill.taxon.table("Sialix"       , NA, "Sialis", "Sialidae", "Megaloptera", NA, "Insecta", "Arthropoda")

# RCS -------------------------------------------------------------------------------
fill.taxon.table("Anostraces",             NA,            NA,               NA,   "Ansotraca", NA, "Branchiopoda", "Arthropoda" )
fill.taxon.table("Copelatinae",            NA,            NA,     "Dytiscidae",  "Coleoptera", NA, "Insecta"   , "Arthropoda")
fill.taxon.table("Nemathelmintha",         NA,            NA,               NA,            NA, NA,           NA,         NA)
fill.taxon.table("Physa stricto-sensu",    NA,       "Physa",       "Physidae",            NA, NA, "Gastropoda", "Mollusca")
fill.taxon.table("Rhyacophila lato-sensu", NA, "Rhyacophila", "Rhyacophilidae", "Trichoptera", NA, "Insecta"   , "Arthropoda")
fill.taxon.table("Nemathelmintha", NA, NA, NA, NA, NA, )

# STAR ------------------------------------------------------------------------------
taxontable[original_name == "Diamesini"      , `:=` (species = NA, genus = NA, family = "Chironomidae"    , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]                                                      
taxontable[original_name == "Hemerodromiinae", `:=` (species = NA, genus = NA, family = "Empididae"       , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]                                                           
taxontable[original_name == "Macropelopiini" , `:=` (species = NA, genus = NA, family = "Chironomidae"    , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]                                                           
taxontable[original_name == "Orthocladiini"  , `:=` (species = NA, genus = NA, family = "Chironomidae"    , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]                                                           
taxontable[original_name == "Procladiini"    , `:=` (species = NA, genus = NA, family = "Chironomidae"    , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]                                                         
taxontable[original_name == "Prodiamesinae"  , `:=` (species = NA, genus = NA, family = "Chironomidae"    , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]                                                           


# WISER -----------------------------------------------------------------------------
taxontable[original_name == "Clinocerinae",`:=` (species = NA, genus = NA, family = "Empididae"    , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Drusinae"    ,`:=` (species = NA, genus = NA, family = "Limnephilidae", order = "Trichoptera", subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Fridericia"    ,`:=` (species = NA, genus = "Fridericia", family = "Enchytraeidae", order = "Enchytraeida", subclass = "Oligochaeta", class = "Clitellata", phylum = "Annelida", kingdom = "Animalia")]
taxontable[original_name == "Melanogaster",`:=` (species = NA, genus = "Melanogaster", family = "Syrphidae"    , order = "Diptera"    , subclass = NA, class = "Insecta", phylum = "Arthropoda", kingdom = "Animalia")]
taxontable[original_name == "Turbellaria" ,`:=` (species = NA, genus = NA, family = NA, order = NA, subclass = NA, class = "Turbellaria", phylum = "Platyhelminthes", kingdom = "Animalia")]




