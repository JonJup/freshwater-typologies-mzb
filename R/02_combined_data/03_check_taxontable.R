# ————————————————————— # 
# ———— Taxon Test ————  #
# ————————————————————— # 

# --------------- #

# --------------- #

# Setup -------------------------------------------------------------------
pacman::p_load(
        data.table,
        magrittr,
        dplyr,
        sf,
        jjmisc
)

# data IO  ----------------------------------------------------------------
set_all    <- readRDS("data/02_combined_data/03_2021-10-06_core_taxa_data_aggregated.rds")
taxontable <- readRDS("data/01_original_data/2021-10-06_taxontable.rds")

# control --------------------------------------------------------------------------
set_all <- rbindlist(set_all)
us <- unique(set_all$species)
for (i in us){
        id <- set_all[species == i, unique(genus)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
    
        id <- set_all[species == i, unique(family)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        #if (i %in% c("Dugesiidae")) next()
        id <- set_all[species == i, unique(order)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        id <- set_all[species == i, unique(subclass)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        id <- set_all[species == i, unique(class)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        id <- set_all[species == i, unique(phylum)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
us <- unique(set_all$genus)
for (i in us){
        id <- set_all[genus == i, unique(family)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        id <- set_all[genus == i, unique(order)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        id <- set_all[genus == i, unique(subclass)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        id <- set_all[genus == i, unique(class)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}

for (i in us){
        id <- set_all[genus == i, unique(phylum)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
us <- unique(set_all$family)

for (i in us){
        id <- set_all[family == i, unique(order)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        id <- set_all[family == i, unique(subclass)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        
        id <- set_all[family == i, unique(class)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
for (i in us){
        id <- set_all[family == i, unique(phylum)]
        if (uniqueN(id) > 1){
                print(i)
                break()
        }
}
# FIXES -----------------------------------------------------------------------------

# taxontable 
taxontable[species == "", species := NA]
taxontable[genus == "", genus := NA]
taxontable[original_name == "Anisus sp.", c("genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .("Anisus", "Planorbidae", NA, NA, "Gastropoda", "Mollusca", "Animalia")]
taxontable[genus == "Physella",  c("family", "order", "class", "phylum") := .("Physidae", NA, "Gastropoda", "Mollusca") ]
taxontable[original_name == "Physa lato-sensu", c("species","genus") := .(NA, "Physa")]
taxontable[genus == "Hydracarina", class := "Arachnida"]
taxontable[family == "Stratiomyiidae" , c("family", "order", "class", "phylum") := .("Stratiomyidae", "Diptera", "Insecta", "Arthropoda")]
taxontable[genus == "Anisus", c("family", "order", "class", "phylum") :=  .("Planorbidae", NA, "Gastropoda", "Mollusca")]
taxontable[genus == "Dugesia", c("family", "order", "class") := .("Dugesiidae", "Tricladida", "NA")]
taxontable[family == "Choronomidae", family := "Chironomidae"]
taxontable[family == "Spongillidae", order := "Spongillida"]
taxontable[order %in% c("Trombidiformes", "Enchytraeida", "Haplotaxida", "Lumbriculida", "Arhynchobdellida"),subclass := NA]
taxontable[order %in% c("Trombidiformes", "Araneae"), class := "Arachnida"]
taxontable[order %in% c("Tricladida"), class := NA]
taxontable[original_name == "Anisus calculiformis", species := "Anisus calculiformis"]
taxontable[original_name == "Asellus meridianus", species := "Asellus meridionalis"]
taxontable[original_name == "Boreoheptagyia punctulata", species := "Boreoheptagyia punctulata"]
taxontable[original_name == "Athripsodes c.f. albifrons", species := "Athripsodes albifrons"]
taxontable[original_name == "Cecidomyiidae genus", c("genus", "family", "order") := .(NA, "Cecidomyiidae", "Diptera")]
taxontable[original_name == "Chaetopteryx fusca/villosa", genus := "Chaetopteryx"]
taxontable[original_name == "Torleya nazarita", species := "Torleya nazarita"]
taxontable[original_name == "Vejdovskiella intermedia", species := "Vejdovskyella intermedia"]
taxontable[original_name == "Tripodura scalaenum", species := "Polypedilum scalaenum"]
taxontable[original_name == "Tinodes c.f. unicolor", species := "Tinodes unicolor"]
taxontable[original_name %in% c("Ablabesmyia monilis / phatta", "Ablabesmyia monilis/phatta", 
                                "Agrypnia obsoleta/varia", "Amphinemura standfussi/sulcicollis", 
                                "Anabolia furcata x nervosa", "Anabolia furcata/nervosa"), species := NA]
taxontable[original_name %in% c("Capnia/Leuctra"), genus := NA]
taxontable[original_name == "Dikerogammarus haemob.", species := "Dikerogammarus haemobaphes"]
taxontable <- taxontable[original_name != "Code gelÃ© 1999 (nematomorphes)"]
taxontable[original_name == "Heteroptera Fam.", c("genus", "family") := .(NA,NA)]
taxontable[original_name == "Heteroptere", c("genus", "family", "order") := .(NA,NA, "Hemiptera")]
taxontable[original_name == "Hydropsyche pelliculada", species := "Hydropsyche pelliculada"]
taxontable[original_name == "Leptophlebia cincta", c("species", "genus") := .("Paraleptophlebia cincta", "Paraleptophlebia")]
taxontable[original_name == "Leptophlebia submarginata", c("species", "genus") := .("Paraleptophlebia submarginata", "Paraleptophlebia")]
taxontable[original_name == "Leptophlebia werneri", c("species", "genus") := .("Paraleptophlebia werneri", "Paraleptophlebia")]
taxontable[original_name == "Lymnea stagnalis", c("species", "genus") := .("Lymnaea stagnalis", "Lymnaea")]
taxontable[original_name == "Micropterna lateralis/sequax", c("genus") := .("Stenophylax")]
taxontable[original_name == "Nebrioporus depressus elegans", c("species") := NA]
taxontable[original_name == "Nebrioporus depressus/elegans", c("genus", "family") := .("Nebrioporus", "Dytiscidae")]
taxontable[original_name == "Nematocera", c("genus", "family") := .(NA,NA)]
taxontable[original_name == "Physiidae", c("family", "order", "class", "phylum") := .("Physidae", NA, "Gastropoda", "Mollusca")]
taxontable[original_name %in% c("Rhyacophila - Hyperrhyacophila", "Rhyacophila - Hyporhyacophila"), genus := "Rhyacophila"]
taxontable[original_name %in% c("Simulium (Nevermannia)"), c("genus", "family", "order") := .("Simulium", "Simuliidae", "Diptera")]

taxontable <- fill_taxon_table(o = "Tipuloidea", g = NA, f = NA, or = "Diptera")
taxontable <- fill_taxon_table(o = "Cladocera", g = NA, f = NA, or = "Diplostraca", c = "Branchiopoda")
taxontable <- fill_taxon_table(o = "Pachygaster", f = "Stratiomyidae", or = "Diptera")
taxontable <- fill_taxon_table(o = "HYDRACARINA", c = "Arachnida")
taxontable <- fill_taxon_table(o = "Acari", sc = "Acari")
taxontable <- fill_taxon_table(o = "Acari, Acarina", sc = "Acari")
taxontable <- fill_taxon_table(o = "Agrion", g = "Calopteryx", f = "Calopterygidae")



#- visual check 
#- set_all

#- focus taxontable on core taxa 
taxontable_focus = taxontable[original_name %in% set_all$original_name]
View(taxontable_focus)




# update set_all --------------------------------------------------------------------
set_all %<>% select(!species:kingdom)
set_all <- taxontable[set_all, on = "original_name"]

set_all2 <-
        list(spring = set_all[season == "spring" | is.na(season)],
             summer = set_all[season == "summer" | is.na(season)],
             autumn = set_all[season == "autumn" | is.na(season)])


addtotu <- data.table(original_name = c("Gasteropoda", "Stratiomidae"), 
                      species = NA, 
                      genus = NA, 
                      family = c(NA, "Stratiomyidae"),
                      order = c(NA, "Diptera"),
                      subclass = NA,
                      class = c("Gastropoda", "Insecta"),
                      phylum = c("Mollusca", "Arthropoda"),
                      kingdom = "Animalia",
                      clean = TRUE
)

taxontable <- rbindlist(list(taxontable, addtotu))
taxontable[original_name == "Amphinemurinae", phylum := "Arthropoda"]
taxontable[class == "Polycheta", class := "Polychaeta"]
taxontable[original_name == "Oligochaeta varia", c("species", "genus", "family", "order", "subclass") := NA]
taxontable[original_name == "Oligochaeta varia", c("class", "phylum") := .("Clitellata", "Annelida")]

taxontable[original_name == "Oligochaeta"]

# save to file ----------------------------------------------------------------------
saveRDS(taxontable, paste0("data/01_original_data/",Sys.Date(),"_taxontable.rds"))
saveRDS(set_all2, paste0("data/combined_data/04_",Sys.Date(),"_core_taxa_data_checked.rds"))
