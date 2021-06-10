# Repository for the publication: Evaluating pan-European freshwater typologies in light of their macroinvertebate communites.

This repository contains the R code for the related manuscript published in ... .

Written by Jonathan F. Jupke, Pepe Barquín, Núria Bonada, Christian K. Feld, Peter Haase, Kaisa-Leena Huttunen, Miguel Cañedo-Argüelles, Timo Muotka, Riku Paavola, Edwin T. H. M. Peeters, Leonard Sandin, Denes Schmera, Philippe Usseglio-Polatera, Ralf B. Schäfer

## Contents overview:
### R/   
**helper/** - auxiliary scripts that are called from the scripts below
**01_combine_data_sets.R** - combine individual macroinvertebrate data sets   
**02_remove_sites_removed_from_river.R** - remove sites that are more than 500 meters away from the next stream segment + assign BRT type to samples   
**03_determine_impact_with_landcover.R** - determined impact category through catchment landcover    
**04_add_more_typologies.R** - add types of other typologies to samples    
**05_taxonomic_resolution_1.R** - determine optimal taxonomic resolution for each taxon/ type (1)   
**06_taxonomic_resolution_2.R** - determine optimal taxonomic resolution for each taxon/ type (2)    
**07_create_sitesXspecies_mzb.R** - create a site X species presence absence table    
**08_derive_typical_assemblages.R** - derinve typical assemblages    
**09_create_biological_typology.R** - create biological typology    
**11_evaluate_clusters.R**   
**13_create_map_of_samplig_sites.R**   - create maps in the publication

### doc/  

### fig/  

 
