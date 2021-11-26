# —————————————————————————————————— #
# ——— Macroinvertebrate Taxonomy ——— # 
# —————————————————————————————————— #

# ———————————————————————————————————
# date: 
#       13.07.21
# files in: 
#       -> 
# files out:
#       <-  
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Harmonize macroinvertebrate data 
# ————————————————


# accept function -------------------------------------------------------------------

accept <- function(taxon, level){
        if (level == "genus"){
                data2[taxon == taxon, genus := taxon]
        } else if (level == "family"){
                data2[taxon == taxon, family := taxon]
        }
        
}


# SPECIES ---------------------------------------------------------------------------


# GENUS -----------------------------------------------------------------------------
genera_accept <- c("Ablabesmyia", "Abra")

accept(genera_accept, "genus")

# FAMILY ----------------------------------------------------------------------------

data2[genus %in% c("Ablabesmyia"), 
      family := "Chironomidae"]

# ORDER -----------------------------------------------------------------------------

data2[family %in% c("Chironomidae"), 
      order := "Diptera"]

# SUBCLASS --------------------------------------------------------------------------


# CLASS -----------------------------------------------------------------------------

data2[order %in% c("Diptera"), 
      class := "Insecta"]

# Phylum -----------------------------------------------------------------------------

data2[class %in% c("Insecta"), 
      phylum := "Arthropoda"]

# kingdom -----------------------------------------------------------------------------

data2[phylum %in% c("Arthropoda"), 
      kingdom := "Animalia"]
