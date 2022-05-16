# Purpose: update the taxonomy table which connect original names to various taxonomic levels 
# date created: 22-09-21
# date last modified: 22-09-21

update_taxonomy <- function(TU){
        
        taxontable_new <- data.table(
                original_name = TU,
                species = character(length(TU)),
                genus = character(length(TU)),
                family = character(length(TU)),
                order = character(length(TU)),
                subclass = character(length(TU)),
                class = character(length(TU)),
                phylum = character(length(TU)),
                kingdom = character(length(TU)),
                clean = F
        )
        
        taxontable <- rbindlist(list(taxontable, taxontable_new))
        
        for (i in seq_along(TU)){
                
                #- skip this iteration of the loop if the focal taxon has already been evaluated. 
                if (taxontable[original_name == TU[i], clean]) next()
                
                i.co <- 
                        classification(TU[i], db = "gbif") |> 
                        {\(x) x[[1]]}()
                
                # skip this iteration of the taxon is not found 
                if (is.na(i.co)) next()
                #- assign taxon levels  
                taxontable[original_name == TU[i], species  := ifelse("species"  %in% i.co$rank, i.co$name[which(i.co$rank == "species")], NA)]
                taxontable[original_name == TU[i], genus    := ifelse("genus"    %in% i.co$rank, i.co$name[which(i.co$rank == "genus")], NA)]
                taxontable[original_name == TU[i], family   := ifelse("family"   %in% i.co$rank, i.co$name[which(i.co$rank == "family")], NA)]
                taxontable[original_name == TU[i], order    := ifelse("order"    %in% i.co$rank, i.co$name[which(i.co$rank == "order")], NA)]
                taxontable[original_name == TU[i], subclass := ifelse("subclass" %in% i.co$rank, i.co$name[which(i.co$rank == "subclass")], NA)]
                taxontable[original_name == TU[i], class    := ifelse("class"    %in% i.co$rank, i.co$name[which(i.co$rank == "class")], NA)]
                taxontable[original_name == TU[i], phylum   := ifelse("phylum"   %in% i.co$rank, i.co$name[which(i.co$rank == "phylum")], NA)]
                taxontable[original_name == TU[i], kingdom  := ifelse("kingdom"  %in% i.co$rank, i.co$name[which(i.co$rank == "kingdom")], NA)]
                taxontable[original_name == TU[i], clean := TRUE]
                rm(i.co)
                
        }
        return(taxontable)
}