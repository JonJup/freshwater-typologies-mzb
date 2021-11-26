fill.taxon.table <- function(o,a,b,c,d,e,f,g){
        taxontable[original_name == o,
                   `:=` (
                           species = a, 
                           genus = b, 
                           family = c, 
                           order = d, 
                           subclass = e,
                           class = f,
                           phylum = g,
                           kingdom = "Animalia"
                   )]
}