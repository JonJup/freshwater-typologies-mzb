### --------------------------- ###
### --- FD Analysis         --- ### 
### --- Macroinvertebrates  --- ### 
### --- Indicator groups    --- ### 
### --------------------------- ###

# --------------- #
# date:  23.03.21
# files in 
#               -> 
# files out
#                
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#               Compute and visualize functional diversity of indicator assemblages 
# --------------- #

# setup -----------------------------------------------
source("R/setup.R")

# load data -------------------------------------------
data = readRDS("data/16_indicator_taxa_w_traits.rds")

# prepare data ----------------------------------------
names(data)[which(names(data) == "final_taxon")] = "taxon"
data %<>% unique(by = c("taxon", "river_type"))
traits = copy(data) 
traits[, c("family_id","river_type") := NULL]
traits %<>% 
        unique(by="taxon") %>% 
        data.frame()
base::rownames(traits) = traits$taxon
traits %<>% dplyr::select(!taxon)

data2 <-
        data.table(species = data$taxon,
                   site   = data$river_type,
                   value = 1) %>%
        pivot_wider(id_cols = site,
                    names_from = species,
                    values_from = value) %>% 
        dplyr::select(!"site") %>% 
        setnafill(fill = 0) %>% 
        as.matrix()

# FD Analysis --------------------------------------------
gd = gowdis(traits)
fc = functcomp(traits, data2)
db = dbFD(x = gd, a = data2, corr = "cailliez") 

# Trait NMDS  -------------------------------------------------------------
nmds_obj = metaMDS(gd, try=1000)
nmds_obj2 <-
        scores(nmds_obj) %>% 
        as.data.frame() %>% 
        setDT %>% 
        .[, taxon := rownames(scores(nmds_obj))]
join = data[,c("taxon", "river_type")]
nmds_obj3 = join[nmds_obj2, on = "taxon"]
hulls = nmds_obj3 %>% group_by(river_type) %>% dplyr::slice(chull(NMDS1, NMDS2))
ggplot(data = nmds_obj3,
       aes(x = NMDS1, y = NMDS2)) +
        geom_polygon(data = hulls,   
                     alpha = 0.5, 
                     aes(fill = river_type)) +
        geom_point(aes(fill = river_type), 
                   shape = 21) + 
        scale_fill_viridis_d() + 
        facet_grid(.~river_type) + 
        theme(legend.position = "none", 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank())

# save data -------------------------------------------