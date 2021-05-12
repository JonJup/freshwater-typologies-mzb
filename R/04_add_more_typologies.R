# ---------------------------------------------------------- #
### --- Add other typologies to macroinvertebrate data --- ### 
# ---------------------------------------------------------- #

# --------------- #
# date:   
#               04.05.21
# files in:  
#               -> gloric_v10.RDS        | GloRiC Typology spatial river network (vector) 
#               -> Ecoregions.shp        | Polygon Shapefile of Illies Ecoregions 
#               -> BiogeoRegions2016.shp | Polygon Shapefile of EEA Biogeoregions
# files out:  
#               <- 13_class_eval_mzb.rds
# Project: 
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose: 
#               Add the remaining typologies to the macroinvertebrate data.    
# --------------- #

# setup ---------------------------------------------------------------------------------------------------------------------------------------------------
source("R/setup.R")

# LOAD DATA -----------------------------------------------------------------------------------------------------------------------------------------------
## -- TYPOLOGIES 
#sf_ls     = st_read(file.path(dir$ls    , "m_river_fec_broad_type.shp"))
sf_gloric = readRDS(file.path(dir$gloric, "gloric_v10.RDS"))
sf_illies = st_read(file.path(dir$ecoregions, "Ecoregions.shp"))
sf_eea    = st_read(file.path(dir$eea, "BiogeoRegions2016.shp"))
## --  MACROINVERTEBRATES
macro = readRDS("data/03_data_low_impact.rds")

# prepare spatial analysis --------------------------------------------------------------------------------------------------------------------------------
## -- create sites data - i.e. one row per sampling site 
sites = unique(macro, by = "gr_sample_id")
## --  drop columns 
sites = sites[, c("gr_sample_id", "geometry", "brt12","brt20")]
## -- create spatial object (sf package)
sites %<>% st_as_sf()

## -- transform GloRiC and sites to ETRS: Lambert equal area azimuthal  
sf_gloric %<>% st_transform(crs = 3035)
sites     %<>% st_transform(crs = 3035)

## -- check bounding box  
tmap_mode("plot")
tm_shape(sites, bbox = st_bbox(sites)) + tm_dots()

## -- crop GloRiC to data 
crp_gl = st_crop(x = sf_gloric, y = st_bbox(sites))
## -- buffer to 500 distance
buf_gl = st_buffer(x = crp_gl, dist = 500)
## -- intersect buffered rivers with sampling sites
sgbp_gl = st_intersects(x = sites, y = buf_gl); beep()
sgbp_gl = lengths(sgbp_gl) > 0 
sites_gl = sites[sgbp_gl, ]

# add types -----------------------------------------------------------------------------------------------------------------------------------------------
nn_gl = st_nearest_feature(sites_gl, buf_gl)
sites_gl$gl = buf_gl$Kmeans_30[nn_gl]

# reduce to gr_sample_id and classes 
sites_gl.dt = copy(sites_gl) %>% setDT 
sites_gl.dt = sites_gl.dt[, c("gr_sample_id", "gl")] 

## -- add to sites 
sites2 = left_join(sites,
                   sites_gl.dt)
setDT(sites2)

## -- observations per type 
sites2 = sites2[!is.na(gl)]
tgl = table(sites2$gl)
tgl = names(which(tgl < 20))
sites2 = sites2[!gl %in% tgl]
tsl = table(sites2$brt12)
tsl = names(which(tsl < 20))
if (length(tsl != 0)) sites2 = sites2[!brt12 %in% tsl]
tsl = table(sites2$brt20)
tsl = names(which(tsl < 20))
sites2 = sites2[!brt20 %in% tsl]
sites3 = st_as_sf(sites2)

## ------------------------------------ ## 
## -- visual checking of the results -- ## 
## -- GloRiC                         -- ## 
## ------------------------------------ ##
st_write(crp_gl, "data/temp/gl.gpkg")
st_write(sites3, "data/temp/sites.gpkg")
## -> ok 
file.remove(fs::dir_ls("data/temp/"))

## -------------------------------------- ## 

### ------------------ ###
### --- BIOREGIONS --- ### 
### ------------------ ###

sf_illies %<>% st_transform(crs = st_crs(sites))
sf_eea %<>% st_transform(crs = st_crs(sites))
jn_ill = st_join(x = filter(sites, gr_sample_id %in% sites3$gr_sample_id), y = sf_illies) %>% 
        dplyr::select(c("gr_sample_id", "NAME")) %>% 
        st_drop_geometry() %>% 
        setDT
jn_eea = st_join(x = filter(sites, gr_sample_id %in% sites3$gr_sample_id), y = sf_eea) %>% 
        dplyr::select(c("gr_sample_id", "name")) %>% 
        st_drop_geometry() %>% 
        setDT

sites4 = left_join(sites3, 
                   jn_ill)

sites4 %<>% left_join(jn_eea)

names(sites4) = c("gr_sample_id", "brt12", "brt20", "gloric",  "illies", "eea", "geometry")

setDT(sites4)
teea = table(sites4$eea)
teea = names(which(teea < 20))
sites4 = sites4[!eea %in% teea]
till = table(sites4$illies)
till = names(which(till < 20))
sites4 = sites4[!illies %in% till]
tgl = table(sites4$gloric)
tgl = names(which(tgl < 20))
sites4 = sites4[!gloric %in% tgl]
tsl = table(sites4$brt20)
tsl = names(which(tsl < 20))
sites4 = sites4[!brt20 %in% tsl]
tsl = table(sites4$brt12)
tsl = names(which(tsl < 20))
sites4 = sites4[!brt12 %in% tsl]

## -- drop BRT-types from sites4 - they are already in macro 
sites4 %<>% dplyr::select(!c("brt12", "brt20", "geometry"))

## -- subset macroinvertebrate data to sites that were assigned to each typology 
macro2 = macro[gr_sample_id %in% sites4$gr_sample_id]
## -- join with typology data 
macro2 %<>% left_join(sites4,
                     by = "gr_sample_id")
macro2 %<>% dplyr::select(!i.WSO1_ID)
# save to file ------------------------------------------------------------
saveRDS(macro2, "data/04_invertebrates_w_typologies.rds")
