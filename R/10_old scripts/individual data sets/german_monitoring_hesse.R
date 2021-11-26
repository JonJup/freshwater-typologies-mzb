
#TODO MANTEL ABUNDANCE VS PA 
#TODO BETA 
#TODO KAPPA 


# SETUP -----------------------------------------------------------------------------
pacman::p_load(
        ade4,
        cluster,
        # cowplot,
        data.table,
        dplyr,
        # fpc,
        ggplot2,
        #ggExtra,
        #ggsignif,
        indicspecies,
        magrittr,
        # mapview,
        # parallelDist,
        # plotly,
        # purrr,
        # sf,
        # stringr,
        tidyr,
        vegan
        # tmap
)


source("R/helper_functions.R")

# DATA DESCRIPTION ------------------------------------------------------------------

#  —— TYPES 

# ONLY ONE BGR TYPE and Illies 

#  —— TAXONOMIC RESOLUTION 

# LOAD AND PREPARE DATA  ------------------------------------------------------------------------

all_seasons <- c("spring", "summer", "autumn", "winter")
typologies <- c("brt12", "german_type")
data.set = "hesse"

data <- readRDS("data/combined_data/01_2021-08-05_combined_data.rds")
data <- lapply(data, function(x) x[data.set == "German federal monitoring data from Hesse"])
#seasons <- which(all_seasons %in% unique(rbindlist(data)$season))
seasons <- 1:3
da <- lapply(seasons, function(x) data[[x]][!is.na(german_type)])


# PREPARE SUBSETS -------------------------------------------------------------------

#- create subset of least impacted sites  
li <- lapply(seasons, function(x) data[[x]][fec.least.impacted == TRUE])
#- create subset of sites that are least impacted and removed from regional borders 
pu <- lapply(seasons, function(x) data[[x]][illies_distance >= 25000 & bgr_distance >= 25000])

#- how many types per typology? 
lapply(1:3, function(x) uniqueN(pu[[x]]$bgr))
lapply(1:3, function(x) uniqueN(pu[[x]]$illies))
lapply(1:3, function(x) uniqueN(pu[[x]]$brt12))
lapply(1:3, function(x) uniqueN(pu[[x]]$german_typ))


das2 <- prep1(data = da, taxon = "species", seasons = c(1:3)) 
lis2 <- prep1(data = li, taxon = "species", seasons = c(1:3)) 
pus2 <- prep1(data = pu, taxon = "species", seasons = c(1:3)) 
dag2 <- prep1(data = da, taxon = "genus"  , seasons = c(1:3)) 
lig2 <- prep1(data = li, taxon = "genus"  , seasons = c(1:3)) 
pug2 <- prep1(data = pu, taxon = "genus"  , seasons = c(1:3)) 
daf2 <- prep1(data = da, taxon = "family" , seasons = c(1:3)) 
lif2 <- prep1(data = li, taxon = "family" , seasons = c(1:3)) 
puf2 <- prep1(data = pu, taxon = "family" , seasons = c(1:3)) 

das3 <- prep2(das2)
lis3 <- prep2(lis2)
pus3 <- prep2(pus2)
dag3 <- prep2(dag2)
lig3 <- prep2(lig2)
pug3 <- prep2(pug2)
daf3 <- prep2(daf2)
lif3 <- prep2(lif2)
puf3 <- prep2(puf2)

# DISTANCE MATRIX  ------------------------------------------------------------------

das.dist <- das3 |> prep3()
lis.dist <- lis3 |> prep3()
pus.dist <- pus3 |> prep3()
dag.dist <- dag3 |> prep3()
lig.dist <- lig3 |> prep3()
pug.dist <- pug3 |> prep3()
daf.dist <- daf3 |> prep3()
lif.dist <- lif3 |> prep3()
puf.dist <- puf3 |> prep3()


# BETA CLUSTERING ------------------------------------------------------------------------------
# das2 <- add.beta.clust("das")
# lis2 <- add.beta.clust("lis")
# pus2 <- add.beta.clust("pus")
# dag2 <- add.beta.clust("dag")
# lig2 <- add.beta.clust("lig")
# pug2 <- add.beta.clust("pug")
# daf2 <- add.beta.clust("daf")
# lif2 <- add.beta.clust("lif")
# puf2 <- add.beta.clust("puf")


#- update typologies 
#typologies <- append (typologies, paste0(typologies, "_beta"))
# MANTEL ----------------------------------------------------------------------------
#- species vs. genus 
m1 <- lapply(1:3, function(x) mantel.rtest(das.dist[[x]], dag.dist[[x]]))
m2 <- lapply(1:3, function(x) mantel.rtest(das.dist[[x]], daf.dist[[x]]))

# IVS -------------------------------------------------------------------------------
ivsas <- ivs(data2 = das2, data3 = das3, season.data = 1:3, typologies = typologies, add = "all observations")
ivsag <- ivs(data2 = dag2, data3 = dag3, season.data = 1:3, typologies = typologies, add = "all observations")
ivsaf <- ivs(data2 = daf2, data3 = daf3, season.data = 1:3, typologies = typologies, add = "all observations")
ivsls <- ivs(data2 = lis2, data3 = lis3, season.data = 1:3, typologies = typologies, add = "least impacted")
ivslg <- ivs(data2 = lig2, data3 = lig3, season.data = 1:3, typologies = typologies, add = "least impacted")
ivslf <- ivs(data2 = lif2, data3 = lif3, season.data = 1:3, typologies = typologies, add = "least impacted")
ivsps <- ivs(data2 = pus2, data3 = pus3, season.data = 1:3, typologies = typologies, add = "purified")
ivspg <- ivs(data2 = pug2, data3 = pug3, season.data = 1:3, typologies = typologies, add = "purified")
ivspf <- ivs(data2 = puf2, data3 = puf3, season.data = 1:3, typologies = typologies, add = "purified")

ivs_species <- rbindlist(list(ivsas, ivsls, ivsps))
ivs_genus   <- rbindlist(list(ivsag, ivslg, ivspg))
ivs_family  <- rbindlist(list(ivsaf, ivslf, ivspf))
ivs.plot1(ivs_species, taxon = "species")
ivs.plot1(ivs_genus, taxon = "genus")
ivs.plot1(ivs_family, taxon = "family")

# CLASSIFICATION STRENGTH -----------------------------------------------------------
csas <- class.strength(data2 = das2, data4 = das.dist, season.data = 1:3, add = "all observations")
csag <- class.strength(data2 = dag2, data4 = dag.dist, season.data = 1:3, add = "all observations")
csaf <- class.strength(data2 = daf2, data4 = daf.dist, season.data = 1:3, add = "all observations")
csls <- class.strength(data2 = lis2, data4 = lis.dist, season.data = 1:3, add = "least impacted")
cslg <- class.strength(data2 = lig2, data4 = lig.dist, season.data = 1:3, add = "least impacted")
cslf <- class.strength(data2 = lif2, data4 = lif.dist, season.data = 1:3, add = "least impacted")
csps <- class.strength(data2 = pus2, data4 = pus.dist, season.data = 1:3, add = "purified")
cspg <- class.strength(data2 = pug2, data4 = pug.dist, season.data = 1:3, add = "purified")
cspf <- class.strength(data2 = puf2, data4 = puf.dist, season.data = 1:3, add = "purified")

cs_species <- rbindlist(list(csas, csls, csps))
cs_genus   <- rbindlist(list(csag, cslg, cspg))
cs_family  <- rbindlist(list(csaf, cslf, cspf))
class.strength.plot1(cs_species, taxon = "species")
class.strength.plot1(cs_genus, taxon = "genus")
class.strength.plot1(cs_family, taxon = "family")
class.strength.plot2(cs_species, taxon = "species")
class.strength.plot2(cs_genus, taxon = "genus")
class.strength.plot2(cs_family, taxon = "family")
class.strength.plot3(cs_species, taxon = "species")
class.strength.plot3(cs_genus, taxon = "genus")
class.strength.plot3(cs_family, taxon = "family")

# SILHOUETTE ------------------------------------------------------------------------
sias <- wraper_silhouette(data2 = das2, distance = das.dist, add = "all observations")
sias <- wraper_silhouette(data2 = das2, distance = das.dist, add = "all observations")
siag <- wraper_silhouette(data2 = dag2, distance = dag.dist, add = "all observations")
siaf <- wraper_silhouette(data2 = daf2, distance = daf.dist, add = "all observations")
sils <- wraper_silhouette(data2 = lis2, distance = lis.dist, add = "least impacted")
silg <- wraper_silhouette(data2 = lig2, distance = lig.dist, add = "least impacted")
silf <- wraper_silhouette(data2 = lif2, distance = lif.dist, add = "least impacted")
sips <- wraper_silhouette(data2 = pus2, distance = pus.dist, add = "purified")
sipg <- wraper_silhouette(data2 = pug2, distance = pug.dist, add = "purified")
sipf <- wraper_silhouette(data2 = puf2, distance = puf.dist, add = "purified")

si_species <- rbindlist(list(sias, sils, sips))
si_genus   <- rbindlist(list(siag, silg, sipg))
si_family  <- rbindlist(list(siaf, silf, sipf))

silhouette.plot1(x = si_species, taxon = "species")
silhouette.plot1(x = si_genus, taxon = "genus")
silhouette.plot1(x = si_family, taxon = "family")

# ANOSIM ----------------------------------------------------------------------------
anas <- wraper_anosim(grouping = das2, distance = das.dist, permutations = 99, add = "all observations")
anas <- wraper_anosim(grouping = das2, distance = das.dist, permutations = 99, add = "all observations")
anag <- wraper_anosim(grouping = dag2, distance = dag.dist, permutations = 99, add = "all observations")
anaf <- wraper_anosim(grouping = daf2, distance = daf.dist, permutations = 99, add = "all observations")
anls <- wraper_anosim(grouping = lis2, distance = lis.dist, permutations = 99, add = "least impacted")
anlg <- wraper_anosim(grouping = lig2, distance = lig.dist, permutations = 99, add = "least impacted")
anlf <- wraper_anosim(grouping = lif2, distance = lif.dist, permutations = 99, add = "least impacted")
anps <- wraper_anosim(grouping = pus2, distance = pus.dist, permutations = 99, add = "purified")
anpg <- wraper_anosim(grouping = pug2, distance = pug.dist, permutations = 99, add = "purified")
anpf <- wraper_anosim(grouping = puf2, distance = puf.dist, permutations = 99, add = "purified")

an_species <- rbindlist(list(anas, anls, anps))
an_genus   <- rbindlist(list(anag, anlg, anpg))
an_family  <- rbindlist(list(anaf, anlf, anpf))

anosim.plot1(x = an_species, taxon = "species")
anosim.plot1(x = an_genus, taxon = "genus")
anosim.plot1(x = an_family, taxon = "family")

# INDICATOR SPECIES & TYPICAL ASSEMBLAGES-----------------------------------------------------------------
#- compute A, B, IndVal.g and p.value 
isas <- indicator_analysis(data3 = das3, data2 = das2, B.lvl = 0.33, add = "all observations")
isag <- indicator_analysis(data2 = dag2, data3 = dag3, B.lvl = 0.50, add = "all observations")
isaf <- indicator_analysis(data2 = daf2, data3 = daf3, B.lvl = 0.66, add = "all observations")
isls <- indicator_analysis(data2 = lis2, data3 = lis3, B.lvl = 0.33, add = "least impacted")
islg <- indicator_analysis(data2 = lig2, data3 = lig3, B.lvl = 0.50, add = "least impacted")
islf <- indicator_analysis(data2 = lif2, data3 = lif3, B.lvl = 0.66, add = "least impacted")
isps <- indicator_analysis(data2 = pus2, data3 = pus3, B.lvl = 0.33, add = "purified")
ispg <- indicator_analysis(data2 = pug2, data3 = pug3, B.lvl = 0.50, add = "purified")
ispf <- indicator_analysis(data2 = puf2, data3 = puf3, B.lvl = 0.66, add = "purified")

jac.spe <- rbindlist(list(jaccard_extract(isas),jaccard_extract(isls),jaccard_extract(isps)))
jac.gen <- rbindlist(list(jaccard_extract(isag),jaccard_extract(islg),jaccard_extract(ispg)))
jac.fam <- rbindlist(list(jaccard_extract(isaf),jaccard_extract(islf),jaccard_extract(ispf)))
mid.spe <- rbindlist(list(mid_extract(isas),mid_extract(isls),mid_extract(isps)))
mid.gen <- rbindlist(list(mid_extract(isag),mid_extract(islg),mid_extract(ispg)))
mid.fam <- rbindlist(list(mid_extract(isaf),mid_extract(islf),mid_extract(ispf)))

jaccard_plot(jac.spe, "species")
jaccard_plot(jac.gen, "genus")
jaccard_plot(jac.fam, "family")
mid_plot(mid.spe, "species")
mid_plot(mid.gen, "genus")
mid_plot(mid.fam, "family")

#  ---------------------------------------------------------------




# BETA CLUSTERING -------------------------------------------------------------------

# KAPPA TEST ------------------------------------------------------------------------






