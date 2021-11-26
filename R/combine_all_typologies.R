# ——————————— #
# PURPOSE: 
# Combine typologies into one data set 
# ——————————— #

# ——————————— #
#date creatd: 21.09.21
#date last modified: 21.09.21
# ——————————— #

pacman::p_load(data.table, sf, dplyr, magrittr)

bgr <- readRDS("E://Arbeit/Data/broad_river_types/with_bgr/2021_09_17_bgr_and_brt.rds")
ill <- readRDS("R/new_typology/brtxife/2021_09_21_brtxife.rds")
fec <- st_read  ("E://Arbeit/Data/Lemm_et_al_21/2021-08-02_least.impacted.gpkg")

bgr2 <- select(bgr, m_zhyd, brt20 = m_btype20c, brt12 = m_btype12, bgr = short_name, brtXbgr = brt_bgr)
ill2 <- select(ill, m_zhyd, illies = NAME, brtXife = brt12_illies)

typo <- left_join(bgr2,
                  ill2)
#- add unique identifier 
typo$id <- 1:nrow(typo)


fec <- st_transform(fec, crs = st_crs(typo))

typo2 = st_join(x = typo,
                y = fec)

# - Some reaches cross multiple catchments? 
typo2 %<>% mutate(least.impacted.numeric = as.numeric(least.impacted))
setDT(typo2)
typo2[, least.impacted.numeric2 := mean(least.impacted.numeric), by = "id"]
typo2 <- unique(typo2, by = "id")
typo2[, least.impacted := ifelse(least.impacted.numeric2 == 1, TRUE, FALSE)]
typo2[, c("least.impacted.numeric", "least.impacted.numeric2") := NULL]
typo2%<>%st_as_sf()

# save to file  ---------------------------------------------------------------------
saveRDS(typo2, "data/all_typologies.rds")
