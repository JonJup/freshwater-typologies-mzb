# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------ macroinvertebrates -------- ###
### ------------ Setup   ------------- ###
# -------------------------------------- #

# --------------- #
# date:         17.03.21
# files out:
#               f_ABplotsTA.rds            | data for plots of A against B   
#               08_typical_assemblages.rds | list with taxa for typical assemblages   
# called by:
#               07_derive_typical_assemblages.R
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#               Derive typical assemblages from indicator_list.rds 
# --------------- #

# load data ---------------------------------------------------------------
ls_mzb = readRDS("data/07_indicator_list.rds")

# read thresholds if missing
if (!"x_ls_thesholds" %in% ls()) {
        source(textConnection(readLines(
                "07_derive_typical_assemblages.R"
        )[c(25:27)]))
}

ls_mzb_old = ls_mzb
dt_mzb_old = rbindlist(ls_mzb_old)
dt_mzb_old$level = rep(c("species", "genus", "fol"), times = c(nrow(ls_mzb_old$spe),nrow(ls_mzb_old$gen), nrow(ls_mzb_old$fol)))
dt_mzb_old[level == "species" & B>=x_ls_thesholds$spe$b & A <= x_ls_thesholds$spe$a, mechanism := "B"]
dt_mzb_old[level == "species" & B<=x_ls_thesholds$spe$b & A >= x_ls_thesholds$spe$a & B>=x_ls_thesholds$spe$b2, mechanism := "A"]
dt_mzb_old[level == "species" & B>=x_ls_thesholds$spe$b & A >= x_ls_thesholds$spe$a, mechanism := "AB"]
dt_mzb_old[level == "genus"   & B>=x_ls_thesholds$gen$b & A <= x_ls_thesholds$gen$a, mechanism := "B"]
dt_mzb_old[level == "genus"   & B<=x_ls_thesholds$gen$b & A >= x_ls_thesholds$gen$a & B>=x_ls_thesholds$gen$b2, mechanism := "A"]
dt_mzb_old[level == "genus"   & B>=x_ls_thesholds$gen$b & A >= x_ls_thesholds$gen$a, mechanism := "AB"]
dt_mzb_old[level == "fol"     & B>=x_ls_thesholds$fol$b & A <= x_ls_thesholds$fol$a, mechanism := "B"]
dt_mzb_old[level == "fol"     & B<=x_ls_thesholds$fol$b & A >= x_ls_thesholds$fol$a & B>=x_ls_thesholds$fol$b2, mechanism := "A"]
dt_mzb_old[level == "fol"     & B>=x_ls_thesholds$fol$b & A >= x_ls_thesholds$fol$a, mechanism := "AB"]

dt_mzb_old %>%
        ggplot(aes(x = B, y = A, shape=level)) +
        geom_point(aes(col = mechanism)) +
        # geom_hline(yintercept = x_ls_thesholds$spe$a) +
        # geom_vline(xintercept = x_ls_thesholds$spe$b) +
        # geom_vline(xintercept = x_ls_thesholds$spe$b2) +
        facet_wrap(.~rt) -> 
        plot_old 
# print(plot_old)

# Deriving typical assemblages ---------------------------------------------------
ls_mzb$spe = ls_mzb$spe[B >= x_ls_thesholds$spe$b | (A >= x_ls_thesholds$spe$a & B >= x_ls_thesholds$spe$b2 )]
ls_mzb$gen = ls_mzb$gen[B >= x_ls_thesholds$gen$b | (A >= x_ls_thesholds$gen$a & B >= x_ls_thesholds$gen$b2 )]
ls_mzb$fol = ls_mzb$fol[B >= x_ls_thesholds$fol$b | (A >= x_ls_thesholds$fol$a & B >= x_ls_thesholds$fol$b2 )] 

ls_mzb$spe[B>=x_ls_thesholds$spe$b & A <= x_ls_thesholds$spe$a, mechanism := "B"]
ls_mzb$spe[B<=x_ls_thesholds$spe$b & A >= x_ls_thesholds$spe$a & B>=x_ls_thesholds$spe$b2, mechanism := "A"]
ls_mzb$spe[B>=x_ls_thesholds$spe$b & A >= x_ls_thesholds$spe$a, mechanism := "AB"]
ls_mzb$gen[B>=x_ls_thesholds$gen$b & A <= x_ls_thesholds$gen$a, mechanism := "B"]
ls_mzb$gen[B<=x_ls_thesholds$gen$b & A >= x_ls_thesholds$gen$a & B>=x_ls_thesholds$gen$b2, mechanism := "A"]
ls_mzb$gen[B>=x_ls_thesholds$gen$b & A >= x_ls_thesholds$gen$a, mechanism := "AB"]
ls_mzb$fol[B>=x_ls_thesholds$fol$b & A <= x_ls_thesholds$fol$a, mechanism := "B"]
ls_mzb$fol[B<=x_ls_thesholds$fol$b & A >= x_ls_thesholds$fol$a & B>=x_ls_thesholds$fol$b2, mechanism := "A"]
ls_mzb$fol[B>=x_ls_thesholds$fol$b & A >= x_ls_thesholds$fol$a, mechanism := "AB"]

dt_mzb <- rbindlist(ls_mzb)

dt_mzb$mechanism %<>% factor
dt_mzb$level = rep(c("species", "genus", "fol"), times = c(nrow(ls_mzb$spe),nrow(ls_mzb$gen), nrow(ls_mzb$fol)))

dt_mzb %>%
        ggplot(aes(x = B, y = A)) +
        geom_point(aes(col = mechanism, shape = level)) +
        # geom_hline(yintercept = x_ls_thesholds$spe$a) +
        # geom_vline(xintercept = x_ls_thesholds$spe$b) +
        facet_wrap(.~rt) -> 
        plot_new


# save to file ------------------------------------------------------------
saveRDS(plot_new, "data/f_ABplotsTA.rds")
saveRDS(dt_mzb, "data/08_typical_assemblages.rds")

