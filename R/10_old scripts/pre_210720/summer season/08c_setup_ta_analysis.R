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


ls_mzb4 = ls_mzb3
# dt_mzb4 = rbindlist(ls_mzb4)
# dt_mzb4$level = rep(c("species", "genus", "fol"), times = c(nrow(ls_mzb4$spe),nrow(ls_mzb4$gen), nrow(ls_mzb4$fol)))
# dt_mzb4[level == "species" & B>= thesholds$spe$b & A <= thesholds$spe$a, mechanism := "B"]
# dt_mzb4[level == "species" & B<= thesholds$spe$b & A >= thesholds$spe$a & B>= thesholds$spe$b2, mechanism := "A"]
# dt_mzb4[level == "species" & B>= thesholds$spe$b & A >= thesholds$spe$a, mechanism := "AB"]
# dt_mzb4[level == "genus"   & B>= thesholds$gen$b & A <= thesholds$gen$a, mechanism := "B"]
# dt_mzb4[level == "genus"   & B<= thesholds$gen$b & A >= thesholds$gen$a & B>= thesholds$gen$b2, mechanism := "A"]
# dt_mzb4[level == "genus"   & B>= thesholds$gen$b & A >= thesholds$gen$a, mechanism := "AB"]
# dt_mzb4[level == "fol"     & B>= thesholds$fol$b & A <= thesholds$fol$a, mechanism := "B"]
# dt_mzb4[level == "fol"     & B<= thesholds$fol$b & A >= thesholds$fol$a & B>= thesholds$fol$b2, mechanism := "A"]
# dt_mzb4[level == "fol"     & B>= thesholds$fol$b & A >= thesholds$fol$a, mechanism := "AB"]
# 
# dt_mzb4 %>%
#         ggplot(aes(x = B, y = A, shape=level)) +
#         geom_point(aes(col = mechanism)) +
#         # geom_hline(yintercept = threshold$spe$a) +
#         # geom_vline(xintercept = threshold$spe$b) +
#         # geom_vline(xintercept = threshold$spe$b2) +
#         facet_wrap(.~rt) -> 
#         plot_old 
# # print(plot_old)
# 
# rm(ls_mzb4)

# Deriving typical assemblages ---------------------------------------------------
ls_mzb3$spe = ls_mzb3$spe[B >= thresholds$spe$b | (A >= thresholds$spe$a & B >= thresholds$spe$b2 )]
ls_mzb3$gen = ls_mzb3$gen[B >= thresholds$gen$b | (A >= thresholds$gen$a & B >= thresholds$gen$b2 )]
ls_mzb3$fol = ls_mzb3$fol[B >= thresholds$fol$b | (A >= thresholds$fol$a & B >= thresholds$fol$b2 )] 

ls_mzb3$spe[B>=thresholds$spe$b & A <= thresholds$spe$a, mechanism := "B"]
ls_mzb3$spe[B<=thresholds$spe$b & A >= thresholds$spe$a & B>=thresholds$spe$b2, mechanism := "A"]
ls_mzb3$spe[B>=thresholds$spe$b & A >= thresholds$spe$a, mechanism := "AB"]
ls_mzb3$gen[B>=thresholds$gen$b & A <= thresholds$gen$a, mechanism := "B"]
ls_mzb3$gen[B<=thresholds$gen$b & A >= thresholds$gen$a & B>=thresholds$gen$b2, mechanism := "A"]
ls_mzb3$gen[B>=thresholds$gen$b & A >= thresholds$gen$a, mechanism := "AB"]
ls_mzb3$fol[B>=thresholds$fol$b & A <= thresholds$fol$a, mechanism := "B"]
ls_mzb3$fol[B<=thresholds$fol$b & A >= thresholds$fol$a & B>=thresholds$fol$b2, mechanism := "A"]
ls_mzb3$fol[B>=thresholds$fol$b & A >= thresholds$fol$a, mechanism := "AB"]

dt_mzb <- rbindlist(ls_mzb3)

dt_mzb$mechanism %<>% factor
dt_mzb$level = rep(c("species", "genus", "fol"), times = c(nrow(ls_mzb3$spe),nrow(ls_mzb3$gen), nrow(ls_mzb3$fol)))

# dt_mzb %>%
#         ggplot(aes(x = B, y = A)) +
#         geom_point(aes(col = mechanism, shape = level)) +
#         # geom_hline(yintercept = threshold$spe$a) +
#         # geom_vline(xintercept = threshold$spe$b) +
#         facet_wrap(.~rt) -> 
#         plot_new


# save to file ------------------------------------------------------------
save_name = paste0("data/summer season/08_typical_assemblages_",choose_typology,".rds")
#saveRDS(plot_new, "data/f_ABplotsTA.rds")
saveRDS(dt_mzb, save_name)

