# ----------------------------------------------------- #
### --- Macroinvertebrates -------------------------- ### 
### --- Seasonal Typical Assemblages   -------------- ### 
### --- Explore stream types            ------------- ###
# ----------------------------------------------------- #


# --------------- #
# date:  
#               17.03.21
# called by: 
#               08_seasonal_typical_assemblages.R
# Project: 
#               Evaluating European Broad River Types for Macroinvertebrates
# Purpose: 
#               Create seasonal typical assemblages
# --------------- #

called = TRUE 


# print plots -------------------------------------------------------------

# This does not happen when called by other scirpts

if (!called){
        for (type in unique(dt_data$rt)){
                ls_plot = prepare_plot(type)
                map     = plot_list(ls_plot)
                tmap_save(map, filename = file.path(dir$plt_sm, paste0(type, ".jpeg")))        
        }
}

# subset data  ------------------------------------------------------------

# drop river types that can not be used 
dt_data = dt_data[! rt %in% paste0("RT", c(1,9))]

# RT2 
new2 = subset_with_sites(sites = c(
                "site_15952_date_04634_mzb_Landau",
                "site_11826_date_03942_MZB_edwin_peters",
                "site_10343_date_03770_mzb_Landau",
                "site_17767_date_04902_mzb_Landau"
        ), type = "RT2"
)
# RT3 
new3 = subset_with_sites(sites = c(
        "site_15834_date_05860_mzb_Landau",
        "site_06776_date_03670_MZB_edwin_peters",
        "site_11770_date_05600_mzb_Landau",
        "site_15575_date_05328_mzb_Landau"
), type = "RT3"
)
new3 = new3[season != "winter"]
# RT4_5
new5 = dt_data[rt == "RT4_5"]
new5 = new5[season != "winter"]

# RT14_15_16
new16 = subset_with_sites(sites = c(
        "site_01619_date_00846_mzb_Naiades",
        "site_01192_date_01183_mzb_Naiades",
        "site_00823_date_00956_mzb_Naiades",
        "site_00719_date_01191_mzb_Naiades"
), type = "RT14_15_16"
)
new16 = new16[season != "spring"]
# drop river types that are replaced by subsets 
dt_data = dt_data[!rt %in% paste0("RT", c("2", "3", "4_5", "14_15_16"))]
dt_data = rbindlist(list(dt_data,
                         new2,
                         new3,
                         new5,
                         new16),
                    use.names = T)
