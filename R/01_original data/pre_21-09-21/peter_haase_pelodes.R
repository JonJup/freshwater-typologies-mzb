# ——————————————————————————————— #
# ——— Pelodes for Peter Haase ——— # 
# ——————————————————————————————— #

# ———————————————————————————————————
# date: 
#       21.07.21
# files in: 
# files out:
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates
# Purpose:
#       Prepare data from Peter Haase for PELODES evaluation. 
# ————————————————



# SETUP -----------------------------------------------------------------------------
pacman::p_load(readxl, dplyr, tidyr, stringr, data.table)

# LOAD DATA -------------------------------------------------------------------------
samples1   <- read_excel("data/original data/peter_haase/raw_data/HE_MZB.xlsx")
samples2   <- read_excel("data/original data/peter_haase/raw_data/HE_MZB2.xlsx", sheet = 2)
sites1     <- read_excel("data/original data/peter_haase/raw_data/HE_STAMM.xlsx") 
sites2     <- read_excel("data/original data/peter_haase/raw_data/HE_STAMM2.xls")

# SAMPLES1  --------------------------------------------------------------------------

#- 2430 samples 
uniqueN(samples1$ID_PN)

samples2 <- 
        samples1 |> 
        select(ID_PN,
               "ID art filter",
               "Taxon", 
               IZpqm) |> 
        rename(taxon = "ID art filter",
               abundance = IZpqm) |> 
        mutate(ID_PN = as.character(ID_PN)) |> 
        setDT()
        

sites1 <-
        samples1 |>
        select(ID_PN, Typ) |>
        mutate(
                Typ =
                        case_when(
                                str_detect(Typ, "Typ") ~ Typ,
                                Typ == "5"   ~ "Typ 05",
                                Typ == "5.1" ~ "Typ 05.1",
                                Typ == "6"   ~ "Typ 06",
                                Typ == "7"   ~ "Typ 07",
                                Typ == "9"   ~ "Typ 09",
                                Typ == "9.2" ~ "Typ 09.2",
                                Typ == "10"  ~ "Typ 10",
                                Typ == "19"  ~ "Typ 19"
                        )
        ) |>
        unique(by = "ID_PN") 

unique(sites1$Typ)

#- we have 2430 sampling events  
uniqueN(sites1$ID_PN)
site.group.beg <- seq(from = 1, to = 2430, by = 500)
site.group.end <- site.group.beg - 1
site.group.end <- site.group.end[-1]
site.group.end[length(site.group.end) + 1]  <- 2430

sites.list   <- lapply(seq_along(site.group.beg), function(x) sites1[site.group.beg[x]:site.group.end[x], ])
samples.list <- lapply(seq_along(site.group.beg), function(x) samples2[ID_PN %in% sites.list[[x]]$ID_PN] )
samples.list <- lapply(samples.list, pivot_wider, names_from = "ID_PN", values_fill = 0, values_from = "abundance")
#- drop taxa that do not occur in subset
samples.list <- lapply(seq_along(samples.list), function(x) samples.list[[x]][which(rowSums(samples.list[[x]][,-c(1,2)]) > 0), ])


for (i in seq_along(samples.list)){
        xlsx::write.xlsx(samples.list[[i]],    paste0("data/original data/peter_haase/auxilliary/perlodes/11_",i, "_", Sys.Date(),"perlodes_taxa1.xlsx"))
        xlsx::write.xlsx(sites.list[[i]],      paste0("data/original data/peter_haase/auxilliary/perlodes/11_",i, "_", Sys.Date(),"perlodes_taxa2.xlsx"))
}


# SAMPLES2 --------------------------------------------------------------------------
#- 619 samples 
uniqueN(samples2$`Prodenahme-ID`)

samples3 <-
        samples2 |>
        select("Prodenahme-ID",
               "ID Art filter",
               "Tax",
               izqm) |>
        rename(taxon = "ID Art filter",
               abundance = izqm,
               ID_PN = "Prodenahme-ID") |>
        mutate(ID_PN = as.character(ID_PN)) |> 
        setDT()
        
        
names(samples3)[1] <- "ID_PN"
names(samples2)[1] <- "ID_PN"
names(sites2)[2] <- "ID_PN"

unique(sites2$TYP_MST)

sites3<-
        samples2 |>
        left_join(sites2, by = "ID_PN") |>
        select(ID_PN, TYP_MST) |>
        rename(Typ = TYP_MST) |>
        mutate(
                Typ =
                        case_when(
                                str_detect(Typ, "Typ") ~ Typ,
                                Typ == "5"   ~ "Typ 05",
                                Typ == "5.1" ~ "Typ 05.1",
                                Typ == "6"   ~ "Typ 06",
                                Typ == "7"   ~ "Typ 07",
                                Typ == "9"   ~ "Typ 09",
                                Typ == "9.2" ~ "Typ 09.2",
                                Typ == "10"  ~ "Typ 10",
                                Typ == "19"  ~ "Typ 19"
                        )
        ) |>
        unique(by = "PN_ID") 
        

sites3$Typ |> unique()

unique(sites3$Typ)

#- we have 619 sampling events  
uniqueN(sites3$ID_PN)
site.group.beg <- seq(from = 1, to = 619, by = 500)
site.group.end <- site.group.beg - 1
site.group.end <- site.group.end[-1]
site.group.end[length(site.group.end) + 1]  <- 619

sites.list   <- lapply(seq_along(site.group.beg), function(x) sites3[site.group.beg[x]:site.group.end[x], ])
samples.list <- lapply(seq_along(site.group.beg), function(x) samples3[ID_PN %in% sites.list[[x]]$ID_PN] )
samples.list <- lapply(samples.list, pivot_wider, names_from = "ID_PN", values_fill = 0, values_from = "abundance")
#- drop taxa that do not occur in subset
samples.list <- lapply(seq_along(samples.list), function(x) samples.list[[x]][which(rowSums(samples.list[[x]][,-c(1,2)]) > 0), ])

for (i in seq_along(samples.list)){
        xlsx::write.xlsx(samples.list[[i]],    paste0("data/original data/peter_haase/auxilliary/perlodes/11_",i, "_", Sys.Date(),"perlodes_taxa_set2_1.xlsx"))
        xlsx::write.xlsx(sites.list[[i]],      paste0("data/original data/peter_haase/auxilliary/perlodes/11_",i, "_", Sys.Date(),"perlodes_taxa_set2_2.xlsx"))
}

# FORMAT RESULTS --------------------------------------------------------------------
res.file <- fs::dir_ls("data/original data/peter_haase/auxilliary/perlodes/results")

res.file <- lapply(seq_along(res.file), function(x) read_excel(res.file[x], sheet = 3))
res.file <- lapply(res.file, function(x) select(x, Probe, ÖZK))
res.file <- rbindlist(res.file)

saveRDS(res.file, "data/original data/peter_haase/auxilliary/perlodes_results.rds")
