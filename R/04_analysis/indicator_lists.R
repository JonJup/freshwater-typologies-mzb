# —————————————————————————————————————————————— #
# ——— Analysis: IndVal -- Indiator lists ——————— # 
# —————————————————————————————————————————————— #

# ———————————————————————————————————
#  date created: 10-02-22
# last modified: 11-02-22
#       Project: Evaluating pan-European river typologies with macroinvertebrates
#       Purpose: Derive indicator lists 
# ———————————————————————————————————

library(pacman)
p_load(
        data.table,
        dplyr,
        forcats,
        ggplot2,
        ggwordcloud,
        indicspecies,
        magrittr,
        stringr,   
        tibble,
        tidyr,
        vegan,
        xlsx
)




# function --------------------------------------------------------------------------
summarize.indval <- function(x){
        
        type_names <- 
                names(x$sign) |> 
                { \(x) x[-which( x %in% c("index", "p.value", "stat") ) ]}() |> 
                str_remove("^s\\.")
        
        out <- x$sign |> 
                filter(p.value <= 0.05) |> 
                select(index) |> 
                rownames_to_column() |> 
                arrange(index) |> 
                mutate(id = 1) |> 
                pivot_wider(id_cols = rowname, names_from = index, values_from = id, values_fill = 0) |> 
                pivot_longer(!rowname) |> 
                mutate(id = 1:n()) |> 
                mutate(rowname = fct_reorder(rowname, id))
        
        out$type <- type_names[as.numeric(out$name)]
        
        
        return(out)
}

# load data -------------------------------------------------------------------------
id   <- readRDS("data/02_combined_data/04_2022-05-04_distance_ids.rds")

sp <- readRDS("data/02_combined_data/04_2022-05-04_community_data_spring.rds")
su <- readRDS("data/02_combined_data/04_2022-05-04_community_data_summer.rds")
au <- readRDS("data/02_combined_data/04_2022-05-04_community_data_autumn.rds")

# prepare analysis ------------------------------------------------------------------

#-  set a retrievable factor order 

brt_lvls1 <- c("RT01", "RT02", "RT03", "RT04", "RT05", "RT06", "RT07", "RT08", "RT09", "RT10", "RT11", "RT12")
brt_lvls2 <- c("RT01", "RT02", "RT03", "RT04", "RT05", "RT06", "RT07", "RT08", "RT09", "RT10", "RT11")
id$sp$brt %<>% factor(levels = brt_lvls1)
id$su$brt %<>% factor(levels = brt_lvls1)
id$au$brt %<>% factor(levels = brt_lvls2)

#- In the following code I want to move the broad river type column to before the first taxon. 
#- For this I need the name of the first taxon 
t1 <-c(names(sp)[2], names(su)[2], names(au)[2])

# - reformat table. Remove site id, transform to presence absence, add typology as factor and arrange by type
sp.brt <- sp[,-1] |> decostand("pa") |>  mutate(brt = id$sp$brt) |> relocate(brt, .before = t1[1]) |> arrange(brt)
su.brt <- su[,-1] |> decostand("pa") |>  mutate(brt = id$su$brt) |> relocate(brt, .before = t1[2]) |> arrange(brt)
au.brt <- au[,-1] |> decostand("pa") |>  mutate(brt = id$au$brt) |> relocate(brt, .before = t1[3]) |> arrange(brt)
sp.ife <- sp[,-1] |> decostand("pa") |>  mutate(ife = id$sp$ife) |> relocate(ife, .before = t1[1]) |> arrange(ife)
su.ife <- su[,-1] |> decostand("pa") |>  mutate(ife = id$su$ife) |> relocate(ife, .before = t1[2]) |> arrange(ife)
au.ife <- au[,-1] |> decostand("pa") |>  mutate(ife = id$au$ife) |> relocate(ife, .before = t1[3]) |> arrange(ife)
sp.bgr <- sp[,-1] |> decostand("pa") |>  mutate(bgr = id$sp$bgr) |> relocate(bgr, .before = t1[1]) |> arrange(bgr)
su.bgr <- su[,-1] |> decostand("pa") |>  mutate(bgr = id$su$bgr) |> relocate(bgr, .before = t1[2]) |> arrange(bgr)
au.bgr <- au[,-1] |> decostand("pa") |>  mutate(bgr = id$au$bgr) |> relocate(bgr, .before = t1[3]) |> arrange(bgr)
rm(t1)

# - compute indicator species 
sp.brt <- multipatt(x = sp.brt[,-1], cluster = pull(sp.brt, brt), duleg = TRUE, permutations = 999)
su.brt <- multipatt(x = su.brt[,-1], cluster = pull(su.brt, brt), duleg = TRUE, permutations = 999)
au.brt <- multipatt(x = au.brt[,-1], cluster = pull(au.brt, brt), duleg = TRUE, permutations = 999)
sp.ife <- multipatt(x = sp.ife[,-1], cluster = pull(sp.ife, ife), duleg = TRUE, permutations = 999)
su.ife <- multipatt(x = su.ife[,-1], cluster = pull(su.ife, ife), duleg = TRUE, permutations = 999)
au.ife <- multipatt(x = au.ife[,-1], cluster = pull(au.ife, ife), duleg = TRUE, permutations = 999)
sp.bgr <- multipatt(x = sp.bgr[,-1], cluster = pull(sp.bgr, bgr), duleg = TRUE, permutations = 999)
su.bgr <- multipatt(x = su.bgr[,-1], cluster = pull(su.bgr, bgr), duleg = TRUE, permutations = 999)
au.bgr <- multipatt(x = au.bgr[,-1], cluster = pull(au.bgr, bgr), duleg = TRUE, permutations = 999)
beepr::beep()

# - summarize indicator analysis results
sp.brt <- summarize.indval(sp.brt)
su.brt <- summarize.indval(su.brt)
au.brt <- summarize.indval(au.brt)
sp.ife <- summarize.indval(sp.ife)
su.ife <- summarize.indval(su.ife)
au.ife <- summarize.indval(au.ife)
sp.bgr <- summarize.indval(sp.bgr)
su.bgr <- summarize.indval(su.bgr)
au.bgr <- summarize.indval(au.bgr)

# - reshape 
sp.brt.wide <- pivot_wider(sp.brt, id_cols = c("rowname"), names_from = type, values_from = value)
su.brt.wide <- pivot_wider(su.brt, id_cols = c("rowname"), names_from = type, values_from = value)
au.brt.wide <- pivot_wider(au.brt, id_cols = c("rowname"), names_from = type, values_from = value)
sp.ife.wide <- pivot_wider(sp.ife, id_cols = c("rowname"), names_from = type, values_from = value)
su.ife.wide <- pivot_wider(su.ife, id_cols = c("rowname"), names_from = type, values_from = value)
au.ife.wide <- pivot_wider(au.ife, id_cols = c("rowname"), names_from = type, values_from = value)
sp.bgr.wide <- pivot_wider(sp.bgr, id_cols = c("rowname"), names_from = type, values_from = value)
su.bgr.wide <- pivot_wider(su.bgr, id_cols = c("rowname"), names_from = type, values_from = value)
au.bgr.wide <- pivot_wider(au.bgr, id_cols = c("rowname"), names_from = type, values_from = value)

#- what types are present in at least one season 
brt_types_all_seasons <-
        c(names(sp.brt.wide[, -1]),
          names(su.brt.wide[, -1]),
          names(au.brt.wide[, -1])) |>
        unique() |> 
        sort()
ife_types_all_seasons <-
        c(names(sp.ife.wide[, -1]),
          names(su.ife.wide[, -1]),
          names(au.ife.wide[, -1])) |>
        unique() |> 
        sort()
bgr_types_all_seasons <-
        c(names(sp.bgr.wide[, -1]),
          names(su.bgr.wide[, -1]),
          names(au.bgr.wide[, -1])) |>
        unique() |> 
        sort()

check_list_brt <- list(sp.brt.wide, su.brt.wide, au.brt.wide)
check_list_ife <- list(sp.ife.wide, su.ife.wide, au.ife.wide)
check_list_bgr <- list(sp.bgr.wide, su.bgr.wide, au.bgr.wide)

## add missing types to seasons 
## BRT 
for (i in 1:3){

        i.x <- check_list_brt[[i]]
        
        if (all(brt_types_all_seasons %in% names(i.x))){
                next()
        } else {
                ## which types are missing 
                i.missing_types <- brt_types_all_seasons[which(!brt_types_all_seasons %in% names(i.x))]
                for (k in seq_along(i.missing_types)) {
                        i.x$add <- 0
                        names(i.x)[ncol(i.x)] <- i.missing_types[k]
                }
        }
        check_list_brt[[i]] <- i.x
                
}
##IFE
for (i in 1:3){
        
        i.x <- check_list_ife[[i]]
        
        if (all(ife_types_all_seasons %in% names(i.x))){
                next()
        } else {
                ## which types are missing 
                i.missing_types <- ife_types_all_seasons[which(!ife_types_all_seasons %in% names(i.x))]
                for (k in seq_along(i.missing_types)) {
                        i.x$add <- 0
                        names(i.x)[ncol(i.x)] <- i.missing_types[k]
                }
        }
        check_list_ife[[i]] <- i.x
        
}
## BGR
for (i in 1:3){
        
        i.x <- check_list_bgr[[i]]
        
        if (all(bgr_types_all_seasons %in% names(i.x))){
                next()
        } else {
                ## which types are missing 
                i.missing_types <- bgr_types_all_seasons[which(!bgr_types_all_seasons %in% names(i.x))]
                for (k in seq_along(i.missing_types)) {
                        i.x$add <- 0
                        names(i.x)[ncol(i.x)] <- i.missing_types[k]
                }
        }
        check_list_bgr[[i]] <- i.x
        
}




BRT <- 
        rbindlist(check_list_brt, use.names = TRUE) |> 
        {\(x) x[, lapply(.SD, sum), by = "rowname", .SDcols = brt_types_all_seasons]}() |> 
        unique(by = "rowname") |> 
        pivot_longer(cols = !rowname) |> 
        filter(value != 0) |> 
        mutate(rowname = as.character(rowname))

BRT.taxon <- BRT |> arrange(rowname)
BRT.type  <- BRT |> arrange(name, value)


IFE <- rbindlist(check_list_ife, use.names = TRUE)
IFE <- IFE[, lapply(.SD, sum), by = "rowname", .SDcols = ife_types_all_seasons]
IFE <- unique(IFE, by = "rowname")
IFE %<>% pivot_longer(cols = !rowname)
IFE %<>% filter(value != 0) %>% mutate(rowname = as.character(rowname))

BGR <- rbindlist(check_list_bgr, use.names = TRUE)
BGR <- BGR[, lapply(.SD, sum), by = "rowname", .SDcols = bgr_types_all_seasons]
BGR <- unique(BGR, by = "rowname")
BGR %<>% pivot_longer(cols = !rowname)
BGR %<>% filter(value != 0) %>% mutate(rowname = as.character(rowname))

names(BRT) <- c("taxon", "type", "number_of_seasons")
names(IFE) <- c("taxon", "type", "number_of_seasons")
names(BGR) <- c("taxon", "type", "number_of_seasons")


# save as excel files  --------------------------------------------------------------

write.xlsx(BRT, file = paste0("data/indicator_lists/",Sys.Date(),"_indicator_taxa_brt.xlsx"))
write.xlsx(IFE, file = paste0("data/indicator_lists/",Sys.Date(),"_indicator_taxa_ife.xlsx"))
write.xlsx(BGR, file = paste0("data/indicator_lists/",Sys.Date(),"_indicator_taxa_bgr.xlsx"))


## Restekiste 

# sp.ife <- indicspecies::multipatt(x = sp.l[,-1], cluster = isp.l$ife, duleg = TRUE, permutations = 999)
# sp.bgr <- indicspecies::multipatt(x = sp.l[,-1], cluster = isp.l$bgr, duleg = TRUE, permutations = 999)
# su.brt <- indicspecies::multipatt(x = su.l[,-1], cluster = isu.l$brt, duleg = TRUE, permutations = 999)
# su.ife <- indicspecies::multipatt(x = su.l[,-1], cluster = isu.l$ife, duleg = TRUE, permutations = 999)
# su.bgr <- indicspecies::multipatt(x = su.l[,-1], cluster = isu.l$bgr, duleg = TRUE, permutations = 999)
# au.brt <- indicspecies::multipatt(x = au.l[,-1], cluster = iau.l$brt, duleg = TRUE, permutations = 999)
# au.ife <- indicspecies::multipatt(x = au.l[,-1], cluster = iau.l$ife, duleg = TRUE, permutations = 999)
# au.bgr <- indicspecies::multipatt(x = au.l[,-1], cluster = iau.l$bgr, duleg = TRUE, permutations = 999)
# 
# 
# 
# 
# sp.brt.i <- summarize.indval(sp.brt, typ = "brt")
# sp.ife.i <- summarize.indval(sp.ife)
# sp.bgr.i <- summarize.indval(sp.bgr)
# su.brt.i <- summarize.indval(su.brt, typ = "brt")
# su.ife.i <- summarize.indval(su.ife)
# su.bgr.i <- summarize.indval(su.bgr)
# au.brt.i <- summarize.indval(au.brt, typ = "brt")
# au.ife.i <- summarize.indval(au.ife)
# au.bgr.i <- summarize.indval(au.bgr)
#         
# install.packages("ggwordcloud")
# library(ggwordcloud)
# 
# ggplot(sp.brt.i, aes(y = rowname, x = type, fill = value)) + 
#         geom_tile()
# 
# ggplot(filter(sp.brt.i, value == 1), aes(label = rowname)) +
#         geom_text_wordcloud() +
#         theme_minimal() + 
#         facet_wrap(. ~type)
# ggplot(filter(su.brt.i, value == 1), aes(label = rowname)) +
#         geom_text_wordcloud() +
#         theme_minimal() + 
#         facet_wrap(. ~type)
# ggplot(filter(au.brt.i, value == 1), aes(label = rowname)) +
#         geom_text_wordcloud() +
#         theme_minimal() + 
#         facet_wrap(. ~type)
# 
# 
# sp.brt.i.wide <- t$pivot_wider(sp.brt.i, id_cols = c("rowname"), names_from = type, values_from = value)
# su.brt.i.wide <- t$pivot_wider(su.brt.i, id_cols = c("rowname"), names_from = type, values_from = value)
# au.brt.i.wide <- t$pivot_wider(au.brt.i, id_cols = c("rowname"), names_from = type, values_from = value)
# 
# ## add missing types as columns 
# sp.brt.i.wide %<>% 
#         mutate(RT04 = 0) %>%
#         mutate(RT05 = 0) %>%
#         mutate(RT07 = 0)
# au.brt.i.wide %<>% 
#         mutate(RT05 = 0)
# 
# 
# xxx <- rbindlist(list(sp.brt.i.wide, au.brt.i.wide, su.brt.i.wide), use.names = TRUE)
# setDT(xxx)
# xxx[, RT01 := sum(RT01), by = "rowname"]
# xxx[, RT02 := sum(RT02), by = "rowname"]
# xxx[, RT03 := sum(RT03), by = "rowname"]
# xxx[, RT04 := sum(RT04), by = "rowname"]
# xxx[, RT05 := sum(RT05), by = "rowname"]
# xxx[, RT06 := sum(RT06), by = "rowname"]
# xxx[, RT07 := sum(RT07), by = "rowname"]
# xxx[, RT08 := sum(RT08), by = "rowname"]
# xxx[, RT09 := sum(RT09), by = "rowname"]
# xxx[, RT10 := sum(RT10), by = "rowname"]
# xxx[, RT11 := sum(RT11), by = "rowname"]
# xxx[, RT12 := sum(RT12), by = "rowname"]
# 
# xxx <- unique(xxx, by = "rowname")
# xxx <- t$pivot_longer(xxx, cols = !rowname)
# xxx %<>% mutate(value = factor(value))
# 
# ggplot(BRT, aes(y = rowname, x = name, fill = value)) +
#         geom_tile() +
#         theme(
#                 panel.background = element_rect(fill = "white"),
#                 axis.ticks.length = unit(0, "line"),
#                 axis.text.x = element_text(angle = 90),
#                 plot.margin = unit(c(1, 1, 1, 1), "cm")
#                 #legend.position = "none"
#         )
# 
# BRT |> 
#         filter(value != 0) |> 
#         ggplot(aes(label = rowname, size = value, col = value)) + 
#         geom_text_wordcloud() + 
#         facet_wrap(.~name)
