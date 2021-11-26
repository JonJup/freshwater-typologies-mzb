# purpose: temporal aggregation function 
# date created: 21-09-21
# date last modified: 22-09-21

temporal_aggregation <- function(data,id_append,debug = FALSE){
        
        data[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
        data[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
        
        sites <- unique(data, by = "site_id")
        
        for (i in 1:nrow(sites)) {
                #for (i in 245:nrow(sites)){
                
                #- setup for a new list
                if (i == 1)
                        new.lst <- list()
                
                #- subset to focal site
                lp.sub   <- data[site_id == sites$site_id[i]]
                #- how many sampling events have taken place here?
                lp.n     <- unique(lp.sub$sampling.events)
                #- if it is only one - check richness and keep as such
                #- of there are less then five taxa at this site, drop it
                if (lp.n == 1) {
                        if (any(lp.sub$richness < 5)) {
                                new.lst[[i]] <- lp.sub[data.set == "none"]
                                next()
                        } else {
                                new.lst[[i]]  <- lp.sub
                                next()
                        }
                } 
                #- Otherwise verify that its one site with consistent x and y coordinates.
                #- Sometimes there are marginal differences between coordinates. 
                #- This can cause an error as more than one unique value exists 
                #- but we do not want this to throw an error since this is not the 
                #- behavior we want to catch with the lines below. 
                #- To avoid this we round the coordinate values 
                lp.x     <- uniqueN(round(lp.sub$x.coord,2))
                if (lp.x != 1){
                        print(paste("in", i, "there is more than one x coordinate"))
                        break()     
                }
                lp.y     <- uniqueN(round(lp.sub$y.coord,2))
                if ( lp.y != 1) {
                        print(paste("in", i, "there is more than one y coordinate"))
                        break()  
                }
                # - DEBUG CODE -# 
                if (debug == TRUE){
                db.sites <- st_as_sf(unique(lp.sub, by = "date"), coords = c("x.coord", "y.coord"), crs = 4326)
                tm_shape(db.sites) + tm_dots(col = "gr_sample_id", alpha = 0.5, jitter = 1)
                }
                # - END DEBUG  -# 
                lp.ls.season <-
                        list(
                                spring = lp.sub[season == "spring"],
                                summer = lp.sub[season == "summer"],
                                autumn = lp.sub[season == "autumn"],
                                winter = lp.sub[season == "winter"]
                        )
                
                #- which seasons are not empty?
                lp.season.id <- which(sapply(lp.ls.season, nrow) != 0)
                
                #- loop over non-empty seasons
                for (k in lp.season.id) {
                        lp.k.dat <- lp.ls.season[[k]]
                        
                        #- how many samples from this season?
                        lp.k.n <- uniqueN(lp.k.dat$gr_sample_id)
                        
                        #- if only one sample ...
                        #- check richness
                        #- keep as is if richness is OK
                        #- replace is empty data.table (lp.sub[season == "dummy"]) if richness is smaller than 5
                        if (lp.k.n == 1) {
                                if (any(lp.k.dat$richness < 5)) {
                                        lp.ls.season[[k]] <- lp.sub[data.set == "none"]
                                        next()
                                } else {
                                        lp.ls.season[[k]]  <- lp.k.dat
                                        next()
                                }
                        }
                        
                        #- ... if there is more than one sample in the season k
                        #- how far apart are the sampling dates?
                        lp.dates <- unique(lp.k.dat$date)
                        lp.time.diff <- diff(lp.dates)
                        #- if samples were taken within the same week combine them.
                        if (any(lp.time.diff < 7)) {
                                #- which samples are close together?
                                lp.diff.id <- which(lp.time.diff < 7)
                                #- extract date from lp.diff.ids
                                lp.diff.date <-
                                        lapply(lp.diff.id, function(x)
                                                lp.dates[x:(x + 1)])
                                
                                #- loop over samples the combinations.
                                #- implemented for cases where multiple lp.diff.ids
                                for (j in seq_along(lp.diff.id)) {
                                        #- select dates for this loop iteration
                                        lp.j.diff <- lp.diff.date[[j]]
                                        #- split data set: i) data to be combined (lp.combine) and ii). rest of the data (lp.rest)
                                        lp.combine <-
                                                lp.k.dat[date %in% lp.j.diff]
                                        lp.rest     <-
                                                lp.k.dat[!date %in% lp.j.diff]
                                        
                                        #- combine taxa; assign mean abundance
                                        lp.combine[, abundance := mean(abundance), by = "lowest.taxon"]
                                        lp.combine <-
                                                unique(lp.combine, by = "lowest.taxon")
                                        lp.combine[, richness     := .N]
                                        lp.draw.id <- which.max(unique(lp.combine$date))
                                        lp.combine[, date         := unique(lp.combine$date)[lp.draw.id]]
                                        lp.combine[, gr_sample_id := unique(lp.combine$gr_sample_id)[lp.draw.id]]
                                        lp.combine[, date_id := unique(lp.combine$date_id)[lp.draw.id]]
                                        lp.k.dat <- rbindlist(list(lp.rest, lp.combine))
                                }
                                
                                
                                lp.k.n <- uniqueN(lp.k.dat$gr_sample_id)
                                lp.k.dat$sampling.events <- lp.k.n
                                
                                if (lp.k.n == 1) {
                                        #- drop date
                                        lp.k.dat[, date := NA]
                                        lp.k.dat[, gr_sample_id := paste0(
                                                site_id,
                                                "_",
                                                c("spring", "summer", "autumn", "winter")[k],
                                                "_combined_biodrought"
                                        )]
                                        lp.ls.season[[k]] <- lp.k.dat
                                        next()
                                }
                        } # END OF: SAMPLES WITHIN ONE WEEK 
                        
                        #- check richness - remove samples with less than five taxa
                        if (any(unique(lp.k.dat$richness) < 5)) {
                                #- if all samples are below 5
                                if (all(lp.k.dat$richness < 5)) {
                                        lp.ls.season[[k]] <- lp.sub[season == "dummy"]
                                        next()
                                }
                                lp.k.dat <- lp.k.dat[richness > 5]
                                lp.k.n   <- uniqueN(lp.k.dat$gr_sample_id)
                                #- if only one remains
                                if (lp.k.n == 1) {
                                        lp.ls.season[[k]] <- lp.k.dat
                                        next()
                                }
                        }
                        
                        lp.tab  <- table(lp.k.dat$lowest.taxon)
                        lp.tab2 <- lp.tab / lp.k.n
                        lp.sub2 <-
                                lp.k.dat[lowest.taxon %in%  names(lp.tab2)[lp.tab2 >= 0.5]]
                        #- combine multiple observations of the same taxon. Take mean of abundance
                        lp.sub2[, abundance := mean(abundance), by = "lowest.taxon"]
                        lp.sub2 <- unique(lp.sub2, by = "lowest.taxon")
                        lp.sub2[, richness := .N]
                        #- add new sample id
                        lp.sub2[, gr_sample_id := paste0(site_id,
                                                         "_",
                                                         c("spring", "summer", "autumn", "winter")[k],
                                                         id_append)]
                        #- drop date
                        lp.sub2[, date := NA]
                        #- keep year if both were from the same year.
                        lp.sub2[, year := ifelse(uniqueN(year) == 1, year, NA)]
                        #- add to list
                        lp.ls.season[[k]] <- lp.sub2
                }
                
                lp.ls.season <- lp.ls.season[lp.season.id]
                lp.ls.season <- rbindlist(lp.ls.season)
                new.lst[[i]] <- lp.ls.season
                
                lp.progress <- round(i/nrow(sites) * 100,2)
                print(paste(i, "/",nrow(sites)," ----- (",lp.progress,"%)"))
                rm(list = ls()[grepl(x = ls(), pattern = "^lp\\.")])
                rm(i)
                gc()
        }
        data.out <- rbindlist(new.lst,use.names=TRUE,fill =TRUE)
        data.out[, richness := .N, by = "gr_sample_id"]
        return(data.out)
}
