### --------------------------------------------- ###
### --- Evaluate bio-clusters from typologies --- ### 
### --------------------------------------------- ###

# --------------- #
# files in:  
#               -> 08_sxs_genus_W_bio_typology.rds
#               -> 09_sxs_genus_typology_wo_bio.rds
# files out:  
#               <- 10_class_eval_mzb.rds
# Purpose: 
#               Compute cluster metrics for the different typologies.   
# --------------- #

# setup ---------------------------------------------------------------------------------------------------------------------------------------------------
pacman::p_load(
        data.table, 
        dplyr, 
        indicspecies, 
        fpc,
        future.apply, 
        magrittr,
        purrr,
        sf,
        tidyr
)

# functions -------------------------------------------------------------------------
genmean<-function(x,p)   {
        x<-x[!is.na(x)]
        n<-length(x)
        M<-((1/n)*(sum(x^p)))^(1/p)
        if(p==-Inf)   M<-min(x)
        if(p==-1)     M<-(sum(x^-1)/length(x))^-1
        if(p==0)      {x[x==0]<-1e-10; M<-exp(mean(log(x)))}
        if(p==Inf)    M<-max(x) 
        return(M)
}
mdist<-function(d,gr,p)  {
        GR<-unique(gr)
        k<-length(GR)
        mean.d<-vector('numeric')
        for(i in 1:k)   {
                mean.d[i]<-genmean(d[gr==GR[i]], p=p)
        }
        return(mean.d)
}
silgen<-function(d,gr,p)   {
        di<-as.matrix(d)
        diag(di)<-NA
        GR<-unique(gr)
        N<-nrow(di)
        Do<-matrix(NA, nrow=N, 3)
        i<-1
        while(i<=N)   {
                Dvec<-mdist(d=di[i,],gr=gr,p=p)
                K<-which(GR==gr[i])
                if(sum(gr==gr[i])>1) {a<-Dvec[K]
                }   else  {a<-0}
                Kx<-which(Dvec==min(Dvec[GR!=gr[i]]))[1]
                b<-Dvec[Kx]
                do1<-ifelse(a==0 & b==0,  do1<-0, do1<-(b-a)/max(a,b))
                Do[i,1]<-do1
                Do[i,2]<-gr[i]
                Do[i,3]<-GR[Kx]
                i<-i+1
        }
        colnames(Do)<-c("Width","Original","Neighbour")
        return(Do)
}
call_gensil = function(type, p_v = c(-Inf, -2,-1,1,2,Inf)){
        gensil_v = vector(mode = "numeric", length = length(p_v))
        gensil_df = 
                # compute mean generalized silhouette width for each 
                # value supplied in p_v
                map_dbl(.x = 1:length(p_v),
                        .f = ~ mean(silgen(
                                d = dt_distance,
                                gr = type,
                                p = p_v[.x]
                        )[, 1])) %>% 
                # format as tibble 
                tibble(silhouette = .)  %>% 
                # add p to table 
                mutate(p = p_v)
        return(gensil_df)
}

# load data  ----------------------------------------------------------------------------------------------------------------------------------------------
data  = readRDS("data/12_sxs_genus_typology_wo_bio.rds")
data2 = readRDS("data/11_sxs_genus_W_bio_typology.rds")

# join typology with bioclusters  ---------------------------------------------------
data2 %<>% dplyr::select(gr_sample_id, bio)
data = left_join(x = data, 
                  y = data2, 
                  by = "gr_sample_id"
)
rm(data2);gc()

## -- remove rare types 
data %<>%
        filter(ls20 != "RT17") %>% 
        filter(!gloric %in%  c(17, 22)) %>% 
        filter(!illies %in%  c("Fenno-scandian shield"))  


# prepare data  ----------------------------------------------------------------
## -- sites x species table as matrix and without typology columns
setDT(data)
ma_data = 
        copy(data) %>% 
        .[, c("gr_sample_id", "ls20", "ls12", "illies", "eea", "gloric", "bio") := NULL] %>% 
         as.matrix

## -- list of classifications - those that are strings are transformed to numbers 
ls_class = list(
        ls12   = as.numeric(factor(data$ls12)),
        ls20   = as.numeric(factor(data$ls20)),
        gloric = data$gloric,
        illies = as.numeric(factor(data$illies)),
        eea    = as.numeric(factor(data$eea)),
        bio    = data$bio
)
## -- compute Jaccard distance matrix of data 
dt_distance = parallelDist(ma_data, method = "binary")

# create null classification --------------------------------------------------------
group_sizes = data[, lapply(.SD, uniqueN), .SDcols = c("ls12", "ls20", "gloric", "eea", "illies", "bio")]

number_of_typologyies = length(ls_class)

## -- loop to fill ls_class with null typologies 
for (i in 1:100){
        set.seed(i)
        n_types = sample(min(group_sizes):max(group_sizes), 1)
        set.seed(i)
        ls_class[[i + number_of_typologyies]] = sample(1:n_types, 
                                   size = nrow(ma_data), 
                                   replace = TRUE)
        names(ls_class)[i+7] = paste0("null",i)
}
rm(group_sizes, i, n_types, number_of_typologyies)
gc()

# compute cluster metrics ---------------------------------------------------------------------------------------------------------------------------------

## -- This was stable when evaluating all 106 typologies together. 
## -- Hence it is not run in separate chunks. 

## -- extend the maximum allowable size of objects to be exportet
## -- MB * 1024^2
options(future.globals.maxSize= 600*1024^2)

## -- cluster.stats function 
eval = future_lapply(
        1:length(ls_class),
        function (x) cluster.stats(
                d = dt_distance,
                clustering = ls_class[[x]],
                silhouette = FALSE,
                wgap = FALSE,
                sepindex = FALSE,
                sepprob = FALSE,
                sepwithnoise = FALSE
                )
        ) %>% 
        flatten(); beep()

## -- reshape to create results table 
eval = eval[ - which(lapply(eval, length) != 1)]
name_var = names(eval)
eval2 = data.table(eval)
eval2[, c("name", "clus") := .(name_var, rep(1:length(ls_class), 
                                                each = nrow(eval2)/length(ls_class)))]
eval2 = pivot_wider(
        eval2,
        id_cols = clus,
        names_from = name,
        values_from = eval) %>%
        apply(2, unlist) %>%
        as.data.frame() %>%
        setDT()

# generalized silhouette width  -------------------------------------------

## -- This was stable when evaluating all 106 typologies together. 
## -- Hence it is not run in separate chunks. 
gensil_output = future_lapply(X = 1:length(ls_class),
                              function(x)
                                      call_gensil(ls_class[[x]]))

## -- old non-parallel approach 
# gensil_test_output = map(.x = 1:length(ls_class),
#                          .f = ~ call_gensil(ls_class[[.x]]))

gensil_dt = rbindlist(gensil_output)
typo_names = append (c("brt12", "brt20", "gloric", "illies", "eea", "bio"), paste0("null",1:100))
gensil_dt[, typology := rep(typo_names, each = 6)]
gensil_dt %<>% 
        mutate(p2 = case_when(p == -Inf ~"min",
                              p == -2 ~ "m2",
                              p == -1 ~ "harmonic",
                              p == 1 ~ "arithmetic",
                              p == 2 ~ "quadratic",
                              p == Inf ~ "max")) %>%  
        pivot_wider(id_cols = typology, names_from = p2, values_from = silhouette) 

## -- save temporary before computing indval in chunks. 
saveRDS(list(gensil_dt, eval2), "data/temp/eval_clust_pre_indval.rds")

## ------------------------------------ ## 
## -- load data to restart from here -- ##
## ------------------------------------ ##
read_list = readRDS("data/temp/eval_clust_pre_indval.rds")
gensil_dt = read_list[[1]]
eval2 = read_list[[2]]
rm(read_list)

## -- clean up 
rm(
        call_gensil,
        typo_names,
        cc,
        data,
        eval,
        extr_trait,
        gen_mean,
        internal_cluster_fun,
        mdist,
        name_var,
        plot_pcoa,
        silgen,
        proj_pcoa,
        my_sil,
        genmean,
        eval_cluster_fun
)


# IndVal ------------------------------------------------------------------

## -------------------- ##
## -- COMPUTE INDVAL -- ##
## -------------------- ##

## -- this crashed R once and will hence be run in chunks of ten. 
## -- Outputs are to be saved as temporary files after each run. 
## -- After the second run, check that results can be combined. 
## -- CHECKED: works 
## -- takes a long time (days)! 

## -- what is the minimum p_value? 
0.05/602 < 1/15000
0.05/602 < 2/25000
plan(multiprocess, workers = 4)
for (i in 0:10){

        lp_name = paste0("results_indval",i+1)
        from = paste0(i,"1")   %>% as.numeric()
        to   = paste0(i+1,"0") %>% as.numeric()
        if (i == 10)
                to = 107
        assign(x = lp_name,
               value = future_lapply(
                       X = ls_class[from:to],
                       FUN = function(x)
                               multipatt(
                                       ma_data,
                                       cluster = x,
                                       control = how(nperm = 25000),
                                       max.order = 1,
                                       print.perm = T
                               )
               )
                       )
        xx = get(lp_name)
        save_name = paste0("data/temp/eval_clust_indval_",i+1,".rds")
        saveRDS(xx, save_name)
}

## ------------------------------------ ##
## -- START FROM HERE TO LOAD INDVAL -- ## 
## ------------------------------------ ## 

## -- list files to load 
files = dir("data/temp/", pattern = "eval_clust_indval")

## -- load files in loop 
for (i in seq_along(files)){
        file_number = gsub(pattern = "eval_clust_indval_", replacement = "", x = files[i])
        file_number = gsub(pattern = ".rds", replacement = "", x = file_number)
        file_number = gsub(pattern = "_", replacement = "", x = file_number)
        assign(
                x = paste0("results_indval",file_number),
                value = 
                        readRDS(file.path("data/temp", files[i]))
        )
        rm(i, file_number);gc()
}

## -- bind files to list 
all_results_indval = c(
        results_indval1, 
        results_indval2,
        results_indval3,
        results_indval4,
        results_indval5,
        results_indval6,
        results_indval7,
        results_indval8,
        results_indval9,
        results_indval10,
        results_indval11
         )

rm(files,results_indval1, results_indval2,results_indval3,results_indval4,results_indval5,results_indval6,results_indval7,results_indval8,results_indval9,results_indval10,results_indval11)

## --------------------------------- ##
## -- ADJUST FOR MULTIPLE TESTING -- ##
## --------------------------------- ##

## -- reshape from indicator class format 
all_results_indval2 = lapply(all_results_indval, 
                    function(x) data.table(taxon  = rownames(x$A), 
                                           pvalue = x$sign$p.value,
                                           indval = x$sign$stat))

## -- drop taxa that do not occur in the river type 
all_results_indval2 %<>%
        lapply(function(x) x[indval != 0])

## -- number of tests = number of taxa. 
## -- Family-wise error only corrected within each river type
n_tests =  lapply(all_results_indval2, nrow)

## -- add a variable with the number of tests to each table 
all_results_indval2 = map(.x = 1:length(all_results_indval2),
                 .f = ~ all_results_indval2[[.x]][, n_tests := n_tests[.x]])

all_results_indval2 %<>%
        ## -- add bonferroni p-value 
        lapply(function(x)
                x[, bonferroni_p := 0.05 / n_tests]) %>%
        ## -- compare to bonferroni p-value 
        lapply(function(x)
                x[, indicator := pvalue <= bonferroni_p])

## -- compute the IndVal Statistic 
indval_statistic = lapply(all_results_indval2, function(x) x[significant == TRUE, sum(stat)/ncol(ma_data)])


# combine data  -----------------------------------------------------------
eval2[, indval := unlist(indval_statistic)]

eval2[, typology := c("brt12", "brt20", "gloric", "illies", "eea", "bio", paste0("null",1:100))]
eval2[, max.diameter := NULL]
eval3 = copy(eval2)
eval3 %<>% left_join(gensil_dt, 
                     by = "typology")



# compute classification strength ---------------------------------------------------

dt_similartiy = 1 - dt_distance
eval4 = lapply(
        1:length(ls_class),
        function (x) cluster.stats(
                d = dt_similartiy,
                clustering = ls_class[[x]],
                silhouette = FALSE,
                wgap = FALSE,
                sepindex = FALSE,
                sepprob = FALSE,
                sepwithnoise = FALSE
        )
) %>% 
        flatten();beep()
eval4 = eval4[ - which(lapply(eval4, length) != 1)]
name_var = names(eval4)
eval5 = data.table(eval4)
eval5[, c("name", "clus") := .(name_var, rep(1:length(ls_class), 
                                             each = nrow(eval5)/length(ls_class)))]
eval5 = pivot_wider(
        eval5,
        id_cols = clus,
        names_from = name,
        values_from = eval4) %>%
        apply(2, unlist) %>%
        as.data.frame() %>%
        setDT()

eval5[, classification_strength := average.within - average.between]

eval3$classification_strength = eval5$classification_strength

# save to file ----------------------------------------------------------------------
saveRDS(eval3, "data/10_class_eval_mzb.rds")
