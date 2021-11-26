#' Optimize a landscape typology given a set of features 
#'
#' @param dist a distance matrix with n x n features  
#' @param grouping a character vector with n labels 
#'   
#' @return
#' @export
#'
#' @examples
optimize_typology <- function(dist, grouping){
        
        #- setup 
        combination_log <- list()
        for (i in 1:11){
                
        #- take the original typology and compute cs and silhouette width 
        #- cs
        cs <- jjmisc::classification_strength(dist = dist, grouping = grouping, permutations = 1, typology = "brt12", season = "summer")
        
        print(paste(i,"-", round(cs$cs,2)))
        #- silhouette 
        sil <- cluster::silhouette(x = grouping, dist = dist)
        sil2 <- data.frame(cluster = sil[,1], 
                           neighbor = sil[,2],
                           sil_width = sil[,3])
        #- determine the most two most similar types 
        sil3 <- tapply(sil2$sil_width, INDEX = sil2$cluster, FUN = mean)
        sil3 <- sort(sil3)
        sil3 <- sil3[1:3]
        
        id1 <- which(sil2$cluster == names(sil3)[1])
        id2 <- which(sil2$cluster == names(sil3)[2])
        id3 <- which(sil2$cluster == names(sil3)[3])
        
        freqs1 <- table(sil2$neighbor[id1])/length(id1)
        freqs2 <- table(sil2$neighbor[id2])/length(id2)
        freqs3 <- table(sil2$neighbor[id3])/length(id3)
        
        entropy <- c()
        
        entropy[1] <- -sum(freqs1 * log2(freqs1))
        entropy[2] <- -sum(freqs2 * log2(freqs2))
        entropy[3] <- -sum(freqs3 * log2(freqs3))
        
        sil4 <- data.frame(cluster    = as.integer(names(sil3)),
                           silhouette = round(sil3,2),
                           entropy    = round(entropy,1))
        sil4$rank <-  rank(sil4$silhouette) +  rank(sil4$entropy)
        
        resolve <- sil4[which.min(sil4$rank), "cluster"]
        add_to <- which(names(sil3) == as.character(resolve))
        add_to <- get(paste0("id",add_to)) 
        add_to <- table(sil2$neighbor[add_to])
        add_to <- as.integer(names(add_to)[which.max(add_to)])
        
        combination_log[[i]] <- paste(resolve, "is combined with", add_to)
        print(combination_log[[i]])
        #- combine the two most similar types 
        grouping[which(grouping == resolve)] <- add_to
        }
        #- return output 
      
}
