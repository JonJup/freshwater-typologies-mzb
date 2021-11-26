### ———————————————————————————————————— ###
### ——— phylogenetic distance metric ——— ### 
### ———————————————————————————————————— ###

## add total # nodes in common 

library(rotl)
library(ape)
library(tidygraph)
library(ggnetwork)
pacman::p_load(ggraph, adephylo, SANTA)

TU <- data$lowest.taxon |> unique() |> sort()
tu.sub <- TU[1:20]
tnrs.match <- tnrs_match_names(tu.sub, context_name = "Animals")
#- fix problems 
#tnrs.match <- tnrs.match[-which(tnrs.match$search_string=="anisus septemgyratus"),]
#- grow tree
my_tree <- tol_induced_subtree(ott_ids = tnrs.match$ott_id)
#- plot tree
plot(my_tree, no.margin = TRUE)
#- cophenetic distance on tree 

#- create network
phylonet <- as.igraph.evonet(my_tree)
#- tidy network 
ggraph(as_tbl_graph(phylonet), 'dendrogram', circular = TRUE) + 
        geom_edge_link() + 
        geom_node_point() + 
        geom_node_label(aes(label = name))

phylo.distance <- distances(phylonet)
maximum.distance <- max(phylo.distance)
phylo.distance.norm <- phylo.distance/maximum.distance


# create example communities 
taxa <- colnames(phylo.distance)

sites <- matrix(rbinom(115,1,.5), ncol = 5)
rownames(sites) <- taxa
colnames(sites) <- paste0("site",1:5)

com1 <- rownames(sites)[as.logical(sites[,1])]
com2 <- rownames(sites)[as.logical(sites[,2])]
id1 <- which(rownames(phylo.distance.norm) %in% com1)
id2 <- which(rownames(phylo.distance.norm) %in% com2)
sub_dist1 <- phylo.distance.norm[id1, id2]
sub_dist2 <- phylo.distance.norm[id2, id1]

min1 <- apply(sub_dist1,1,min)
min2 <- apply(sub_dist2,1,min)

dist1 <- mean(min1)
dist2 <- mean(min2)

dist <- mean(c(dist1,dist2))
