bivariate_clustering <- function(merged_data, min_nodes = 3, mtd = "complete", graph_type = "Dendrogram") {
  
  rownames(merged_data) <- merged_data$state
  merged_data$state <- NULL
  
  #scaled_data <- scale(merged_data)
  dist_data <- dist(merged_data[], diag=TRUE)
  
  robustNBClust = function(x) {
        tryCatch(NbClust(x),
                 warning = function(w) {print(paste("Warning: ", w)); 
                   NbClust(x)},
                 error = function(e) {print(paste("error", e)); 
                   NaN}) 
     }
  
  nbclust_data <- robustNBClust(data = merged_data, diss = NULL, distance = "euclidean", min.nc = 8, max.nc = 13, method = mtd)
  
  k_count <- max(nbclust_data$Best.partition)
  h_clust <- hclust(dist_data, method = mtd)
  
  if(graph_type == "Dendrogram"){
    graph <- fviz_dend(h_clust, k = k_count, # Cut in four groups
                     cex = 0.5, # label size
                     k_colors = c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000"), 
                     color_labels_by_k = TRUE,# color labels by groups
                     rect = TRUE# Add rectangle around groups
    )
  } else if(graph_type == "Cluster Graph")
    {
    sub_grp <- cutree(h_clust, k = k_count)
    graph <- fviz_cluster(list(data = merged_data, cluster = sub_grp))
  } else {graph <- NA}
  
  clust <- merge(merged_data, nbclust_data$Best.partition, by = 0)
  
  clust$cluster <- clust$y
  clust$y <- NULL
  
  output <- list(clusters = clust, hc = h_clust, nbc = nbclust_data, k = k_count, graph = graph)

  return(output)
  }
