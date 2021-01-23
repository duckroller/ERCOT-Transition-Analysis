multivariate_hcl <- function(merged_data, k_count = 9, var_count = 2, mtd = "complete", graph_type = "Dendrogram") {
  
  
  var_groupings <- subsets(13, var_count, names(merged_data)[1:13])
  
  n <- nrow(var_groupings)
  
  selected_data <- data.frame(merged_data[,var_groupings[x,]])
  
  #rownames(merged_data) <- merged_data$state
  #merged_data$state <- NULL
  
  #scaled_data <- scale(merged_data)
  dist_data <- dist(selected_data[], diag=TRUE)
  
 # robustNBClust = function(x) {
#    tryCatch(NbClust(x),
#             warning = function(w) {print(paste("Warning: ", w)); 
#               NbClust(x)},
#             error = function(e) {print(paste("error", e)); 
#               NaN}) 
#  }
  
 # nbclust_data <- robustNBClust(data = merged_data, diss = NULL, distance = "euclidean", min.nc = 8, max.nc = 13, method = mtd)
  
#  k_count <- max(nbclust_data$Best.partition)
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
    graph <- fviz_cluster(list(data = selected_data, cluster = sub_grp))
  } else {graph <- NA}
  
  clust <- merge(selected_data, cutree(h_clust, k = k_count), by = 0)
  
  #print(clust)
  
  clust$cluster <- clust$y
  clust$y <- NULL
  
  output <- list(clusters = clust, hc = h_clust, k = k_count, graph = graph)
  
  return(output)
}
