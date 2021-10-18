closeness_function <- function(network){
  
  dist_matrix <-  distances(graph = network, mode = "out")
  diag(dist_matrix) <- NA
  mean_closeness <- mean(1 / dist_matrix, na.rm = T)
  return(mean_closeness)
}