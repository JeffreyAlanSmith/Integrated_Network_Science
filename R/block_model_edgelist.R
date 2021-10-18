block_model_edgelist <- function(block_model, relation_label, directed = T){
  # Arguments: 
  # block_model: blockmodel object 
  # relation_label: label for type of tie, 
  # directed: is blockmodel based on directed network?

  # First we grab the actual matrix from the blockmodel:
  block_mat <- block_model$block.model

  # Here we set any NA to 0.
  block_mat[is.na(block_mat)] <- 0

  # Now we create a little network based on the matrix.
  net_block <- network(block_mat, loops = T, 
                       directed = directed)

  # Here we extract the edgelist:
  edges_netblock <- as.edgelist(net_block)

  # Now we get the edge weights, stringing out the matrix
  # into a vector. We only extract those weights corresponding
  # to where an edge exists, defined by the edgelist 
  # extracted above.
  weight_edge <- c(block_mat[edges_netblock])

  # Now we create a little data frame putting the information together.
  block_edgelist <- data.frame(edges_netblock, weight = weight_edge, 
                               Tie = relation_label)

  # Here we create the additional weighting scheme,
  # where weight is equal to 0, 1, or 2, depending if it is
  # less than the mean, greater than the mean (but less than 1 sd above mean)
  # or greater than 1 sd above the mean. 
  edge_mean <- mean(block_mat)
  edge_sd <- sd(block_mat)
  edge_max <- max(block_mat)

  block_edgelist$WeightRecode <- cut(block_edgelist$weight, 
                                     breaks = c(0, edge_mean, 
                                                edge_mean + edge_sd, 
                                                edge_max),
                                     include.lowest = T, 
                                     right = F, labels = c("0", "1", "2")) 

  block_edgelist$WeightRecode <- as.character(block_edgelist$WeightRecode)
  block_edgelist$WeightRecode <-as.numeric(block_edgelist$WeightRecode)

  colnames(block_edgelist)[1:2] <- c("sender", "receiver")

  block_edgelist
}