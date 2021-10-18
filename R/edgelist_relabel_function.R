edgelist_relabel_function <- function(edgelist, ids){
  
  edgelist_simple <- data.frame(sender = as.numeric(factor(edgelist[, 1], 
                                                             levels = ids)), 
                                receiver = as.numeric(factor(edgelist[, 2], 
                                                             levels = ids))) 
  return(edgelist_simple)  
}