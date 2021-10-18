dyad_mat_function <- function(dat){
  # Arguments:
  # dat: network object

  # getting matrix form of network
  mat <- as.matrix(dat) 
  
  # putting NA diagonal as we don’t want to consider self-ties
  diag(mat) <- NA 

  # Next, we do a little trick where we take the matrix and 
  # add it to its transpose, yielding a matrix of 0s, 1s 
  # and 2s. If it is null, the resulting value will be 0 
  # (neither ij nor ji exists); if it is asymmetric there
  # will be a 1 (as ij or ji exists but not both); and 
  # if it is mutual there will be a 2 (as there are ties 
  # from ij and ji).
  dyad_mat <- mat + t(mat) 

  # Now we label for ease of interpretation:
  dyad_mat[dyad_mat == 0] <- "null" 
  dyad_mat[dyad_mat == 1] <- "asym"
  dyad_mat[dyad_mat == 2] <- "mut"
  
  return(dyad_mat)
}
