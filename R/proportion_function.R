proportion_function <- function(communities, attribute){ 
  
  # Arguments:
  # communities: vector of community membership
  # attribute: attribute vector

  # Here we calculate the proportion that fall into each category 
  # found in attribute. We first do a table and then find the proportion 
  # in each category. This is done for each community (using tapply 
  # over the communities).
  
  dat <- tapply(factor(attribute), communities, 
             function(x) {y <-  table(x); y / sum(y)})

  # We then output it as a matrix using do.call
  return(do.call(rbind, dat))
} 