prop_same_function <- function(alt.attr, ego.attr){

  # Arguments:
  # alt.attr: alter attributes for a given ego
  # ego.attr: ego attributes

  # taking ego attribute and comparing to alter 
  # attributes, summing up number of times they match
  # ignoring missing data:
  same <- sum(ego.attr == alt.attr, na.rm = T) 

  # calculating proportion of ego-alter pairs that match:
  # just for alters with no missing data
  prop_same <- same / sum(!is.na(alt.attr))
  
  # making sure if ego is missing, then prop_same is also missing
  prop_same[is.na(ego.attr)] <- NA
  
  return(prop_same) 
} 
