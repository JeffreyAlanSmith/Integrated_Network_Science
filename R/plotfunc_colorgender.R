plotfunc_colorgender <- function(nets){ 
  # Arguments: 
  # nets: ego network of interest
  
  # extracting the attribute from the ego network:
  cols <- vertex_attr(nets, "SEX") 

  # now we use an ifelse statement to set color,
  # light sky blue if gender equals female, blue otherwise:
  cols <- ifelse(cols == "female", "lightskyblue", "blue")

  # plotting ego network with nodes colored based on gender:
  plot(nets, vertex.color = cols) 
}