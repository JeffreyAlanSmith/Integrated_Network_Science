create_alter_data <- function(egonets, netsize, var_names, egoID){
  # Arguments: 
  # egonets: ego network data frame
  # netsize: vector of ego network size
  # var_names: names of variables to put on alter data frame (assumed to be
  # var.name1, var.name2, for each alter)
  # egoID: name of id of ego on ego network data frame

  # creating empty list to hold output for each variable:
  overall_list <- list() 

  # taking out the isolates as they have no alters:
  egonets_noisolates <- egonets[netsize > 0, ] 
  
  # redefining network size after taking out isolates:
  netsize_updated <- netsize[netsize > 0] 

  # running over each variable name:
  for (p in 1:length(var_names)){
  var_list <- list()
  alter_list <- list()
  
  # running over each ego:
    for (x in 1:nrow(egonets_noisolates)){
      # getting alter id number:
          alter_nums <- rep(1:netsize_updated[x], times = 1)
      
      # Now we grab the alter columns for that ego and put those 
      # values in a larger list so we can stack them later on 
      # into one big vector (one for each variable of interest)
          alt_cols <- paste(rep(var_names[p], each = netsize_updated[x]), 
                            alter_nums, sep = "")

          var_list[[x]] <- egonets_noisolates[x, alt_cols]
          
          alter_list[[x]] <- alter_nums
    }
  
  # stacking all alter values into one long column
  var <- unlist(var_list) 
  overall_list[[p]] <- var
  } 

  # putting all new variables together:
  dat <- data.frame(do.call(data.frame, overall_list)) 

  # putting useful column names on data frame
  colnames(dat) <- var_names 

  # adding egoID and alterID to data frame:
  dat <- cbind(rep(egonets_noisolates[, egoID], netsize_updated), 
          unlist(alter_list), dat) 

  colnames(dat)[1:2] <- c(egoID, "alterID")

  return(dat)
}
