create_alteralter_data <- function(egonets, netsize, aa_tie_data, 
                                     egoID, max_alter){
  # Arguments:
  # egonets: ego network data frame 
  # netsize: vector of ego network size 
  # aa_tie_data : data for each ego showing ties between alters; 
  # assumed to be ordered by 1-2; 1-3; 1-4; 1-5, etc. on the columns
  # egoID: name of id of ego on ego network data frame
  # max_alter: maximum of number of alter of which alter-alter tie 
  # data was reported on

  overall_list <- list()
  
  # taking out the isolates and those
  # with only one alter as they have no alter-alter ties 
  egonets_noisolates <- egonets[netsize > 1, ] 
  
  # also taking out the 
  # isolates and those with only one alter 
  # for the alter-alter tie input data
  alteralter_tie_data <- aa_tie_data[netsize > 1, ] 

  # redefining network size after taking out isolates:
  netsize_updated <- netsize[netsize > 1] 
  
  # defining possible alter-alter ties
  alter_ids <- t(combn(max_alter, 2)) 
  
  # running over each ego:
  for (x in 1:nrow(egonets_noisolates)){
    
  # First we create a data frame based on the ego ids, the possible 
  # alter-alter ties and the weights for each alter-alter tie, 
  # based on the  input data for that ego
    alter_dat_row <- data.frame(egoID = egonets_noisolates[x, egoID], 
                                alter_ids, 
                                weight = unlist(alteralter_tie_data[x, ]))
    
  # Here we reduce some of the rows (corresponding to alter-alter ties) 
  # if ego had less than the max number of alters or if some 
  # of the alter-alter ties are not present (assumed if value 
  # is equal to 0 or NA)
    alter_dat_row <- alter_dat_row[alter_dat_row[, 2] <= netsize_updated[x] & 
                              alter_dat_row[, 3] <= netsize_updated[x] & 
                              !is.na(alter_dat_row$weight) & 
                              alter_dat_row$weight != 0, ]

    overall_list[[x]] <- alter_dat_row
  }

  #putting all alter-alter ties, by ego, in one data frame:
  alter_alter_dat <- do.call(rbind, overall_list) 

  #putting useful column names on the data frame:
  colnames(alter_alter_dat) <- c(egoID, "source", "target", "weight")
  rownames(alter_alter_dat) <- 1:nrow(alter_alter_dat)
  
  return(alter_alter_dat)
}