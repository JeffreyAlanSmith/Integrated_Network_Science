count_triads_function=function(dat, triad_names){#inputs are the network and the name of the triad type
triads=triad.census(dat) #calculating the triad census
triads[1, triad_names] #grabbing counts of triad of interest
}