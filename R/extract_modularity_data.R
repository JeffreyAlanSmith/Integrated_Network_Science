extract_modularity_data=function(communities, graph){

mems_list=list() #list where membership information will be saved
num_communities=NA #vector where number of communities will be saved
modularities=NA #vector to store modularity scores

#Extracting levels of aggregation, or steps,
#in the hierarchical merge data.
num_merges=0:nrow(communities$merges)

#Note that we start from 0 as the first level
#corresponds to all nodes in their own community,
#and that does not have a row in the  merge data frame.

#Looping through each level of aggregation

for (x in 1:length(num_merges)){
  
  #We first extract the membership 
  #information at the given merge level using a 
  #cut_at function. The inputs are the community
  #object and the merge step of interest. 
  
  mems_list[[x]]=cut_at(communities, steps=num_merges[x])

  #Now we calculate the number of communities associated
  #with the given clustering solution: 
  num_communities[x]=length(unique(mems_list[[x]]))

  #Let's also calculate the modularity score, just to make sure
  #we get the right value for that set of community memberships:
  modularities[x]=modularity(graph, mems_list[[x]])
    }

#We will now put together our extracted 
#information in a data frame. 

plot_data=data.frame(modularity=modularities, 
                     num_communities=num_communities)

#Let's reorder to go from low number of communities to high:
mems_list=mems_list[order(plot_data$num_communities)]
plot_data=plot_data[order(plot_data$num_communities),] 
rownames(plot_data)=1:nrow(plot_data)

#outputting resuts in a list:
list(summary_data=plot_data, membership_list=mems_list)
}
