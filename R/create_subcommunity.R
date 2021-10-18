create_subcommunity <- function(graph, initial_communities, community_number){
  
  # Arguments:
  # graph: igraph object
  # initial_communities: the original community memberships
  # community_number: the community number of interest (i.e., the
  # community that you want to divide further).

  # here will create a subgraph of just the community of interest
  in_community <- which(initial_communities == community_number)
  subgraph1 <- induced_subgraph(graph = graph,
                                vids = in_community) 

  # We now perform a community detection algorithm (using fast and greedy) 
  # on the subgraph
  comm1 <- cluster_fast_greedy(graph = subgraph1) 

  # grabbing the community membership of each 
  # person in the subgraph
  mems_subgraph1 <- membership(comm1) 

  # Now we grab the ids of those in the subgraph, so we can map them back 
  # onto the original, full network 
  ids_map <- as.numeric(vertex_attr(subgraph1, "name")) 

  mems_new <- initial_communities # just copying the original communities

  # Here, we begin to relabel the communities so we can put 
  # them back onto the original set of communities on 
  # the full network. We want to make sure that 
  # these new community ids are unique, so we take the max
  # original community number and add that to the community
  # ids on the subgraph.

  mems_subgraph1_relabel <- mems_subgraph1 + max(initial_communities) 

  # Here we put the new communities onto a vector of community 
  # membership corresponding to the whole network. 
  mems_new[ids_map] <- mems_subgraph1_relabel 

  # Note we just change those in the subgraph of interest.

  # We can then relabel all communities, if desired, to take out the old 
  # community number and put in order from low to high:
  num_comms_new <- length(unique(mems_new))
  mems_updated <- as.numeric(as.character(factor(mems_new, 
                                                 labels = 1:num_comms_new)))

  # here we output the subgraph, the membership and the updated
  # vector of community membership:
  return(list(subgraph = subgraph1, 
              mems_subgraph = mems_subgraph1, 
              membership_updated = mems_updated))
}