##### QUESTION 3 #####
# convert directed to undirected graph
# option 1 : keep the number of edges unchanged, and just remove the directions

undirected_option_1 = as.undirected(giant_connected_component, mode = "each")
community_option_1 = label.propagation.community(undirected_option_1, weights = E(undirected_option_1)$weights)
modularity(community_option_1)
sizes(community_option_1)

# option 2 : merge the two directed edges between i and j
sqrt_weights = function(weight) sqrt(prod(weight))

undirected_option_2 = as.undirected(giant_connected_component, mode = "collapse", edge.attr.comb = sqrt_weights)

community_option_2a = label.propagation.community(undirected_option_2, weights = E(undirected_option_2)$weights)
modularity(community_option_2a)
sizes(community_option_2a)

community_option_2b = fastgreedy.community(undirected_option_2, weights = E(undirected_option_2)$weights)
modularity(community_option_2b)
sizes(community_option_2b)

##### QUESTION 4 #####
# Sub Giant Connected Component
index_sub_GCC = which.max(sizes(community_option_2b))
non_sub_GCC_nodes = (1:vcount(undirected_option_2))[community_option_2b$membership != index_sub_GCC]
sub_giant_connected_component = delete.vertices(undirected_option_2, non_sub_GCC_nodes)

community_sub_GCC = fastgreedy.community(sub_giant_connected_component, weights=E(sub_giant_connected_component)$weights)
modularity(community_sub_GCC)
sizes(community_sub_GCC)