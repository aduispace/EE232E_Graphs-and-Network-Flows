community_option_2a_100 = which(sizes(community_option_2a)>100)
community_option_2b_100 = which(sizes(community_option_2b)>100)

modularity_100_2a = c()
sizes_100_2a = c()
for (i in 1:length(community_option_2a_100)) {
  index_nodes = (1:vcount(undirected_option_2))[community_option_2a$membership != community_option_2a_100[i]]
  GCC_nodes = delete.vertices(undirected_option_2, index_nodes)
  sub_GCC = fastgreedy.community(GCC_nodes)
  modularity_100_2a = c(modularity_100_2a, c(modularity(sub_GCC)))
  sizes_100_2a = c(sizes_100_2a, sizes(sub_GCC))
}
modularity_100_2a
sizes_100_2a

modularity_100_2b = c()
sizes_100_2b = c()
for (i in 1:length(community_option_2b_100)) {
  index_nodes = (1:vcount(undirected_option_2))[community_option_2b$membership != community_option_2b_100[i]]
  GCC_nodes = delete.vertices(undirected_option_2, index_nodes)
  sub_GCC = fastgreedy.community(GCC_nodes)
  modularity_100_2b = c(modularity_100_2b, modularity(sub_GCC))
  sizes_100_2b = c(sizes_100_2b, sizes(sub_GCC))
}
modularity_100_2b
sizes_100_2b