multi_community = numeric(0)
# visit probability of each node
for (i in 1:vcount(directed_network)) {
  teleportation_prob = rep(0, vcount(directed_network))
  teleportation_prob[i] = 1 # local page-rank 
    
  page_rank = netrw(directed_network , walker.num = 1, start.node = i,
                    damping = 0.85, teleport.prob = teleportation_prob, output.visit.prob = TRUE) # random walk on node_i
  
  visit_prob = page_rank$ave.visit.prob
  top_visit_prob = sort(visit_prob, decreasing = TRUE, index.return = TRUE) # sort the visit_prob and keep index
  M_i = rep(0, length(community_option_2a)) 
  
  for (j in 1:30) { # loop through only top 30 visit_prob
    m_j = rep(0, length(community_option_2a))
    m_j[community_option_2a$membership[which(V(giant_connected_component) == V(directed_network)[top_visit_prob$ix[j]])]] = 1
    M_i = M_i + top_visit_prob$x[j] * m_j
  }
  
  if (length(which(M_i > 0.1)) >= 2) { # change threshold and see if there are more than 2 communities with this value
    multi_community = rbind(multi_community, c(i, M_i)) # store results and community score
  }
}
multi_community