
## prob5 solution, note it could use previous data 

g1_mst_dir = as.directed(g1_mst, 'mutual')
g1_double_mst = as.undirected(g1_mst_dir, 'each' )

el = get.edgelist(g1_double_mst)
v3 = as.vector(E(g1_double_mst)$weights)
out = cbind(el, v3)

#Read in the results from python
tsp_python = read.table('/Users/dui/Google Drive/XPS/Spring 2017 Courses/EE232E Graphs Mining/HW4/tsp.txt')
tsp_path_names = as.vector(tsp_python[,1])

node_names = V(g1)$name
tsp_path_idx = numeric()
for(i in 1:length(tsp_path_names)){
  matches = which(tsp_path_names[i] == node_names)
  tsp_path_idx = c(tsp_path_idx,matches)
}

adjacency_matrix = get.adjacency(g1, attr = 'weights',sparse = FALSE, names = FALSE)

#Calculate weight of travelling salesman path
tsp_weight = 0
for(i in 2:length(tsp_path_idx)){
  tsp_weight = tsp_weight + adjacency_matrix[tsp_path_idx[i],tsp_path_idx[i-1]]
}

lower_bound = sum(E(g1_mst)$weights)
upper_bound = sum(E(g1_double_mst)$weights)



## Prob5 Bonus

library('TSP')

tsp_object = TSP(adjacency_matrix)
tsp_package_path = labels(tsp_object)
tsp_package_path_idx = numeric()
for(i in 1:length(tsp_package_path)){
  matches = which(tsp_package_path[i] == node_names)
  tsp_package_path_idx = c(tsp_package_path_idx,matches)
}


tsp_package_weight = 0
for(i in 2:length(tsp_package_path_idx)){
  tsp_package_weight = tsp_package_weight + adjacency_matrix[tsp_package_path_idx[i],tsp_package_path_idx[i-1]]
}
cat("Sum of edge weights of external package TSP solution: ", tsp_package_weight,"\n")
