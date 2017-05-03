library(igraph) 
library(netrw)

randomwalk = function(node){
  random_network <- barabasi.game(n = node, directed = FALSE)
  cat("Diameter of network with", node, "nodes = ", diameter(random_network))
  
  averagestep = numeric()
  averagestandarddeviation = numeric()
  distancematrix = shortest.paths(random_network, v = V(random_network), to = V(random_network))
  degofrandomwalk = numeric()
  
  for (t in 1:100) {  
    distance = numeric()
    vertexsequence = netrw(random_network, walker.num = node, damping = 1, T = t, output.walk.path = TRUE)$walk.path # get vertex sequence of random walk
    for(n in (1:node))
    {
      startvertex = vertexsequence[1,n]
      tailvertex = vertexsequence[t,n]
      shortestdistance = distancematrix[startvertex, tailvertex]
      if (shortestdistance == Inf) {
        shortestdistance = 0
      }
      distance = c(distance, shortestdistance)
      degofrandomwalk = c(degofrandomwalk, degree(random_network, v = tailvertex))  
    }
    
    averagestep = c(averagestep, mean(distance))
    averagestandarddeviation = c(averagestandarddeviation, mean((distance - mean(distance))**2))
  }
  
  
  plot(averagestep, typ='l', main = paste("Average Steps vs. t - ", n, "nodes"), xlab = "t", ylab = "Average Steps")
  plot(averagestandarddeviation, typ='l', main = paste("Average Standard Deviation vs. t - ", n, "nodes"), xlab = "t", ylab = "Average Standard Deviation")
  
  if (node == 1000) {
    degofnetwork = degree(random_network)
    hist(x = degofnetwork, breaks = seq(from = min(degofnetwork), to = max(degofnetwork), by=1), main = "Degree Distribution for Random Undirected Graph (with n=1000)", xlab = "Number of Degrees")
    hist(x = degofrandomwalk, breaks = seq(from = min(degofrandomwalk), to = max(degofrandomwalk), by=1), main = "Degree Distribution at end of Random Walk", xlab = "Number of Degrees")
  }
}


cat("Executing for Random Network with 1000 nodes")
randomwalk(node = 1000)
cat("Executing for Random Network with 100 nodes")
randomwalk(node = 100)
cat("Executing for Random Network with 10000 nodes")
randomwalk(node = 10000)