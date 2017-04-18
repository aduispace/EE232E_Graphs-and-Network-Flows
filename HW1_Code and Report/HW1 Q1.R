library("igraph") 

#1(a)
p <- c(0.01, 0.05, 0.1) 
nodes <- 1000

graph_1 <- random.graph.game(n = nodes, p = p[1],directed = FALSE)
graph_2 <- random.graph.game(n = nodes, p = p[2],directed = FALSE)
graph_3 <- random.graph.game(n = nodes, p = p[3],directed = FALSE)

degree_1 <- degree(graph = graph_1)
degree_2 <- degree(graph = graph_2)
degree_3 <- degree(graph = graph_3)

degree_dist_1 <- hist(x = degree_1, breaks = seq(from = min(degree_1), to = max(degree_1), by=1))
degree_dist_2 <- hist(x = degree_2, breaks = seq(from = min(degree_2), to = max(degree_2), by=1))
degree_dist_3 <- hist(x = degree_3, breaks = seq(from = min(degree_3), to = max(degree_3), by=1))


#1(b)
connect1 = connect2 = connect3 = diameter1 = diameter2 = diameter3 = 0;
for (i in 1:50) {
  g1 <- erdos.renyi.game(1000, 0.1, type="gnp", directed = FALSE)
  g2 <- erdos.renyi.game(1000, 0.05, type="gnp", directed = FALSE)
  g3 <- erdos.renyi.game(1000, 0.01, type="gnp", directed = FALSE)
  connect1 = c(connect1, is.connected(g1))
  connect2 = c(connect1, is.connected(g2))
  connect3 = c(connect1, is.connected(g3))
  diameter1 = c(diameter1, diameter(g1))
  diameter2 = c(diameter2, diameter(g2))
  diameter3 = c(diameter3, diameter(g3))
}
Connect1 <- mean(connect1)
Connect2 <- mean(connect2)
Connect3 <- mean(connect3)
Diameter1 <- mean(diameter1)
Diameter2 <- mean(diameter2)
Diameter3 <- mean(diameter3)



#1(c)
pc <- 0
for (i in 1:50) {
  for (p in seq(from = 0, to = 0.0100, by = 0.0001)) {
    g = random.graph.game(1000, p, directed = FALSE)
    if (is.connected(g)) 
      break
  }
  pc <- c(pc, p)
}

mean(pc)
