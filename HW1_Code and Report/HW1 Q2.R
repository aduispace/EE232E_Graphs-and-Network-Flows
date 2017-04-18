
### Problem2(a)
## plot degree distribution
library("igraph")
g1 <- barabasi.game(1000, directed = FALSE)

degreeVector1 <- degree(g1)
# plot the pic by row = 1, col = 2
par(mfrow = c(1,2))
# Ploting main distribution and log distribution
h1 <- hist(degreeVector1, breaks = 50, main = "Histogram Degree Distribution", xlab = "Degree", ylab = "Density", freq = FALSE)
h2 <- plot(degree.distribution(g1), log = "xy", main = "Scatterplot Degree Distribution in Log", xlab = "Degree", ylab="Density")

## calculate the diameter
Connectivity <- 0
Diameter <- 0
for (i in 1:100) {
  g <- barabasi.game(1000, directed=FALSE)
  Connectivity <- c(Connectivity, is.connected(g))
  Diameter <- c(Diameter, diameter(g))
}
# verify if all the graphs are connected and calculate the mean diameter
Con_avg <- mean(Connectivity)
Dia_avg <- mean(Diameter)
print(Dia_avg)

### Problem2(b)
## Find Giant Clustered Component
cl1 <- clusters(g1)
gccIndex <- which.max(cl1$csize)
nonGccNodes <- (1:vcount(g1))[cl1$membership != gccIndex]
gcc <- delete.vertices(g1, nonGccNodes)
fg <- fastgreedy.community(gcc)
mod <- modularity(fg)
## Print out modularity and plot the gcc/g
print(mod)
par(mfrow = c(1,2))
plot(g1)
plot(gcc)

### Problem2(c)
## The same as previous one with 10000 nodes
g2 <- barabasi.game(10000,directed=FALSE)
cl2 <- clusters(g2)
gccIndex2 <- which.max(cl2$csize)
nonGccNodes2 <- (1:vcount(g2))[cl2$membership != gccIndex2]
gcc2 <- delete.vertices(g2, nonGccNodes2)
fg2 <- fastgreedy.community(gcc2)
mod2 <- modularity(fg2)

print(mod2)

### Problem2(d)
## randomly picked 1000 nodes i and its corresponding neighbor j and plot the distribution
Degree <- 0
for (i in 1:1000){
  ran <- sample(1:vcount(g1),1)
  neib <- neighbors(g1, ran)
  if (length(neib) == 1) {
    picked = neib
  } else {
    picked = sample(neib, 1)
  }
  Degree <- c(Degree,degree(g1, picked))
}
plot(density(Degree), main='Random Neighbor Degree distribution', xlab='Degree', ylab='Density')
