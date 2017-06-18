library("igraph")
filePath = "C:/Users/lindu/Downloads/facebook_combined.txt/facebook_combined.txt"

#################---------Problem 1-----------------######################

## determine if connected, measure the d of network, plot the dg
g <- read.graph(file = filePath, format = "ncol", directed = FALSE)
connectivity <- is.connected(g)
d <- diameter(g)
dg <- degree.distribution(g)
## plot(g, vertex.size=0, vertex.label=NA, edge.arrow.size=0 )
plot(dg, type = "h", main = "Problem 1 Degree Distribution", xlab = "degree", ylab = "density")

## fit a curve

len <- max(degree(g))
x <- 0:len
y <- dg

### Option1:
lo <- loess(y~x)
plot(x,y, main = "Option 1 Fitting curves")
lines(predict(lo), col='red', lwd=2)

### Option2: 
smoothingSpline = smooth.spline(x, y, spar=0.35)
plot(x,y, main = "Option 2 Fitting curves")
lines(smoothingSpline, col='blue', lwd = 2)

### Calculate the mean squared error

h = hist(degree(g), breaks=seq(0, by=1 , length.out=max(degree(g))+2))
dat = data.frame(x=h$mids, y=h$density)
mse1 <- sum((predict(lo)-dat$y)^2)/max(degree(g))
mse2 <- sum((smoothingSpline$y-dat$y)^2)/max(degree(g))
avg_degree = mean(degree(g))


#################---------Problem 2-----------------######################

g <- read.graph(file = filePath, format = "ncol", directed = FALSE)
g_node1 = induced.subgraph(g, c(1, neighbors(g,1)))
vertexvector = rep(3,vcount(g_node1))
vertexvector[1]=5
vertexcolor = rep("magenta",vcount(g_node1))
vertexcolor[1] ="black"
plot.igraph(g_node1,vertex.size=vertexvector,vertex.label =NA,vertex.color=vertexcolor, main = "personal network of node 1")
n_node = vcount(g_node1)
n_edge = ecount(g_node1)


#################-------------Problem 3-------------######################

core_index = numeric(0)
core_degree = numeric(0)
for(i in 1: length(degree(g))){
  if(length(neighbors(g,i))>200){
    core_index = c(core_index, i)
    core_degree = c(core_degree, length(neighbors(g,i)))
  }
}
core_ave_degree = mean(core_degree)

fg = fastgreedy.community(g_node1)
color_vec = fg$membership+1
plot(g_node1,vertex.color=color_vec,vertex.label=NA,vertex.size=5, main = "Community Structure by Fast-greedy Algorithm")
eb = edge.betweenness.community(g_node1)
color_vec = eb$membership+1
plot(g_node1,vertex.color=color_vec,vertex.label=NA,vertex.size=5, main = "Community Structure by Edge-betweenness Algorithm")
ic = infomap.community(g_node1)
color_vec = ic$membership+1
plot(g_node1,vertex.color=color_vec,vertex.label=NA,vertex.size=5, main = "Community Structure by Infomap Algorithm")

hist(fg$membership, col="red", main="Community distribution of Fast-Greedy Algorithm", xlab="community number", ylab="number of nodes")
hist(eb$membership, col="yellow", main="Community distribution of Edge-Betweenness Algorithm", xlab="community number", ylab="number of nodes")
hist(ic$membership, col="blue",main="Community distribution of Infomap Algorithm",xlab="community number",ylab="number of nodes")

#################-------------Problem 4-------------######################

g_node1_removed = induced.subgraph(g,neighbors(g,1))

fg2 = fastgreedy.community(g_node1_removed)
color_vec = fg2$membership+1
plot(g_node1_removed, vertex.color=color_vec, vertex.label=NA, vertex.size=5, main = "Community Structure by Fast-greedy Algorithm")
eb2 = edge.betweenness.community(g_node1_removed)
color_vec = eb2$membership+1
plot(g_node1_removed, vertex.color=color_vec,vertex.label=NA, vertex.size=5, main = "Community Structure by Edge-betweenness Algorithm")
ic2 = infomap.community(g_node1_removed)
color_vec = ic2$membership+1
plot(g_node1_removed, vertex.color=color_vec,vertex.label=NA, vertex.size=5, main = "Community Structure by Infomap Algorithm")

hist(fg2$membership, col="red", main="Community distribution of Fast-Greedy Algorithm", xlab="community number", ylab="number of nodes")
hist(eb2$membership, col="yellow", main="Community distribution of Edge-Betweenness Algorithm", xlab="community number", ylab="number of nodes")
hist(ic2$membership, col="blue",main="Community distribution of Infomap Algorithm",xlab="community number",ylab="number of nodes")