# HW3_problem1
library("igraph")
library("netrw")

mydata <- read.table("C:/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/HW3/sorted_directed_net.txt")
g1 <- graph.data.frame(mydata)
### if connected?
connectivity = is.connected(g1)
### find gcc 
cl1 <- clusters(g1, mode = "strong")
gccIndex <- which.max(cl1$csize) ## return the index of maximun cluster ID
nonGccNodes <- (1:vcount(g1))[cl1$membership != gccIndex]
nums <- vcount(g1)
gcc <- delete.vertices(g1, nonGccNodes)
# plot(gcc)


# HW3_problem2
in_degree <- degree(gcc, mode = "in", loops = TRUE, normalized = TRUE)
out_degree <- degree(gcc, mode = "out", loops = TRUE, normalized = TRUE)
plot( in_degree, xlab = "degree", ylab = "frequency", main = "In-degree Distribution of Giant Connected Component", type = "h")
plot(out_degree, xlab = "degree", ylab = "frequency", main = "Out-degree Distribution of Giant Connected Component", type = "h" )


