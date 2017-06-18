library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 4 #####

jaccard.weights = fread("jaccard_edge_weight.txt", header = FALSE, data.table = TRUE)  # created using Python
jaccard.weights$V2 = NULL
jaccard.weights$V4 = NULL
colnames(jaccard.weights) = c("Node 1", "Node 2", "weights")
movies.network = graph.data.frame(jaccard.weights, directed = FALSE)
movies.network <- simplify(movies.network)
##### END OF QUESTION 4 #####ß

