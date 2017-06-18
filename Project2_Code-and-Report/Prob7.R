library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)
movie.ratings = fread("movie_rating.txt", header = FALSE, sep = "\t")
movie.ratings$V2 = NULL
Encoding(movie.ratings$V1) = 'latin1'
movie.ratings$V1 = iconv(movie.ratings$V1, "latin1", "ASCII", sub="")
movie.ratings$V1 = gsub("^\\(", "", movie.ratings$V1)
movie.ratings$V1 = gsub("\\([^[:digit:]]+\\)", "", movie.ratings$V1)
movie.ratings$V1 = gsub("^\\s+|\\s+$", "", movie.ratings$V1)
movie.name.greater.5 = unlist(fread("movie-list-greater-5.txt", header = FALSE))
movie.ratings.greater.5 = fread("movie-list-greater-5.txt", header = FALSE)
node.not.there = c(89967, 230839)  # nodes not present in the network but have actors > 5
movie.ratings.greater.5 = movie.ratings.greater.5[-node.not.there,]
movie.name.greater.5 = movie.name.greater.5[-node.not.there]
movies.network$ratings = movie.ratings.greater.5
tag.batman = which(movie.name.greater.5 == "Batman v Superman: Dawn of Justice (2016)")
tag.mission = which(movie.name.greater.5 == "Mission: Impossible - Rogue Nation (2015)")
tag.minions = which(movie.name.greater.5 == "Minions (2015)")
tag = c(tag.batman, tag.mission, tag.minions)
k = 20
names.q = c("Batman v Superman: Dawn of Justice (2016)", "Mission: Impossible - Rogue Nation (2015)",  "Minions (2015)")
actual.q = c(6.7,7.4,6.4)
for (m in 1:3) {
  community = movies.network.community$membership[tag[m]]
  community.member.ratings = movies.network$ratings[which(movies.network.community$membership == community)]
  community.member.ratings = community.member.ratings[which(community.member.ratings != 0)]
  edges.node = jaccard.weights[which(jaccard.weights$"Node 1" == tag[m] | jaccard.weights$"Node 2" == tag[m]),]
  edges.node =  edges.node[order(edges.node$"weights"),]
  top.neighbor = data.frame(edges.node[1:k,])
  top.neighbor$"weights" = NULL
  node.id = c()
  for (i in 1:k){
    node.id = c(node.id,top.neighbor[i,which((top.neighbor[i,])!=tag[m])])
  }
  nearest.neighbor.rating = movie.ratings.greater.5[node.id]
  nearest.neighbor.rating = nearest.neighbor.rating[which(nearest.neighbor.rating != 0)]
  neigbhors = as.numeric(unlist(neighbors(movies.network, tag[m])))
  neighbors.ratings = movie.ratings.greater.5[neigbhors]
  neighbors.ratings = neighbors.ratings[which(neighbors.ratings != 0)]
  ratings.movie.q = c(as.numeric(unlist(community.member.ratings)), as.numeric(unlist(nearest.neighbor.rating)), as.numeric(unlist(neighbors.ratings)))
  cat(names.q[m], actual.q[m],"\n")
  cat("Community Rating ", mean(as.numeric(unlist(community.member.ratings))), "\n")
  cat("Nearest Neighbor Rating (k=20) ", mean(as.numeric(unlist(nearest.neighbor.rating))), "\n")
  cat("Neighbors Rating", mean(as.numeric(unlist(neighbors.ratings))), "\n")
  cat("All Combined ", mean(ratings.movie.q), "\n")
  cat("\n")
}




dir <- dirname(sys.frame(1)$ofile)
setwd(dir)
g<-read.graph("/Users/????/Documents/jaccard_edge_weight.txt",format="ncol",directed=FALSE)
movie_interest <- c("Batman v Superman: Dawn of Justice (2016)", "Mission: Impossible - Rogue Nation (2015)",
                   "Minions (2015)")
neighbor<-list()
edgeW<-list()
comId<-rep(0,3)
for(i in 1:3){
  nodeID<-(1:vcount(g))[V(g)$movieName==movie_interest[i]]
  tmp<-ego(g,1,V(g)[nodeID])
  neighbor[[i]]<-tmp[[1]][2:length(tmp[[1]])]
  print(movie_interest[i])
  edge_weight <- rep(0,length(neighbor[[i]]))
  for(j in 1:length(neighbor[[i]])){
    edge_weight[j] <- g[from=nodeID,to=neighbor[[i]][j]]
  }
  names(edge_weight)<-neighbor[[i]]
  edgeW[[i]]<- edge_weight
  edge_weight <- sort(edge_weight,decreasing=TRUE)
  neighbor_name<-as.numeric(names(edge_weight[1:5]))
  neighbor_name<-V(g)[neighbor_name]$movieName
  comId[i]<-com$membership[nodeID]
}
for(i in 1:3) {
  neigh_rate<-V(g)[neighbor[[i]]]$Rate
  neigh_rate<-as.numeric(neigh_rate)
  neigh_rate<-neigh_rate[which(neigh_rate!=0)]
  comNode<-(1:vcount(g))[com$membership==comId[i]]
  com_rate<-V(g)[comNode]$Rate
  com_rate<-as.numeric(com_rate)
  com_rate<-com_rate[which(com_rate!=0)]
  r1<-mean(neigh_rate)
  r2<-mean(com_rate) 
  edgeW[[i]]<-edgeW[[i]]/sum(edgeW[[i]])
  r1_p<-as.numeric(V(g)[neighbor[[i]]]$Rate)*edgeW[[i]]
  r1_p <- sum(r1_p[which(r1_p != 0)])
  r_simple<-0.5*r1+0.5*r2 
  print(movie_interest[i])
  print(r_simple)
}


