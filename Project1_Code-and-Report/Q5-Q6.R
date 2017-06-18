library(igraph)
library(Matrix)
library(ggplot2)
library(hash)
library(fit.models)
filePath = "C:/Users/lindu/Downloads/facebook_combined.txt/facebook_combined.txt"

g <- read.graph(file = filePath , format = "ncol" , directed=FALSE)



#############    Data Preprocessed    ##############
core_Nodes <- which(neighborhood.size(g, 1, nodes=V(g)) > 200)
no_of_coreNodes <- length(core_Nodes)
degree_g <- degree(g)
ave_degree <- mean(degree_g[core_Nodes])
# sum_coreNodes <- 0
# for(i in 1:no_of_coreNodes){
#  sum_coreNodes <- sum_coreNodes+degree_g[core_Nodes[i]]
# }
# average_degree <- sum_coreNodes/no_of_coreNodes
core <- core_Nodes[1]
subgraph_coreNodes <- neighborhood(g, 1, nodes=V(g)[core])
subgraph_coreNodes <- subgraph_coreNodes[[1]]
nonsubgraph_coreNodes <- which( !( (1:vcount(g)) %in% subgraph_coreNodes)  )
subgraph_core <- delete.vertices(g , nonsubgraph_coreNodes)
# plot(subgraph_core, vertex.label=NA, vertex.size=5, main="Community structure of the core node 1")

vertexSizeVector = rep(3,vcount(subgraph_core))
vertexSizeVector[V(subgraph_core)$name==V(g)[core]$name] = 5
# find communities
community_1 <- fastgreedy.community(subgraph_core)
#cmsize <- sizes(community_1)
#cmsize <- as.vector(sizes(community_1))
modularity(subgraph_core,membership(community_1))
#V(subgraph_core)$color <- as.vector(V(subgraph_core)[community_1$membership])
#plot(community_1, subgraph_core, vertex.label=NA, vertex.size=vertexSizeVector, main="Community structure of the core node 1 - Fast-Greedy")

community_2 <- edge.betweenness.community(subgraph_core)
modularity(subgraph_core,membership(community_2))
#plot(community_2, subgraph_core, vertex.label=NA, vertex.size=vertexSizeVector, main="Community structure of the core node 1 - Edge-Betweenness")

community_3 <- infomap.community(subgraph_core)
modularity(subgraph_core,membership(community_3))
#plot(community_3, subgraph_core, vertex.label=NA, vertex.size=vertexSizeVector, main="Community structure of the core node 1 - Infomap community")

#hist(community_1$membership, col="red", main="Community distribution of Fast-Greedy Algorithm", xlab="community number", ylab="number of nodes")
#hist(community_2$membership, col="yellow", main="Community distribution of Edge-Betweenness Algorithm", xlab="community number", ylab="number of nodes")
#hist(community_3$membership,col="blue",main="Community distribution of Edge-Betweenness Algorithm",xlab="community number",ylab="number of nodes")


#############     PROBLEM 5    ##############

commNeib_find <- function(u,v,g)
{
  neighborsU <- neighborhood(g,1,u)[[1]][-1]
  neighborsV <- neighborhood(g,1,v)[[1]][-1]
  intersect(neighborsU, neighborsV)
}

embd_find <- function (u,v,g)
{
  emdb = length(commNeib_find(u,v,g))
  emdb
}

perNet_find <- function(u, g)
{
  pnNodes <- neighborhood(g , 1 , nodes=u)[[1]]
  nonPNNodes <- which( !( (1:vcount(g)) %in% pnNodes)  )
  perNetw <- delete.vertices(g , nonPNNodes)
  perNetw$name =  sort(pnNodes)
  perNetw
}

nodeID_find <- function(g, vertex)
{
  temp <- which(g$name == vertex)
  temp
}

disp_find <- function(u,v,g) 
{
  disp <- 0
  commonUV <- commNeib_find(u, v, g)
  gNoUV <- delete.vertices(g, c(u, v))
  
  for(s in commonUV) 
  {
    for(t in commonUV) 
    {
      if(s != t) 
      {
        if(!is.na(match(s,V(gNoUV))) && !is.na(match(t,V(gNoUV))) && !are.connected(gNoUV,s,t) && length(commNeib_find(s,t,gNoUV)) == 0) {
          disp = disp + 1
        }
      }
    }
  }
  disp
}

dispEmb_find <- function(g,coreNode){
  
  dHigh = 0;
  dNode = 0;
  rHigh = 0;
  rNode = 0;
  eHigh = 0;
  eNode = 0;
  
  pnOfU <- perNet_find(coreNode,g)
  u <- nodeID_find(pnOfU, coreNode)
  
  nodes <- V(pnOfU)
  for(v in nodes){
    if(v == u)
      next
    
    dip = disp_find(u,v,g)
    embd = embd_find(u,v,g)
    
    if (embd > 0)
    {
      rt = dip/embd
      if (rt > rHigh)
      {
        rNode = v;
        rHigh=rt;
      }
    }
    
    if (dip > dHigh)
    {
      dNode = v;
      dHigh=dip;
    }
    if (embd > eHigh)
    {
      eNode = v
      eHigh=embd;
    }
    
  }
  

  if (dNode > 0)
  {
    # community detection
    fc = fastgreedy.community(pnOfU); sizes(fc)
    mfc = membership(fc)
    
    sizeVet = rep(3, length(V(pnOfU)));
    sizeVet[dNode] = 8;  
    colEd = rep(8, length(E(pnOfU)));
    colEd[which(get.edgelist(pnOfU,name=F)[,1] == dNode | get.edgelist(pnOfU,name=F)[,2] == dNode)] = 3;
    E(pnOfU)$color = colEd;
    widEd = rep(1, length(E(pnOfU)));
    widEd[which(get.edgelist(pnOfU,name=F)[,1] == dNode | get.edgelist(pnOfU,name=F)[,2] == dNode)] = 3;
    dev.new ();
    plot(pnOfU, vertex.label= NA, vertex.color=mfc,vertex.size=sizeVet, edge.width = widEd,mark.groups = by(seq_along(mfc), mfc, invisible),main="Max dispersion");
  }
  
  else
  {
    print (paste(c("No high Disp node", toString(coreNode)), collapse=" "));
  }
  

  if (eNode > 0)
  {
    # community detection
    fc = fastgreedy.community(pnOfU); sizes(fc)
    mfc = membership(fc)
    sizeVet = rep(3, length(V(pnOfU)));
    sizeVet[eNode] = 8;  
    colEd = rep(8, length(E(pnOfU)));
    colEd[which(get.edgelist(pnOfU,name=F)[,1] == eNode | get.edgelist(pnOfU,name=F)[,2] == eNode)] = 3;
    E(pnOfU)$color = colEd;
    widEd = rep(1, length(E(pnOfU)));
    widEd[which(get.edgelist(pnOfU,name=F)[,1] == eNode | get.edgelist(pnOfU,name=F)[,2] == eNode)] = 3;
    dev.new ();
    plot(pnOfU, vertex.label= NA, vertex.color=mfc,vertex.size=sizeVet, edge.width = widEd,mark.groups = by(seq_along(mfc), mfc, invisible),main="Max embeddedness");# ,mark.groups = by(seq_along(mfc), mfc) );
  }
  else
  {
    print (paste(c("No high Emb node", toString(coreNode)), collapse=" "));
  }

  if (rNode > 0)
  {
    
    # community detection
    fc = fastgreedy.community(pnOfU); sizes(fc)
    mfc = membership(fc)
    
    sizeVet = rep(3, length(V(pnOfU)));
    sizeVet[rNode] = 8;  
    colEd = rep(8, length(E(pnOfU)));
    colEd[which(get.edgelist(pnOfU,name=F)[,1] == rNode | get.edgelist(pnOfU,name=F)[,2] == rNode)] = 3;
    E(pnOfU)$color = colEd;
    widEd = rep(1, length(E(pnOfU)));
    widEd[which(get.edgelist(pnOfU,name=F)[,1] == rNode | get.edgelist(pnOfU,name=F)[,2] == rNode)] = 3;
    dev.new ();
    plot(pnOfU, vertex.label= NA, vertex.color=mfc,vertex.size=sizeVet, edge.width = widEd,mark.groups = by(seq_along(mfc), mfc, invisible) , main="Max dispersion/embeddedness");
  }
  else
  {
    print (paste(c("No high Disp node", toString(coreNode)), collapse=" "));
  }
  
}

dispVec <- c();
emb_vector <- c();


for(coreNode in core_nodes)
{
  pnOfU <- perNet_find(coreNode,g)
  u <- nodeID_find(pnOfU, coreNode)
  print(u %in% V(pnOfU))
  
  nodes <- V(g)
  for(v in nodes){
    if(v == u)
      next
    
    emdb = embd_find(u,v,g)
    dip = disp_find(u,v,g)
    emb_vector <- c(emb_vector, emdb);
    dispVec <- c(dispVec, dip);
    
  }
}



hist (emb_vector, breaks=seq (-0.5, by=1, length.out=max(emb_vector) +2), main ="Embeddedness Distribution");

hist (dispVec, breaks=seq (-0.5, by=1, length.out=max(dispVec) +2), main="Dispersion Distribution");

dispEmb_find(g,core_nodes[1])
dispEmb_find(g,core_nodes[5])
dispEmb_find(g,core_nodes[10])


#############     PROBLEM 6    ##############
type_1 <- rep(0, length(core_Nodes))
type_2 <- rep(0, length(core_Nodes))

core_Nodes <- which(neighborhood.size(g, 1, nodes=V(g)) > 200)
conductance <- list(rep(0,length(core_Nodes)))

for (i in 1:length(core_Nodes)){
  core <- core_Nodes[i]
  subgraph_coreNodes <- neighborhood(g, 1, nodes=V(g)[core])
  subgraph_coreNodes <- subgraph_coreNodes[[1]]
  nonsubgraph_coreNodes <- which( !( (1:vcount(g)) %in% subgraph_coreNodes)  )
  subgraph_core <- delete.vertices(g , nonsubgraph_coreNodes)
  
  community_fg <- fastgreedy.community(subgraph_core)
  community_2 <- edge.betweenness.community(subgraph_core)
  community_3 <- infomap.community(subgraph_core)
  
  #cmsize <- as.vector(sizes(community_fg))
  cmsize <- as.vector(sizes(community_2))
  #cmsize <- as.vector(sizes(community_3))
  
  a <- as.vector(degree(subgraph_core))
  conductance[[i]] <- rep(NA,length(cmsize))
  
  for (j in 1:length(cmsize)){
    if (cmsize[j] > 10){
      subgraph_nodes <- which(( community_fg$membership == j ))
      nonsubgraph_nodes <- which( !( community_fg$membership == j ))
      subgraph <- delete.vertices(subgraph_core, nonsubgraph_nodes)
      
      e <- length(E(subgraph))
      v <- 0
      for (k in 1:length(subgraph_nodes)){
        v <- v+a[subgraph_nodes[k]]
      }
      judge <- (v-2*e)/v
      # avoid the vector length to be 0
      if (length(judge) != 0) {
        conductance[[i]][j] <- (v-2*e)/v
      }
    }
  }
  type_1[i] <- which.min(conductance[[i]])
  type_2[i] <- which.max(conductance[[i]])
}

print(type_1)
print(type_2)
