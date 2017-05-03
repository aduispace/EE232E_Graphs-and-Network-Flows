library("igraph")
library("netrw")

#(3a)
RandomWalker2 =function(g,nNodes, DF)
{
  r=netrw(g, damping=DF,T=1000, output.walk.path=TRUE,output.visit.prob=TRUE)
  par(mfrow=c(1,1))
  #plot(r$ave.visit.prob)
  
  #plot relationship between degree distribution and visted nodes'degree distribution
  deg=numeric(0)
  vst=numeric(0)
  #degree for undirected graph
  deg=degree(g)
  #degree for directed graph
  ### deg=degree(g,mode="in")
  #### deg=degree(g,mode="out")
  vst=r$ave.visit.prob # visit probability history 
  sumprob= numeric(max(deg)-min(deg)+1)
  count= numeric(max(deg)-min(deg)+1)
 # print(sumprob)
 for (i in 1:1000){
  sumprob[deg[i]]<-sumprob[deg[i]]+r$ave.visit.prob[i]
  count[deg[i]]<-count[deg[i]]+1
}
for (k in min(deg):max(deg))
{
  sumprob[k]=sumprob[k]/count[k]
}
  # plot(vst)
  plot(sumprob,main="Relationship Between Prob and In-Degree",xlab="degree",ylab="prob",type="o")
  cor=cor(deg,vst) # count the probability correlation
  print(cor)
}

#the answer is yes, linearly related for undirected, no for directed
g1=erdos.renyi.game(1000, p = 0.01,directed = FALSE)
#RandomWalker2(g1,1000,1)

g2=erdos.renyi.game(1000, p = 0.01,directed = TRUE)
#RandomWalker2(g2,1000,1)

#RandomWalker2(g1,1000,0.85)

#(4a)
RandomWalker2(g2,nNodes,0.85)
prank=page.rank(g2)
plot(prank$vector,main="PageRank",xlab="Node",ylab="rankscore")
#print(prank)
#(4b)
RandomWalker3=function(g,nNodes,DF,teleProb)
{
  r=netrw(g, damping=DF, teleport.prob=teleProb, output.walk.path=TRUE)
  deg=numeric(0)
  vst=numeric(0)
  deg=degree(g)
  #degree for directed graph
  #deg=degree(g,mode="in")
  #deg=degree(g,mode="out")
  vst=r$ave.visit.prob
  sumprob= numeric(max(deg)-min(deg)+1)
  count= numeric(max(deg)-min(deg)+1)
  # print(sumprob)
  for (i in 1:1000){
    sumprob[deg[i]]<-sumprob[deg[i]]+r$ave.visit.prob[i]
    count[deg[i]]<-count[deg[i]]+1
  }
  for (k in min(deg):max(deg))
  {
    sumprob[k]=sumprob[k]/count[k]
  }
  #plot(sumprob,main="Relationship Between Prob and Degree",xlab="degree",ylab="prob",type="o")
  cor=cor(deg,vst)
}

RandomWalker3(g2,nNodes,0.85,prank$vector)
prank_per=personalized.pagerank(g,damping=0.85,prob=prank$vector)
plot(prank_per,main="Personalized_PageRank",xlab="Node",ylab="rankscore")


#(4c)
probVec=rep(1/nNodes,nNodes)
RandomWalker3(g2,nNodes,0.85,probVec)