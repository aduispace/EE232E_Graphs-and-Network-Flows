
library("igraph")
library("hash")
library(e1071)
#create the graph from the document
g<-read.graph("/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/edge_weight.txt",format="ncol",directed=TRUE)
actor_id_file<-file("/Users/lindu/Desktop/Spring 2017 Classes/EE232E Graphs Mining/Project2/project_2_data/actor_id.txt",open="r")
actor_id_map<-readLines(actor_id_file,encoding="latin1")

#top 10 actor/actress based on pagerank algorithm
rank_score<-page.rank (g, algo = "prpack", directed = TRUE, damping = 0.85, weights = NULL)
rankscore <- rank_score$vector
high_score<-sort(rank_score$vector,decreasing=TRUE)
high_id<-NULL
for(i in 1:10)
{
  high_id<-as.numeric(names(high_score[i]))
  # the index of actor_id_map starts from 1
  high_ID<-actor_id_map[high_id+1]
  aline=strsplit(high_ID,"\t\t")
  print(aline[[1]][1])
  print(high_score[i])
}
 rankindex <- seq(from = 0, to = length(rankscore)-1, by = 1)

 plot(rankindex,rankscore,col="red",pch=20,bg="yellow",lwd=2,xlab="Actor ID",ylab = "Score")
 

# 10 famous movie celebrities in my opinion
famous_name<-c("Depp, Johnny","Hanks, Tom","Travolta, John (I)","Thurman, Uma","Reeves, Keanu",
               "Damon, Matt", "Bale, Christian","Marceau, Sophie","Portman, Natalie","Johansson, Scarlett")
famous_id<-c(16878,27258,67477,109050,55726,15209,3632,96187,102225,90962)
famous_score<-hash()
for(i in 1:length(rank_score$vector))
{
  for(j in 1:10)
  {
    id<-famous_id[j]
    if(names(rank_score$vector[i])==id)
    {
      .set(famous_score,keys=famous_name[j],values=rank_score$vector[i])
    }
  }
}
print(values(famous_score,keys=famous_name))
close(actor_id_file)

#find the significant pairs
undirected_g<-as.undirected(g,mode="collapse",edge.attr.comb=list(weight="mean"))
high_weight<-sort(E(undirected_g)$weight,index.return=TRUE,decreasing=TRUE)
for(i in 1:10)
{
  highest=high_weight$ix[i]
  print(highest)
  print(high_weight$x[i])
  print(E(undirected_g)[highest])
}
# #find the names of those significant pairs
# significant_id<-c(111843,91669,111843,87535,111843,112573,112685,111843,67880,91669,61554,91669,92309,87535,112573,34294,112573,19250,70028,91669)
# for(i in 1:length(significant_id)){
#   idx<-significant_id[i]
#   print(idx)
#   id<-as.numeric(names(V(undirected_g)[idx]))
#   print(id)
#   significant_ID<-actor_id_map[id+1]
#   aline=strsplit(significant_ID,"\t\t")
#   print(aline[[1]][1])
# }

