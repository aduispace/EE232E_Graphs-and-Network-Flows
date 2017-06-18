library(igraph)
filePath = "C:/Users/lindu/Downloads/gplus/"

edgesFiles = list.files(path=filePath,pattern="edges")
g2Raw<-{}
g2<-{}
g2u<-{}
egoNodes={}
circlesRaw={{}}

count=1
commu1 <- {{}}
commu2 <- {{}}
for(j in 1:132)
{
  nodeId = sub("\\..*","",edgesFiles[j])
  circlesFile = paste("C:/Users/lindu/Downloads/gplus/",nodeId,".circles",sep="")
  fileConnection <- file(circlesFile, open="r")
  if(length(fileConnection)>0)
  {
    lines <- readLines(fileConnection)
    close(fileConnection)
    if(length(lines>0))
    {
      circles <- list()
      print(length(lines))
      for (i in 1:length(lines)) 
      {
        sp <- strsplit(lines[i],"\t")
        circles[[i]] <- sp[[1]][-1]
      }
      
      if(length(circles)>2)
      {
        print("Found one!")
        edgelistFile = paste(filePath,edgesFiles[j],sep="")
        
        g2Raw[[count]] = read.graph(edgelistFile,format="ncol",directed=TRUE)
        circlesRaw[[count]] <-circles
        
        
        nonEgoNodes = V(g2Raw[[count]])
        egoNodes[count]=nodeId
        g2[[count]] <- add.vertices(g2Raw[[count]],1,name=nodeId)
        egoNodeIndex <- which(V(g2[[count]])$name==nodeId) 
        edgeAppendList <- c()
        for (nodeIndex in 1:(vcount(g2[[count]])-1)) 
        {
          edgeAppendList <- c(edgeAppendList , c(vcount(g2[[count]]),nodeIndex))
        }
        g2[[count]] <- add.edges(g2[[count]],edgeAppendList)
        g2u[[count]]<- as.undirected(g2[[count]])
        commu1[[count]] <- walktrap.community(g2u[[count]], steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
        commu2[[count]] <- infomap.community (g2u[[count]], e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = TRUE)
        
        count=count+1
        print(count)
      }
    } 
  }
 # close(fileConnection)
}
##Sample Community Plot
gcomm_1<-walktrap.community(g2[[2]])
plot(gcomm_1,g2[[2]],vertex.label=NA,vertex.size=5,edge.arrow.size=0.2,main="Node 2 Community Structure by Walktrap (Directed)")
gcomm_2<-infomap.community(g2[[2]])
plot(gcomm_2,g2[[2]],vertex.label=NA,vertex.size=5,edge.arrow.size=0.2,main="Node 2 Community Structure by Infomap (Directed)")

# viewing as non-directed graph
plot(commu1[[2]],g2u[[2]],,vertex.label=NA,vertex.size=5,edge.arrow.size=0.2,main="Node 2 Community Structure by Walktrap (Undirected)")
plot(commu2[[2]],g2u[[2]],,vertex.label=NA,vertex.size=5,edge.arrow.size=0.2,main="Node 2 Community Structure by Infomap (Undirected)")

