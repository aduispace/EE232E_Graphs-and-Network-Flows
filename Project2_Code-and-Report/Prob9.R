rm(list=ls())
library(igraph)
setwd('/Users/ÓÆÄÏ/Documents/project_2_data/')
bipartite_edgelist <- "Bipartite.txt"
g_bipartite <- read_graph(bipartite_edgelist,format="ncol",directed=FALSE)
neighbors_list_batman <- list()
for(i in 1:length(threemovies_actors[[1]])){
  actor <- threemovies_actors[[1]][i]
  neighbors_list_batman[[actor]] <- c(neighbors(g_bipartite, which(names(V(g_bipartite))==actor)))
}
neighbors_list_mission <- list()
for(i in 1:length(threemovies_actors[[2]])){
  actor <- threemovies_actors[[2]][i]
  neighbors_list_mission[[actor]] <- c(neighbors(g_bipartite, which(names(V(g_bipartite))==actor)))
}
neighbors_list_minion <- list()
for(i in 1:length(threemovies_actors[[3]])){
  actor <- threemovies_actors[[3]][i]
  neighbors_list_minion[[actor]] <- c(neighbors(g_bipartite, which(names(V(g_bipartite))==actor)))
}
rating_list_batman <- list()
for(i in 1:length(neighbors_list_batman)){
  for (t in 1:length(neighbors_list_batman[[i]])){
    movie <- names(neighbors_list_batman[[i]][t])
    ratingval <- rating_namedList[[movie]]
    if(!is.null(ratingval)){
      rating_list_batman[[names(neighbors_list_batman[i])]] <- c(rating_list_batman[[names(neighbors_list_batman[i])]],ratingval)
    }
  }
}
rating_list_mission <- list()
for(i in 1:length(neighbors_list_mission)){
  for (t in 1:length(neighbors_list_mission[[i]])){
    movie <- names(neighbors_list_mission[[i]][t])
    ratingval <- rating_namedList[[movie]]
    if(!is.null(ratingval)){
      rating_list_mission[[names(neighbors_list_mission[i])]] <- c(rating_list_mission[[names(neighbors_list_mission[i])]],ratingval)
    }
  }
}
rating_list_minion <- list()
for(i in 1:length(neighbors_list_minion)){
  for (t in 1:length(neighbors_list_minion[[i]])){
    movie <- names(neighbors_list_minion[[i]][t])
    ratingval <- rating_namedList[[movie]]
    if(!is.null(ratingval)){
      rating_list_minion[[names(neighbors_list_minion[i])]] <- c(rating_list_minion[[names(neighbors_list_minion[i])]],ratingval)
    }
  }
}
rateofactor_batman <- c()
for(i in 1:length(rating_list_batman)){
  rateofactor_batman <- c(rateofactor_batman, mean(as.double(rating_list_batman[[i]])))
  print(rateofactor_batman)
  print(i)
}
rateofactor_mission <- c()
for(i in 1:length(rating_list_mission)){
  rateofactor_mission <- c(rateofactor_mission, mean(as.double(rating_list_mission[[i]])))
  print(rateofactor_mission)
  print(i)
}
rateofactor_minion <- c()
for(i in 1:length(rating_list_minion)){
  rateofactor_minion <- c(rateofactor_minion, mean(as.double(rating_list_minion[[i]])))
  print(rateofactor_minion)
  print(i)
}
avg_batman <- mean(rateofactor_batman)
avg_mission<- mean(rateofactor_mission)
avg_minion <- mean(rateofactor_minion)
