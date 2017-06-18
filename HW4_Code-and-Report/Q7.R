## Problem 7
closeAllConnections()
rm(list=ls())

library('igraph')
library('readr')

fname_ticker = "/Users/dui/Google Drive/XPS/Spring 2017 Courses/EE232E Graphs Mining/HW4/finance_data/Name_sector.csv"
ticker = read.csv(fname_ticker,stringsAsFactors = FALSE) 

co_names = ticker$Symbol

r_i_mat = matrix(list(),length(co_names),2)

P.data <- data.frame(A = character(),
                     B = character(), 
                     C = numeric(), 
                     stringsAsFactors=FALSE) 
colnames(P.data) = c("Node 1", "Node 2", "weights")


row_count = 0
for(i in co_names) { #loop through each company name
  
  row_count = row_count + 1
  fname_co = paste('/Users/dui/Google Drive/XPS/Spring 2017 Courses/EE232E Graphs Mining/HW4/finance_data/data/',i,'.csv',sep = "") #get directory of current company name
  co_data_close = read.csv(fname_co,stringsAsFactors = FALSE)$Close # get all closing data
  
  
  r_i_local = numeric(0) # clear variable
  for (j in 2:length(co_data_close)){
    r_i_local[j-1] = log(co_data_close[j]) - log(co_data_close[j-1])  #r_i formula
  }
  
  r_i_mat[[row_count,1]] = i
  r_i_mat[[row_count,2]] = r_i_local
}

edge_count = 0
count = 1
tot_d_ij = numeric()
tot_P_ij = numeric()
for( i in 1 : (length(co_names)-1) ) {
  count = count + 1
  for( j in (i+1) : length(co_names) ){
    
    edge_count = edge_count + 1
    P_i = r_i_mat[[i,2]]
    P_j = r_i_mat[[j,2]]
    
    avg_i = mean(P_i) # <P_i>
    avg_j = mean(P_j)
    avg2_i = mean(P_i^2) # <P_i^2>
    avg2_j = mean(P_j^2)
    
    P_ij = (mean(P_i*P_j)  - avg_i * avg_j) / sqrt((avg2_i - avg_i^2) * (avg2_j - avg_j^2)) # P_ij formula
    tot_P_ij = c(tot_P_ij , P_ij) #un-modified P_ij's
    
    if(P_ij > 0.3){
      P_ij = -1 # modify P_ij!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    }
    
    d_ij = sqrt(2*(1-P_ij)) # d_ij from modified P_ij
    
    tot_d_ij = c(tot_d_ij , d_ij)
    P.data[edge_count,1] = co_names[i] # store to data.frame
    P.data[edge_count,2] = co_names[j]
    P.data[edge_count,3] = d_ij
  }
}

g1 = graph.data.frame(P.data,directed = FALSE)


nodes = V(g1)$name # vector of all nodes
sectors = ticker$Sector # vector of all sectors 
u_sectors = unique(sectors)

node_cols = rep(0,length(nodes))
col_id = 1
for(i in u_sectors){
  node_cols[which(sectors == i)] = col_id #identify colors to unique sectors
  col_id = col_id + 1
}

g1_mst = mst(g1 , weights = P.data$weights) # create minimal spanning tree



plot(g1_mst, vertex.color = node_cols ,
     vertex.size = rep(7,length(nodes)) , 
     vertex.label = NA, 
     main = "Modified Minimal Spanning Tree" ) # mst

# Hist of un-modified P_ij's 
hist( x= tot_P_ij, breaks = 50, col = "blue", 
      main = "Histogram of Un-modified P_ij's", xlab = "d_ij Value", ylab = "Frequency")

# Hist of d_ij calculated from modified P_ij
hist( x= tot_d_ij, breaks = 50, col = "yellow",
      main = "Histogram of Modified d_ij's ", xlab = "d_ij Value", ylab = "Frequency")