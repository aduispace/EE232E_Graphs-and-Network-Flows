library('igraph')
library('readr')

############## Question 2 Constructing Correlation Graphs ############## 

fname_sector = "finance_data/Name_sector.csv"
sector = read.csv(fname_sector,stringsAsFactors = FALSE) 

# construct adjacency matrix D = [dij ]
stock_names = sector$Symbol
r_i_mat = matrix(list(),length(stock_names),2)
P.data <- data.frame(A = character(), B = character(),  C = numeric(), stringsAsFactors=FALSE) 
colnames(P.data) = c("Node_1", "Node_2", "weights")

#loop through each stock name
row_count = 0
for(i in stock_names) { 
  row_count = row_count + 1
  cat("stock count" , row_count ,"\n")
  fname_co = paste('finance_data/data/',i,'.csv',sep = "") 
  co_data_close = read.csv(fname_co,stringsAsFactors = FALSE)$Close 
  r_i_local = numeric(0) 
  for (j in 2:length(co_data_close)){
    r_i_local[j-1] = log(co_data_close[j]) - log(co_data_close[j-1])  #r_i formula
  }
  r_i_mat[[row_count,1]] = i
  r_i_mat[[row_count,2]] = r_i_local
}

# create correlation graph
edge_count = 0
count = 1
all_d_ijs = numeric()
for( i in 1 : (length(stock_names)-1) ) {
  cat("Creating Graph for Stock",count, "\n")
  count = count + 1
  for( j in (i+1) : length(stock_names) ){
    edge_count = edge_count + 1
    P_i = r_i_mat[[i,2]]
    P_j = r_i_mat[[j,2]]
    avg_i = mean(P_i) 
    avg_j = mean(P_j)
    avg2_i = mean(P_i^2) 
    avg2_j = mean(P_j^2)
    P_ij = (mean(P_i*P_j)  - avg_i * avg_j) / sqrt((avg2_i - avg_i^2) * (avg2_j - avg_j^2)) # cross_corelation formula
    d_ij = sqrt(2*(1-P_ij))
    all_d_ijs = c(all_d_ijs , d_ij)
    P.data[edge_count,1] = stock_names[i] 
    P.data[edge_count,2] = stock_names[j]
    P.data[edge_count,3] = d_ij
  }
}

g1 = graph.data.frame(P.data,directed = FALSE)
var(all_d_ijs)
mean(all_d_ijs)
hist( x= all_d_ijs, col = "red", breaks = seq(from = round(min(all_d_ijs)), to = round(max(all_d_ijs)), by = (max(all_d_ijs)-min(all_d_ijs))/50), 
      main = "Histogram of d_ij's", xlab = "d_ij", ylab = "Frequencies")

############## Question 3 Finding Minimum Spanning Trees (MSTs) for the Correlation Graphs##############

nodes = V(g1)$name # create a vector of all nodes
sectors = sector$Sector # create a vector of all sectors 
u_sectors = unique(sectors)
colors = rep(0,length(nodes))

#color unique sectors
col_id = 1
for(i in u_sectors){
  colors[which(sectors == i)] = col_id 
  col_id = col_id + 1
}

# create MST
mst(g1 , weights = P.data$weights)
plot(g1, vertex.color = colors ,vertex.size = rep(7,length(nodes)) , vertex.label = NA, 
main = "MST of the Weighted Correlation Graph" ) 
