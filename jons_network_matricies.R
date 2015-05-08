library(readr)
edgelist <- read_csv("yelp_academic_edgelist1.csv")

stayconnected <- function(edgelist){ #function that returns in-network ties
  edgelist <- na.omit(edgelist)
  format <- colnames(edgelist)
  require(dplyr)
  # create two columns, min and max, that will serve as identifiers
  edgelist <- edgelist %>%
    rowwise() %>%
    mutate(min=min(node,edge),max=max(node,edge)) %>%
    arrange(min,max) 
  # Locate in-network ties by those which are reciprocated
  index <- edgelist %>%
    group_by(min,max) %>%
    summarize(N=n()) %>%
    mutate(node=min)
  # join the indicator with the original list
  edgelist <- left_join(index[index$N==2,],edgelist)
  return(edgelist[,format]) # return only original columns
}

undirected <- stayconnected(edgelist)

library(igraph)
yelp_net <- as.matrix(undirected[,3:4])
yelp_net[,1]=as.character(yelp_net[,1])
yelp_net[,2]=as.character(yelp_net[,2])
graph.yelp <- graph.edgelist(yelp_net,directed=FALSE)

library(Matrix)
adj.yelp <- Matrix(get.adjacency(graph.yelp),sparse=TRUE)

# compute the 2nd degee adjacency matrix
d2 <- adj.yelp %*% adj.yelp # d2 contains 2-walks
names <- dimnames(d2)[1]
diag(d2) <- 0     # take out loops
d2[d2!=0] <- 1    # transform to remove multiplicity of walks
D2 <- d2 # remove 1st degree connections
D2[adj.yelp==1] <- 0
D2[D2<0] <- 0 # remove negative values created by above

# calculate reach of 2nd degree
degree2 <- data.frame(c(1:dim(D2)[1]))
colnames(degree2) <- "Matrix.key"
degree2$colsum <- colSums(D2)
degree2$rowsum <- rowSums(D2)
realkey <- as.data.frame(names,stringsAsFactors=FALSE)
colnames(realkey) <- "node"
degree2 <- cbind(realkey,degree2)
degree2$node <- as.integer(degree2$node)
# check to verify square matrix and non-negatives
table(degree2$colsum==degree2$rowsum)
table(degree2$colsum<0)

library(dplyr)
out.degree <- na.omit(edgelist) %>%
              group_by(node, user_id) %>%
              summarize(degree=n()) 

neighborhood <- inner_join(degree2, out.degree)
neighborhood$degree2 <- neighborhood$colsum
neighborhood <- neighborhood[order(neighborhood$node),c("user_id","node","degree","degree2")]

write.csv(neighborhood, "yelp_network_degree.csv", row.names=FALSE)
