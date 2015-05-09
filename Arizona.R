##################################################################################
# 1. Gather the Arizona Data

# read in the user data
library(readr)
users <- read_csv("yelp_academic_dataset_user.csv")
review <- read_csv("yelp_academic_dataset_review.csv")
business <- read_csv("yelp_academic_dataset_business.csv")

# subset restaurant reviews
bvar <- c("business_id","name","state","city","stars","categories")
business <- business[,bvar]
restaurant <- business[grep("Restaurant",business$categories,ignore.case=TRUE),]
colnames(restaurant)[c(2,5)] <- c("b.name","b.stars")

# subset AZ restaurants
az.restaurant <- restaurant[restaurant$state=="AZ",]

# merge with reviews to get all reviews on these businesses
library(plyr)
library(dplyr)
az.review <- left_join(az.restaurant,review)
az.review <- az.review %>%
             mutate(votes=votes.cool+votes.funny+votes.useful)
rvar <- c("user_id","review_id","business_id","b.name","state","city","b.stars",
          "stars","votes","date")
az.review <- az.review[,rvar]
colnames(az.review)[8:10] <- c("r.stars","r.votes","r.date")

# merge with users in order to gain the AZ network
az.users <- left_join(az.review,users)
az.users <- az.users %>%
            rowwise  %>%
            mutate(u.compliments=sum(c(compliments.hot,compliments.list,
                    compliments.more,compliments.note,compliments.photos,
                    compliments.plain,compliments.profile,
                    compliments.writer),na.rm=TRUE),
                   u.votes=sum(c(votes.cool,votes.funny,votes.useful),na.rm=TRUE))
colnames(az.users)[c(23,26)] <- c("u.stars","u.name")
uvar <- c("user_id","u.name","u.stars","review_count","fans","yelping_since",
          "u.votes","u.compliments","elite","friends")
bvar <- c("business_id","b.name","b.stars","state","city")
rvar <- c("review_id","r.date","r.stars","r.votes")
az.users <- az.users[,c(uvar,bvar,rvar)]

write.csv(az.users,"az.network.csv",row.names=FALSE)

##################################################################################

# 2. Create Arizona Edgelist

# subset dataset to edgelist components
# obtain original count of reviews
az.users <- read_csv("az.network.csv")
social_1 <- az.users[,c("user_id","friends")] %>%
            group_by(user_id,friends) %>%
            summarize(az.count=n())

# function to remove special characters
bookends <- function(x,front,back){
  substr(x,front+1,nchar(x)-back)
}
# remove brackets from friends variable
social_1$friends <- bookends(social_1$friends,1,1)

# Convert into a complete edgelist 
library(stringr)
# Function to create row for each connection
longedge <- function(edgelist){
  user <- edgelist$user_id
  cnx <- edgelist$friends
  split <- as.data.frame(ifelse(cnx=="",NA,str_split(cnx,", ")))
  combine <- as.data.frame(cbind(user,split))
  colnames(combine) <- c("user_id", "friend")
  combine$user_id <- as.character(combine$user_id)
  combine$friend <- as.character(combine$friend)
  return(combine)
}
# Creating long edgelist
az.edgelist <- social_1 %>%
  rowwise() %>%
  do(longedge(.)) %>%
  rbind()
# remove special characters from edgelist
az.edgelist$friend <- bookends(az.edgelist$friend,2,1)

##################################################################################

# 3. Create conversion keys for user_id's

keymaster <- function(edgelist,complete=FALSE,key=FALSE){
  require(plyr)
  # Divide the network into components and outliers
  edgelist_1 <- na.omit(edgelist)
  # create unique list of values in order to make numeric keys
  edgelevels <- unique(sort(c(edgelist_1$user_id,edgelist_1$friend)))
  # mapvalues from keys to user_id
  edgelist_1 <- arrange(edgelist_1,user_id)
  edgelist_1$node <- mapvalues(edgelist_1$user_id,from=edgelevels,to=c(1:length(edgelevels)))
  # mapvalues from keys to connections/friends
  edgelist_1 <- arrange(edgelist_1,friend)
  edgelist_1$edge <- mapvalues(edgelist_1$friend,from=edgelevels,to=c(1:length(edgelevels)))
  # create unique keys for outliers
  if (complete==TRUE){
    na.start <- 1+length(edgelevels)
    na.end <- na.start + dim(edgelist_2)[1] -1
    edgelist_2$node <- c(na.start:na.end)
    edgelist_2$edge <- edgelist_2$friend
    # combine divided network into whole
    edgelist <- rbind(edgelist_1,edgelist_2)} else {
      edgelist <- edgelist_1
    }
  if (key==TRUE){
    # create conversion key as a dataframe for all nodes
    key_1 <- as.data.frame(cbind(edgelevels,c(1:length(edgelevels))),stringsAsFactors=FALSE)
    colnames(key_1) <- c("user_id","node")
    if (complete==TRUE){
      key_2 <- edgelist_2[,c("user_id","node")]
      key_1 <- rbind(key_1,key_2)}
    return(key_1)
  } else {
    return(edgelist)
  }     
}
az.edgelist <- keymaster(az.edgelist)
az.key <- keymaster(az.edgelist,key=TRUE)

##################################################################################

# 4. Remove out of network ties to create undirected edgelist

stayconnected <- function(edgelist,isolates=FALSE){ #function that returns in-network ties
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
  if (isolates==TRUE){
    edgelist <- left_join(index[index$N==2,],edgelist)
  } else {
    edgelist <- left_join(index[index$N==2,],edgelist)
  }
  return(edgelist[,format]) # return only original columns
}
az.edgelist <- stayconnected(az.edgelist)
# save edgelist as a csv
write.csv(az.edgelist,"az.network1.csv",row.names=FALSE)

##################################################################################

# 5. Pass the edgelist into igraph

library(igraph)
az_net <- as.matrix(az.edgelist[,3:4])
az_net[,1]=as.character(az_net[,1])
az_net[,2]=as.character(az_net[,2])
graph.az <- graph.edgelist(az_net,directed=FALSE)

layout1 <- layout.fruchterman.reingold(graph.az)
plot(graph.az,layout=layout1,vertex.labels=NA,vertex.size=1)

##################################################################################

# 6. Compute the 2nd degee adjacency matrix

library(Matrix)
adj.az <- Matrix(get.adjacency(graph.az),sparse=TRUE)

d2 <- adj.az %*% adj.az # d2 contains 2-walks
diag(d2) <- 0     # take out loops
names <- dimnames(d2)[1]

D2 <- d2
D2[D2!=0] <- 1       # transform to remove multiplicity of walks
D2[adj.az==1] <- 0 # remove 1st degree connections
D2[D2<0] <- 0 # remove negative values created by above

# Convert 2nd degree adjacency matrix into edglist
graph.az2 <- graph.adjacency(D2, mode="undirected")
az.edgelist2 <- as.data.frame(get.edgelist(graph.az2),stringsAsFactors=FALSE)
az.edgelist2 <- left_join(az.edgelist2,az.key,by=c("V2"="node"))
colnames(az.edgelist2) <- c("node","edge","friend")
az.edgelist2 <- left_join(az.edgelist2,az.key)
az.edgelist2 <- az.edgelist2[,c(4,3,1,2)]
write.csv(az.edgelist2,"2nd_degree_edgelist.csv")

##################################################################################

# 7. Compute 1st Degree recommendation predictions

az.review <- read_csv("az.network.csv")
az.review <- az.review[,c("user_id","u.stars","business_id","r.stars","r.votes")]

ratings <- Matrix(nrow=dim(az.key)[1],ncol=length(unique(sort(az.review$business_id))))
count <- Matrix(nrow=dim(az.key)[1],ncol=length(unique(sort(az.review$business_id))))

az.review <- left_join(az.review,az.key)
az.bizkey <- as.data.frame(unique(sort(az.review$business_id)),stringsAsFactors=FALSE)
az.bizkey$key <- c(1:dim(az.bizkey)[1])
colnames(az.bizkey) <- c("business_id","bizkey")
az.review <- left_join(az.review,az.bizkey)
az.review <- na.omit(az.review)
az.review <- az.review[,c("bizkey","node","r.stars")]
colnames(az.review)[2] <- "u.node"

for (i in 1:dim(az.bizkey)[1]){
  reviewbybiz <- az.review[az.review$bizkey==i,]
  # 1st Degree node
  cnxlist <- reviewbybiz %>%
    rowwise() %>%
    left_join(az.edgelist[,3:4],.,by=c("node"="u.node")) %>%
    na.omit() %>%
    rbind()
  cnxlist$edge <- as.integer(cnxlist$edge)
  for (j in 1:dim(cnxlist)[1]){
    ratings[cnxlist$edge[j],i] <- sum(c(ratings[cnxlist$edge[j],i],cnxlist$r.stars[j]),na.rm=TRUE)
    count[cnxlist$edge[j],i] <- sum(c(count[cnxlist$edge[j],i],1),na.rm=TRUE)
  }
  # 1st Degree edge
  cnxlist <- reviewbybiz %>%
    rowwise() %>%
    left_join(az.edgelist[,3:4],.,by=c("edge"="u.node")) %>%
    na.omit() %>%
    rbind()
  cnxlist$node <- as.integer(cnxlist$node)
  for (j in 1:dim(cnxlist)[1]){
    ratings[cnxlist$node[j],i] <- sum(c(ratings[cnxlist$node[j],i],cnxlist$r.stars[j]),na.rm=TRUE)
    count[cnxlist$node[j],i] <- sum(c(count[cnxlist$node[j],i],1),na.rm=TRUE)
  }
}

##################################################################################

# 8. Network Graph

# plot that Jake did
library(ggplot2)
library(scales)
theme_set(theme_bw())

neighborhood <- read_csv("yelp_network_degree.csv")

cum.degree <- neighborhood %>%
  group_by(degree) %>%
  summarize(add=n()) %>%
  arrange(degree) %>%
  mutate(cdf=cumsum(add)/sum(add))

ggplot(data=cum.degree) +
  geom_point(aes(x=degree,y=add),color="black") + 
  scale_x_log10() +
  scale_y_log10() +
  xlab('Degree') + ylab('Number of Nodes') 

ggplot(data=neighborhood) +
  geom_point(aes(x=degree,y=degree2)) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Friends") + ylab("Friends of Friends")
