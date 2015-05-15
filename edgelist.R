# Edgelist transformation

# Subset the data to obtain edgelist
library(readr)
users <- read_csv("yelp_academic_dataset_user.csv")
social_1 <- users[,c("user_id","friends")]
# Remove special characters
find <- c("\\[|\\]", "u'", "'")
replace <- c("","","")
for (i in 1:length(find)){
  social_1[,2] <- gsub(find[i], replace[i],social_1[,2])
}

# Convert into a complete edgelist 
library(plyr)
library(dplyr)
library(stringr)
# Function to be applied to each row of data
# takes each node and creates a line for each connection
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
edgelist <- social_1 %>%
            rowwise() %>%
            do(longedge(.)) %>%
            rbind()

# Divide the network into components and outliers
edgelist <- arrange(edgelist,user_id)
edgelist_1 <- na.omit(edgelist)
edgelist_2 <- edgelist[is.na(edgelist$friend),]
# create unique list of values in order to make numeric keys
edgelevel1 <- unique(sort(edgelist_1$user_id))
edgelevel2 <- unique(sort(edgelist_1$friend))
edgelevels <- unique(sort(c(edgelevel1,edgelevel2)))
# mapvalues from keys to user_id
edgelist_1 <- arrange(edgelist_1,user_id)
edgelist_1$node <- mapvalues(edgelist_1$user_id,from=edgelevels,to=c(1:length(edgelevels)))
# mapvalues from keys to connections/friends
edgelist_1 <- arrange(edgelist_1,friend)
edgelist_1$edge <- mapvalues(edgelist_1$friend,from=edgelevels,to=c(1:length(edgelevels)))
# create unique keys for outliers
na.start <- 1+length(edgelevels)
na.end <- na.start + dim(edgelist_2)[1] -1
edgelist_2$node <- c(na.start:na.end)
edgelist_2$edge <- edgelist_2$friend
# combine divided network into whole
edgelist <- rbind(edgelist_1,edgelist_2)
# save edgelist as a csv
write.csv(edgelist, "yelp_academic_edgelist.csv", row.names=FALSE)

# create conversion key as a dataframe for all nodes
key_1 <- as.data.frame(cbind(edgelevels,c(1:length(edgelevels))),stringsAsFactors=FALSE)
colnames(key_1) <- c("user_id","node")
key_2 <- edgelist_2[,c("user_id","node")]
key <- rbind(key_1,key_2)
# save key as a csv
write.csv(key, "edgelist_node_key.csv", row.names=FALSE)
