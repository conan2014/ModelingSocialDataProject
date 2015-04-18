library(readr)

user <- read_csv("yelp_academic_dataset_user.csv")
user$friend.list <- strsplit(user$friends,'\'')
user$friend.list <- sapply(1:nrow(user), function(x) user$friend.list[[x]][c(F,T)])
user$num_friends <- sapply(1:nrow(user), function(x) length(user$friend.list[[x]]))

edgelist <- cbind(user$user_id,user$friend.list)
