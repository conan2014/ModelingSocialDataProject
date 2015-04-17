library(readr)

user <- read_csv("yelp_academic_dataset_user.csv")
user$num_friends <- sapply(1:nrow(user), function(x) length(strsplit(user$friends[x],',')[[1]]))
user$friend.list <- strsplit(user$friends,'\'')
user$friend.list <- sapply(1:nrow(user), function(x) user$friend.list[[x]][c(F,T)])
