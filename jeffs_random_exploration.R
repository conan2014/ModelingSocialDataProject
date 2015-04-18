library(readr)
library(dplyr)
library(ggplot2)

# sample_n(user,nrow(user)*.1)

user <- read_csv("yelp_academic_dataset_user.csv")
user$num_friends <- sapply(1:nrow(user), function(x) length(strsplit(user$friends[x],',')[[1]]))
ggplot(user,aes(x=log10(fans),y=average_stars,alpha=review_count)) + 
  geom_point() + #xlim(0,200) + 
  labs(title="Effect of Network Size on Ratings", x="Number of Friends", y="Average Star Rating")

friend.list <- strsplit(user$friends,'\'')
friend.list <- sapply(1:length(friend.list), function(x) friend.list[[x]][c(F,T)])



users_who_rated <- function(category){
  biz_in_cat <- business$business_id[grep(category,business$categories)]
  reviews_in_cat <- reviews[reviews$business_id %in% biz_in_cat,]
  users_reviewed_cat <- user[user$user_id %in% reviews_in_cat$user_id,]
  return(users_reviewed_cat)
}
user$nightlife <- user$user_id %in% users_who_rated("Nightlife")$user_id

ggplot(user,
       aes(x=num_friends,y=compliments.hot,color=nightlife)) + geom_point() +
  labs(title="Differences in Reviewers and Non-Reviewers of Nightlife",
       x="Number of Friends",y="Times Voted: 'Hot'") +
  guides(color=guide_legend(title="Reviewed\nNightlife\nEstablishment"))



rm(list=ls())
setwd("C:/Users/Jeff.Bernard/Data/Yelp/yelp_dataset_challenge_academic_dataset/")

reviews <- read_csv("no_text_reviews.csv")
reviews$business_id %<>% as.factor
reviews$user_id %<>% as.factor


user$avg.cool <- user$votes.cool/user$review_count
user$avg.funny <- user$votes.funny/user$review_count
user$avg.useful <- user$votes.useful/user$review_count

user$total.votes <- user$votes.cool + user$votes.funny + user$votes.useful
user$perc.cool <- user$votes.cool/user$total.votes
user$perc.funny <- user$votes.funny/user$total.votes
user$perc.useful <- user$votes.useful/user$total.votes

business <- read_csv("yelp_academic_dataset_business.csv")

users_who_rated <- function(category){
  biz_in_cat <- business$business_id[grep(category,business$categories)]
  reviews_in_cat <- reviews[reviews$business_id %in% biz_in_cat,]
  users_reviewed_cat <- user[user$user_id %in% reviews_in_cat$user_id,]
  return(users_reviewed_cat)
}

user$doctors <-  user$user_id %in% users_who_rated("Doctors")$user_id
user$shopping <- user$user_id %in% users_who_rated("Shopping")$user_id


ggplot(sample_n(user,nrow(user)*.1),aes(x=num_friends,y=votes.useful,color=nightlife)) + geom_point(size=2)
ggplot(sample_n(user,nrow(user)*.1),aes(x=avg.cool,y=avg.funny,color=shopping)) + geom_point(alpha=.25) + theme_bw() + ylim(0,20) + xlim(0,20)
ggplot(user,aes(x=perc.useful,y=perc.cool)) + geom_point(alpha=.25) + theme_bw()
ggplot(user,aes(x=nperc.funny,y=nperc.cool)) + geom_point(alpha=.25) + theme_bw()
