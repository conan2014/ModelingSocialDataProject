
library(Matrix)
library(magrittr)
library(dplyr)
library(readr)

setwd("C:/Users/Jeff.Bernard/Data/Yelp/yelp_dataset_challenge_academic_dataset/")

##### SET UP #####
reviews <- read_csv("no_text_reviews.csv")
reviews$user_id %<>% as.factor
reviews$business_id %<>% as.factor

business <- read_csv("yelp_academic_dataset_business.csv")
restaurants <- business[grep("Restaurant",business$categories),]
restreviews <- reviews[reviews$business_id %in% restaurants$business_id,] %<>% droplevels

write.csv(restreviews,"restaurantreviews.csv",row.names=F)
rm(list=ls())

##### MAIN FUNCTION #####

reviews <- read_csv("restaurantreviews.csv")
reviews$user_id %<>% as.factor
reviews$business_id %<>% as.factor

knn <- function(k){
  ## COMPUTE DISTANCES ##
  #for each business: compute distances between each user pair if not already computed
  # store results in sparse matrix
  review.matrix <- sparseMatrix(i=as.numeric(reviews$user_id),
                                j=as.numeric(reviews$business_id),
                                x=reviews$stars,
                                use.last.ij=TRUE)
  system.time(distances <- sparseDist(review.matrix[1:5000,],7) %>% t)
  ## for each user: sort list of users with non-inf distance and keep closest k ##
  
  ## for each user: 
  
}

business <- read_csv("yelp_academic_dataset_business.csv")
business$state %<>% as.factor
restaurants <- business[grep("Restaurant",business$categories),]
PAlist <- filter(restaurants,state=="PA") %>% select(business_id) %>% unlist
PArests <- filter(reviews,business_id %in% PAlist) %>% droplevels

PAmatrix <-  sparseMatrix(i=as.numeric(PArests$user_id),
                          j=as.numeric(PArests$business_id),
                          x=PArests$stars,
                          use.last.ij=TRUE)
system.time(PAdists <- sparseDist(PAmatrix[1:5000,],7) %>% t)


numreviews <- rowSums(review.matrix != 0)
library(ggplot2)
qplot(numreviews)

csum <- sapply(1:50, function(i) sum(numreviews > i))
qplot(x=1:50,y=csum,geom="line")

users_with_nine_plus_reviews <- which(numreviews > 9)

reviews_from_active_users <- 
  reviews[as.numeric(reviews$user_id) %in% users_with_nine_plus_reviews,] %>% 
  droplevels

act.review.mat <- sparseMatrix(i=as.numeric(reviews_from_active_users$user_id),
                               j=as.numeric(reviews_from_active_users$business_id),
                               x=reviews_from_active_users$stars,
                               use.last.ij=TRUE)

act.review.mat@Dimnames <- list(as.character(unique(reviews_from_active_users$user_id)),
                                as.character(unique(reviews_from_active_users$business_id)))
library(recommenderlab)
ratings <- new("realRatingMatrix")
ratings@data <- act.review.mat

rec <- Recommender(ratings,"POPULAR")

system.time(dists <- sparseDist(act.review.mat,7) %>% t)

sparseDist <- function(m, k) {
  m <- t(m)
  n <- ncol(m)
  d <- vapply(1:n, function(i) { 
    d <- colSums((m[,-c(i)] - m[,i])^2)
    o <- sort.list(d, method='quick')[1:k]
    return(c(sqrt(d[o]), ifelse(o<i,o,o+1)))
  }, numeric(2*k)
  )
  dimnames(d) <- list(c(paste0('d', 1:k),
                        paste0('i', 1:k)), colnames(m)[-n])
  d
}
