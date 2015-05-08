
library(Matrix)
library(magrittr)
library(dplyr)
library(readr)
# rm(list=ls())
setwd("C:/Users/Jeff.Bernard/Data/Yelp/yelp_dataset_challenge_academic_dataset/")

##### SET UP #####
# reviews <- read_csv("no_text_reviews.csv")
# reviews$user_id %<>% as.factor
# reviews$business_id %<>% as.factor
# 
# business <- read_csv("yelp_academic_dataset_business.csv")
# restaurants <- business[grep("Restaurant",business$categories),]
# restreviews <- reviews[reviews$business_id %in% restaurants$business_id,] %<>% droplevels
# 
# write.csv(restreviews,"restaurantreviews.csv",row.names=F)
# rm(list=ls())
# 
# reviews <- read_csv("restaurantreviews.csv")
# reviews$user_id %<>% as.factor
# reviews$business_id %<>% as.factor
# 
# business <- read_csv("yelp_academic_dataset_business.csv")
# business$state %<>% as.factor
# restaurants <- business[grep("Restaurant",business$categories),]
# AZrests <- 
#   dplyr::filter(restaurants,state=="AZ") %>% 
#   dplyr::select(business_id) %>% 
#   unlist
# AZreviews <- 
#   filter(reviews,business_id %in% AZrests) %>% 
#   droplevels
# 
# AZmatrix <-  sparseMatrix(i=as.numeric(AZreviews$user_id),
#                           j=as.numeric(AZreviews$business_id),
#                           x=AZreviews$stars,
#                           use.last.ij=TRUE)
# numreviews <- rowSums(AZmatrix != 0)
# 
# active.users <- which(numreviews > 9)
# 
# act.user.reviews <- 
#   AZreviews[as.numeric(AZreviews$user_id) %in% active.users,] %>% 
#   dplyr::select(user_id,business_id,stars) %>%
#   droplevels

########### start here ############
act.user.reviews <- read_csv("actreviewmat.csv")
act.user.reviews$business_id %<>% as.factor
act.user.reviews$user_id %<>% as.factor

act.review.mat <- sparseMatrix(i=as.numeric(act.user.reviews$user_id),
                               j=as.numeric(act.user.reviews$business_id),
                               x=act.user.reviews$stars,
                               use.last.ij=TRUE)

train <- read_csv("train.csv")
test <- read_csv("test.csv")

userlist <- unique(train$user_id)

test$pred3 <- 0
test$pred5 <- 0
test$pred7 <- 0
test$pred9 <- 0
test$pred11 <- 0

# for each test point, 
for(i in 1:nrow(test)){
### find all users who rated that business, 
  biz <- test[i,2] %>% as.character
  user <- test[i,1] %>% as.character
  stars <- train[train$business_id==biz,3] %>% unlist
#  simusers <- train[train$business_id==biz,1] %>% unlist %>% c(user,.)
#  usernums <- match(simusers,userlist)
  print(paste(i,length(stars)))
### find nearest k to test point (USING TRAIN DATA)
#  simusermat <- act.review.mat[usernums,] %>% as.matrix
#  d <- dist(simusermat, "euclidean") %>% as.matrix %>% .[1,-1]

  simusers <- train[train$business_id==biz,1] %>% unlist
  usernums <- match(simusers,userlist)
  simusermat <- act.review.mat[usernums,] %>% as.matrix
  d <- simusermat %>%
    sweep(2,act.review.mat[match(user,userlist),]) %>%
    .^2 %>% rowSums

### take avg of points, then round
  orderd <- stars[order(d)]
  test$pred3[i] <- orderd %>% head(3) %>% mean %>% round
  test$pred5[i] <- orderd %>% head(5) %>% mean %>% round
  test$pred7[i] <- orderd %>% head(7) %>% mean %>% round
  test$pred9[i] <- orderd %>% head(9) %>% mean %>% round
  test$pred11[i] <- orderd %>% head(11) %>% mean %>% round
}

rmse3 <- (test$stars - test$pred3)^2 %>% na.omit %>% mean %>% sqrt
rmse5 <- (test$stars - test$pred5)^2 %>% na.omit %>% mean %>% sqrt
rmse7 <- (test$stars - test$pred7)^2 %>% na.omit %>% mean %>% sqrt
rmse9 <- (test$stars - test$pred9)^2 %>% na.omit %>% mean %>% sqrt
rmse11 <- (test$stars - test$pred11)^2 %>% na.omit %>% mean %>% sqrt

##### could weight by distance (inverse)

##### or network measures

knn <- function(k){
  ## COMPUTE DISTANCES ##
  #for each business: compute distances between each user pair if not already computed
  # store results in sparse matrix
  nusers <- act.user.reviews$user_id %>% unique %>% length
  dists <- sparseMatrix(i=1:nusers,j=1:nusers,x=Inf,symmetric=TRUE,dims=c(nusers,nusers))
  
  ## reviews for businesses must be greater than 1
  
  for(bID in unique(act.user.reviews$business_id)){
    revs <- act.user.reviews[act.user.reviews$business_id %in% bID,]
    users <- unique(as.numeric(revs$user_id))
    n <- length(users)
    mat.sub <- act.review.mat[users,]
    
    # convert mat.sub to real matrix
    mat.sub %<>% as.matrix
    # run dist() on it
    mdist <- dist(mat.sub) %>% as.matrix
    # check all combos for non-zero values
    udists <- dists[users,users] %>% as.matrix
    zeroes <- udists == 0
    # for all zero values, add dist to the matrix
    
#     for(i in seq_len(n-1)){
#       eucd <- sqrt(rowSums((mat.sub[c(i+1):n,,drop=FALSE] - mat.sub[i,])^2))
#       print(paste0("i",i))
#       for(j in 1:length(eucd)){
#         dists[users[i],users[i+j]] <- eucd[j]
#         print(paste0("j",j))
#       }
#     }
    print(which(unique(act.user.reviews$business_id)==bID))
  }
  
  review.matrix <- sparseMatrix(i=as.numeric(reviews$user_id),
                                j=as.numeric(reviews$business_id),
                                x=reviews$stars,
                                use.last.ij=TRUE)
  system.time(distances <- sparseDist(review.matrix[1:5000,],7) %>% t)
  ## for each user: sort list of users with non-inf distance and keep closest k ##
  
  ## for each user: 
  
}

#system.time(AZdists <- sparseDist(PAmatrix[1:5000,],7) %>% t)


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
