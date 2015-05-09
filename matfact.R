rm(list=ls())
library(dplyr)
library(Matrix)
library(magrittr)
library(readr)
library(recommenderlab)
library(MASS)
setwd("C:/Users/Jeff.Bernard/Data/Yelp/yelp_dataset_challenge_academic_dataset/")

reviews <- read_csv("no_text_reviews.csv")
reviews$business_id %<>% as.factor
reviews$user_id %<>% as.factor
reviews %<>% arrange(date)


business <- read_csv("yelp_academic_dataset_business.csv")
business$state %<>% as.factor
restaurants <- business[grep("Restaurant",business$categories),]
restreviews <- reviews[reviews$business_id %in% restaurants$business_id,] %<>% droplevels

# Function to extract restaurant reviews by state and produce recommender
getLocalRec <- function(st,method,restaurants,restreviews){
  locallist <- filter(restaurants,state==st) %>% select(business_id) %>% unlist
  localrests <- filter(restreviews,business_id %in% locallist) %>% droplevels
  
  localmatrix <-  sparseMatrix(i=as.numeric(localrests$user_id),
                            j=as.numeric(localrests$business_id),
                            x=localrests$stars,
                            use.last.ij=TRUE)
  
  localmatrix@Dimnames <- list(as.character(unique(localrests$user_id)),
                            as.character(unique(localrests$business_id)))
  
  ratings <- new("realRatingMatrix")
  ratings@data <- localmatrix
  
  rec <- Recommender(ratings,method)
  
  return(rec)
}

## Different methods available
recommenderRegistry$get_entry_names()

## States in dataset
states <- unique(business$state)
tab <- table(business$state)
feasible <- names(tab)[tab>13]

azrec <- getLocalRec("AZ","POPULAR",restaurants,restreviews)
nvrec <- getLocalRec("NV","POPULAR",restaurants,restreviews)

## all local recs
## (takes a minute or two)
reclist <- list()
for(i in 1:length(feasible)){
  st <- feasible[i]
  reclist[i] <- getLocalRec(st,"POPULAR",restaurants,restreviews)
}

sigsq=0.25, lambda=10, d=20, iterations=100){
  
  # define our prior belief
  prior <- lambda * sigsq * diag(1, nrow=d, ncol=d) 
  
  # create sorted index list of unique users and movies in training set
  # sorting is just convenience for when we need to calculate RMSE and objective function later

  train <- act.user.reviews
  
  train$id <- paste0(train$user_id,train$business_id)
  train <- train[!duplicated(train$id),-4]
  
  ndx <- sample(nrow(train),nrow(train)*0.1,FALSE)
  
  test <- train[ndx,]
  train <- train[-ndx,]
  
  # remove test rows for which restaurant or user is not in training set
  feasiblerows <- test$business_id %in% unique(train$business_id) & 
    test$user_id %in% unique(train$user_id)
  test <- test[feasiblerows,]
  rm(feasiblerows)
  
  op <- rbind(train,test) %>% droplevels
  
  train <- op[1:nrow(train),]
  test <- op[c(nrow(train)+1):nrow(op),]
  
  users <- as.character(unique(train$user_id))
  rests <- as.character(unique(train$business_id))
  
  train <- arrange(train,business_id,user_id)
  test <- arrange(test,business_id,user_id)

  testrests_by_user <- sparseMatrix(i=as.numeric(test$user_id),
                                    j=as.numeric(test$business_id),
                                    x=T,
                                    dims=c(length(users),length(rests))) %>% as.matrix
    
  # randomly initialize v
  v <- mvrnorm(length(rests),rep(0,d),diag(1/lambda,d,d))
  u <- matrix(0,nrow=length(users),ncol=d)
  
  # initialize Log likelihood and RMSE arrays
  RMSE <- rep(0,iterations)
  
  for(step in 49:iterations){
    
    # update user locations
    for(i in users){ 
      #print(which(users %in% i))
      omega <- train[which(train$user_id==i),-1]
      restlist <- v[which(rests %in% omega$business_id),]
      
      p <- prior + (t(restlist) %*% restlist)
      
      u[which(users==i),] <- solve(p) %*% (t(restlist) %*% omega$stars) 
    }
    write.csv(u,paste0("u",step,".csv"),row.names=F)
    print("finished u")          
    # update movie locations
    
    for(j in rests){
      omega <- train[which(train$business_id==j),-2]
      uselist <- u[which(users %in% omega$user_id),]
      
      #print(which(rests %in% j))
      
      if(nrow(omega)==1){
        p <- prior + (uselist %*% t(uselist))
        v[which(rests==j),] <- solve(p) %*% (uselist * omega$stars)        
      }else{
        p <- prior + (t(uselist) %*% uselist)
        v[which(rests==j),] <- solve(p) %*% (t(uselist) %*% omega$stars)
      }
    }
    write.csv(v,paste0("v",step,".csv"),row.names=F)
    print("finished v")

    # predict all user-movie ratings
    preds <- u %*% t(v)
    

    # calculate test predictions
    tstpreds <- preds * testrests_by_user
    tstpreds <- round(tstpreds[tstpreds != 0])
    tstpreds[tstpreds > 5] <- 5
    tstpreds[tstpreds < 1] <- 1
  
    # compute RMSE for this iteration (based on test set)
    RMSE[step] <- sqrt(mean((test$stars - tstpreds)^2))
    write.csv(RMSE,"rmse.csv",row.names=F)
    print(paste("finished iteration", step))
  }
  return(list(L,RMSE,cbind(rests,v),u))
}

results <- MatrixFactorize(train, test)

loss <- as.data.frame(cbind(x=1:100,y=results[[1]]))
ggplot(loss, aes(x=x,y=y)) + geom_line() + 
  labs(x="Iteration",y="Objective Function\n(Log Joint Likelihood)") + theme_bw()

RMSE <- as.data.frame(cbind(x=1:100,y=results[[2]]))
ggplot(RMSE, aes(x=x,y=y)) + geom_line() + labs(x="Iteration",y="RMSE") + theme_bw()

v <- read.csv("v1.csv")

u <- read.csv("u1.csv")

