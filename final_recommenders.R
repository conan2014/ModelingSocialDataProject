
rm(list=ls())
library(dplyr)
library(Matrix)
library(magrittr)
library(readr)
library(recommenderlab)
library(MASS)
setwd("SET DIRECTORY HERE")

##### SET UP #####
reviews <- read_csv("no_text_reviews.csv")
reviews$user_id %<>% as.factor
reviews$business_id %<>% as.factor

# pull out all restaurants
business <- read_csv("yelp_academic_dataset_business.csv")
business$state %<>% as.factor
restaurants <- business[grep("Restaurant",business$categories),]

# pull out all reviews of restaurants in AZ
AZrests <- 
  dplyr::filter(restaurants,state=="AZ") %>% 
  dplyr::select(business_id) %>% 
  unlist
AZreviews <- 
  filter(reviews,business_id %in% AZrests) %>% 
  droplevels

AZmatrix <-  sparseMatrix(i=as.numeric(AZreviews$user_id),
                          j=as.numeric(AZreviews$business_id),
                          x=AZreviews$stars,
                          use.last.ij=TRUE)

# purge users with fewer than 10 reviews
numreviews <- rowSums(AZmatrix != 0)
active.users <- which(numreviews > 9)

act.user.reviews <- 
  AZreviews[as.numeric(AZreviews$user_id) %in% active.users,] %>% 
  dplyr::select(user_id,business_id,stars) %>%
  droplevels

## END SET UP ###

#### Collaborative Filtering / Matrix Factorization algorithm

MF <- function(act.user.reviews,sigsq=0.25, lambda=10, d=20, iterations=100){
  
  # define our prior belief
  prior <- lambda * sigsq * diag(1, nrow=d, ncol=d) 
  
  
  ## remove duplicates and perform training/test split
  train <- act.user.reviews
  
  train$id <- paste0(train$user_id,train$business_id)
  train <- train[!duplicated(train$id),-4]
  
  ndx <- sample(nrow(train),nrow(train)*0.1,FALSE)
  
  test <- train[ndx,]
  train <- train[-ndx,]
    
  # create sorted index list of unique users and restaurants in training set
  # sorting is just convenience for when we need to calculate RMSE and objective function later
  
  # remove test rows for which restaurant or user is not in training set
  feasiblerows <- test$business_id %in% unique(train$business_id) & 
    test$user_id %in% unique(train$user_id)
  test <- test[feasiblerows,]
  rm(feasiblerows)
  
  # recombine to drop levels so that factor levels stay the same
  op <- rbind(train,test) %>% droplevels
  
  train <- op[1:nrow(train),]
  test <- op[c(nrow(train)+1):nrow(op),]
  
  # create lists of unique users and restaurants
  users <- as.character(unique(train$user_id))
  rests <- as.character(unique(train$business_id))
  
  # sort to facilitate RMSE calculation  
  train <- arrange(train,business_id,user_id)
  test <- arrange(test,business_id,user_id)
  
  # create matrix sparse indicator matrix for test set (used to separate out predictions)
  testrests_by_user <- sparseMatrix(i=as.numeric(test$user_id),
                                    j=as.numeric(test$business_id),
                                    x=T,
                                    dims=c(length(users),length(rests))) %>% as.matrix
  
  # initialize u and randomly initialize v
  v <- mvrnorm(length(rests),rep(0,d),diag(1/lambda,d,d))
  u <- matrix(0,nrow=length(users),ncol=d)
  
  # initialize RMSE vector
  RMSE <- rep(0,iterations)
  
  for(step in 1:iterations){
    
    # update user matrix
    for(i in users){ 

      omega <- train[which(train$user_id==i),-1]
      restlist <- v[which(rests %in% omega$business_id),]
      
      p <- prior + (t(restlist) %*% restlist)
      
      u[which(users==i),] <- solve(p) %*% (t(restlist) %*% omega$stars) 
    }

    # update restaurant matrix
    for(j in rests){
      omega <- train[which(train$business_id==j),-2]
      uselist <- u[which(users %in% omega$user_id),]
        
      if(nrow(omega)==1){
        p <- prior + (uselist %*% t(uselist))
        v[which(rests==j),] <- solve(p) %*% (uselist * omega$stars)        
      }else{
        p <- prior + (t(uselist) %*% uselist)
        v[which(rests==j),] <- solve(p) %*% (t(uselist) %*% omega$stars)
      }
    }
    
    # generate all predictions
    preds <- u %*% t(v)
    
    
    # calculate test predictions
    tstpreds <- preds * testrests_by_user
    tstpreds <- round(tstpreds[tstpreds != 0])
    tstpreds[tstpreds > 5] <- 5
    tstpreds[tstpreds < 1] <- 1
    
    # compute RMSE for this iteration (based on test set)
    RMSE[step] <- sqrt(mean((test$stars - tstpreds)^2))
  }
  return(list(RMSE,v,u))
}

results <- MF(act.user.reviews)



##### kNN Setup #####

act.review.mat <- sparseMatrix(i=as.numeric(act.user.reviews$user_id),
                               j=as.numeric(act.user.reviews$business_id),
                               x=act.user.reviews$stars,
                               use.last.ij=TRUE)

userlist <- unique(train$user_id)

### read in network summary data
netdeg <- read.csv("yelp_network_degree.csv")
# pull out relevant users
actAZnetdeg <- netdeg[netdeg$user_id %in% userlist,]
# identify users without network data and give them dummy data
friendless_users <- userlist[!(userlist %in% actAZnetdeg$user_id)] 
azlist <-
  cbind(user_id = friendless_users, 
        node = NA,
        degree = 1,
        degree2 = 1) %>% as.data.frame
azlist$degree %<>% as.integer
azlist$degree2 %<>% as.integer
azlist$node %<>% as.integer
azlist$user_id %<>% as.character
actAZnetdeg$user_id %<>% as.character

full <- rbind(actAZnetdeg,azlist)
full$user_id %<>% as.factor

full$degree[full$degree <= 1] <- 1.1
full$degree2[full$degree2 <= 1] <- 1.1

# combine full network summary data with review count data from the user file
user <- read_csv("yelp_academic_dataset_user.csv")
subuser <- user[user$user_id %in% userlist,]
rm(user)
subuser %<>% select(user_id,review_count)
full %<>% left_join(subuser)
full$user_id %<>% as.factor

rm(list=c("azlist","netdeg","subuser","actAZnetdeg","friendless_users"))

#### End kNN Set up ####


##### KNN RECOMMENDER #####

res <- knnrec(test,train,full,userlist,act.review.mat)

knnrec <- function(test,train,full,userlist,act.review.mat){
  
  tstSEs <- matrix(0,nrow=nrow(test),ncol=140) # 7 weights x 20 values of k
  
  for(i in 1:nrow(test)){
    ### find all users who rated that business, 
    biz <- test[i,2] %>% as.character
    user <- test[i,1] %>% as.character
    stars <- train[train$business_id==biz,3] %>% unlist
    
    print(paste(i,length(stars)))
    ### find nearest k to test point (USING TRAIN DATA)
    
    simusers <- train[train$business_id==biz,1] %>% unlist
    usernums <- match(simusers,userlist)
    
    if(length(usernums) == 1){
      tstSEs[i,] <- rep((stars-unlist(test[i,3]))^2,ncol(tstSEs))
    }
    else{
      simusermat <- act.review.mat[usernums,] %>% as.matrix
      d <- simusermat %>%
        sweep(2,act.review.mat[match(user,userlist),]) %>%
        .^2 %>% rowSums
      
      userndnums <- match(simusers,full$user_id)
      dweight1 <- full$degree[userndnums] %>% unlist
      dweight2 <- full$degree2[userndnums] %>% unlist
      
      dweight1L <- full$degree[userndnums] %>% unlist %>% log
      dweight2L <- full$degree2[userndnums] %>% unlist %>% log
      
      rcweight  <- full$review_count[userndnums] %>% unlist
      rcweightL <- full$review_count[userndnums] %>% unlist %>% log
      
      weights <- list(dweight1,dweight2,dweight1L,dweight2L,rcweight,rcweightL)
      ### take avg of points, then round
      orderd <- stars[order(d)]
      r <- c()
      
      for(j in seq(3,41,2)){
        r %<>% c(orderd %>% head(j) %>% mean %>% round)
      }
      
      for(w in weights){
        q <- c()
        for(j in seq(3,41,2)){
          q %<>% c(orderd %>% head(j) %>% weighted.mean(head(w,j)) %>% round)
        }
        r %<>% c(q)
      }
      SE <- (r - unlist(test[i,3]))^2
      tstSEs[i,] <- SE
    }
  }
  return(tstSEs)
}


## post processing RMSEs to produce the graph

RMSEs <- res %>% colMeans %>% sqrt
RMSEs %<>% matrix(ncol=7,nrow=20,byrow=F)

row.names(RMSEs) <- seq(3,41,2)
RMSEs %<>% as.data.frame
names(RMSEs) <- c("unweighted","deg1","deg2","deg1log","deg2log","revcount","revcountlog")
RMSEs$k <- row.names(RMSEs)
library(reshape2)
mRMSEs <- RMSEs %>% melt(id.vars="k")
mRMSEs$k %<>% as.numeric
ggplot(mRMSEs, aes(x=k,group=variable,color=variable,y=value)) + geom_line() + 
  geom_hline(y=1.312,linetype="dashed") + labs(y="RMSE") + scale_color_brewer(palette="Set1")

