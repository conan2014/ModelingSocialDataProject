rm(list=ls())
library(dplyr)
library(Matrix)
library(magrittr)
library(readr)
library(recommenderlab)
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


