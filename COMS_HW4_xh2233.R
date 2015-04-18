## COMS W4721 Homework 4
# Xu Huang
# Professor Paisley
# Homework due April 14th, 2015

require("reshape2")
require("ggplot2")
require("plyr")
require("dplyr")
require("MASS")
require("psych")
require("stats")

setwd("C:/Users/Conan/Desktop")  # Change to your own working directory

## Problem 2 Matrix Factorization

# Part a
# Plot RMSE and log joint likelihood as a function of iterations
L2_norm <- function (x){
  L <- sqrt(sum(x^2))
  return (L)
}

mf <- function (sigma, d, lamda, iterations){
  
  # Read in data 
  movies <- read.csv("movies.txt", header = F)
  movies_ma <- as.matrix(movies)
  ratings <- read.csv("ratings.txt", header = F)
  ratings_test <- read.csv("ratings_test.txt", header = F)
  ratings_test <- mutate(ratings_test, ratings_pred = 0)
  
  # Create training sets
  train <- matrix(data = NA, nrow = 943, ncol = 1682)
  ratings_dim <- dim(ratings)
  for (i in 1:ratings_dim[1]){
    train[ratings[i,1],ratings[i,2]] <- ratings[i,3]
  }
  
  # Initialization
  u <- matrix(NA, nrow = d, ncol = nrow(train))
  v <- matrix(0, nrow = d, ncol = ncol(train))
  final <- matrix(NA, nrow = iterations, ncol = 2)
  
  # Initialize v_j
  v <- mvrnorm(n = ncol(train), mu = rep(0,d), Sigma = 1/lamda * diag(d))
  v <- as.matrix(t(v))
  
  for (i in 1:iterations){
    
    # Update user location per iteration
    second_term = 0    !!!!
    for (j in 1:nrow(train)){
      sum_u_left <- matrix(0, nrow = d, ncol = d)
      sum_u_right <- 0
      index_u <- which(!is.na(train[j, ]))
      for (k in 1:length(index_u)){
        sum_u_left <- sum_u_left + v[ ,index_u[k]] %*% t(v[ ,index_u[k]])
        sum_u_right <- sum_u_right + train[j,index_u[k]] * v[ ,index_u[k]]
      }
      u[ ,j] <- ginv(lamda*sigma*diag(d) + sum_u_left) %*% sum_u_right
      second_term <- second_term + L2_norm(u[ ,j])  !!!
        
    }
    
    # Update object location per iteration
    third_term = 0
    for (l in 1:ncol(train)){
      sum_v_left <- matrix(0, nrow = d, ncol = d)
      sum_v_right <- 0
      index_v <- which(!is.na(train[ ,l]))
      if (length(index_v) > 0){
        for (m in 1:length(index_v)){
          sum_v_left <- sum_v_left + u[ ,index_v[m]] %*% t(u[ ,index_v[m]])
          sum_v_right <- sum_v_right + train[index_v[m],l] * u[ , index_v[m]]
        }
        v[ ,l] <- ginv(lamda*sigma*diag(d) + sum_v_left) %*% sum_v_right
        third_term <- third_term + L2_norm(v[ ,l])  !!!!
      }
    }
    
    # Calculate RMSE
    for (n in 1:nrow(ratings_test)) {
      rating_pred <- t(u[ ,ratings_test[n,1]]) %*% v[ ,ratings_test[n,2]]
      if (rating_pred >= 5) {
        ratings_test[n,4] = 5
      }
      else if (rating_pred <= 1) {
        ratings_test[n,4] = 1
      }
      else {
        ratings_test[n,4] = round(rating_pred)
      }
    }
    final[i,1] <- sqrt(mean((ratings_test[ ,3] - ratings_test[ ,4])^2))
    
    # Calculate the joint likelihood
    first_term <- 0
    for (p in 1:length(ratings)){
      first_term <- first_term + (train[ratings[p,1],ratings[p,2]] - t(u[ ,ratings[p,1]]) %*% v[ ,ratings[p,2]])^2
    }
    
    final[i,2] <- -1/(2*sigma) * first_term - lamda/2 * second_term - lamda/2 * third_term
    
  }
  
  print(final)
  return (final)
}

result <- mf(0.25, 20, 10, 100)
RMSE <- result[,1]
log_like <- result[,2]

# Plot RMSE on the test set as a function of training iterations
iterations = 1:100
plot <- cbind(RMSE, iterations)
plot <- melt(plot, id = "iterations")
plot_RMSE <- ggplot(plot, aes(x = iterations, y = RMSE, color = "red")) +
  geom_line() + labs(x = "Iterations", y = "RMSE", title = "RMSE as a function of iterations") +
  theme(legend.title = element_blank()) + 
  scale_colour_discrete(labels = "Testing")
plot_RMSE

# Plot log likelihood as a function of training iterations
iterations = 1:100
plot_l <- cbind(log_like, iterations)
plot_l <- melt(plot_l, id = "iterations")
plot_log <- ggplot(plot_l, aes(x = iterations, y = log_like)) +
  geom_line() + labs(x = "Iterations", y = "Log Likelihood", title = "Log joint likelihood as a function of iterations") +
  theme(legend.title = element_blank())
plot_log

## Part 2
# Pick 3 movies and find 5 closest movies using Euclidean Distance
# I pick No.25 Braveheart, No.33 Apollo 13, and No.66 Pulp Fiction
match = matrix(NA, nrow = ncol(v), ncol = 1)
top_match = matrix(NA, nrow = 5, ncol = 1)

# For Braveheart
for (i in 1:nrow(match)){
  if (i != 25){
    match[i] = L2_norm(v[ ,25] - v[ ,i])
  }
}
for (j in 1:5){
  top_match[j,1] <- which(match == sort(match)[j])
  print (movies[top_match[j,1],1])
}
sort(match)[1:5]

# For Apollo 13
for (i in 1:nrow(match)){
  if (i != 33){
    match[i] = L2_norm(v[ ,33] - v[ ,i])
  }
}
for (j in 1:5){
  top_match[j,1] <- which(match == sort(match)[j])
  print (movies[top_match[j,1],1])
}
sort(match)[1:5]

# For Pulp Fiction
for (i in 1:nrow(match)){
  if (i != 66){
    match[i] = L2_norm(v[ ,66] - v[ ,i])
  }
}
for (j in 1:5){
  top_match[j,1] <- which(match == sort(match)[j])
  print (movies[top_match[j,1],1])
}
sort(match)[1:5]

## Part 3
# Perform K means with k = 30. Show 10 movies with the most positive dot product for each of the 5 centroid 
set.seed(10)
index <- sample(943, 30)
init_center <- cbind(u[,index])
clusters <- kmeans(t(u),t(init_center),20)

set.seed(20)
centers_5 <- sample(30,5)
dot_prod <- matrix(0, nrow = ncol(v), ncol = 5)
for (i in 1:5){
  for (j in 1:ncol(v)){
    dot_prod[j,i] <- clusters$centers[centers_5[i], ] %*% v[ ,j]
  }
  sorted <- sort(dot_prod[ ,i], decreasing = TRUE)
  for (t in 1:10){
    movie_index <- which(dot_prod[ ,i] == sorted[t])
    movie = movies[movie_index,1]
    print (movie)
  }
}


