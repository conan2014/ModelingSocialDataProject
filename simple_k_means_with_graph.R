library(psych)
library(cluster)

##Define sub as the subset of variables you want to cluster on
sub <- #INSERT
  
#create plot of cluster explanatory power
wss <- (nrow(sub)-1)*sum(apply(sub,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(sub, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")  

#------------------------------------------------------------------------------------#
f
it <- kmeans(sub, 8) ### Need to specify the # of clusters you want to look for
aggregate(sub,by=list(fit$cluster),FUN=mean)  ### get cluster means ###
table1<-data.frame(aggregate(sub,by=list(fit$cluster),FUN=mean))
fit  ### see what cluster command did in detail ###
sub1 <- data.frame(sub, fit$cluster)  ### append cluster assignment  ###
head(sub1) ### look at the dataset again ###