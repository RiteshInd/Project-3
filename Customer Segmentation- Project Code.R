# Customer Segmentation Project

# Loading required libraries
library(dplyr); library(plyr)
library(cluster); library(fpc)
library(mclust); library(sqldf)

# Reading the data set
cust_seg <- read.csv("Data.csv", T, ",")

# View of the data set
summary(cust_seg)
str(cust_seg)
table(is.na(cust_seg))
length(unique(cust_seg$RetailStore))
length(unique(cust_seg$CustomerID))
length(unique(cust_seg$TransactionID))

# Converting the class of the column as required
cust_seg$RetailStore <- as.factor(cust_seg$RetailStore)
cust_seg$CustomerID <- as.factor(cust_seg$CustomerID)
cust_seg$TransactionID <- as.factor(cust_seg$TransactionID)
cust_seg$ProductID <- as.factor(cust_seg$ProductID)

# Converting the date format
cust_seg$Transaction.Time <- gsub('/', '-', cust_seg$Transaction.Time)

cust_seg$Transaction.Time1 <- strptime(cust_seg$Transaction.Time,
                                     format='%d-%m-%Y %H:%M')

cust_seg$Transaction.Time <- as.factor(substr(cust_seg$Transaction.Time, 1, 11))

summary(cust_seg[which(is.na(cust_seg$Transaction.Time1)),])

# We observe that 75 NAs in the transaction column contains data of 29th Feb 2011, so it is
# removed from the data set as 2011 was not leap year

cust_seg$Transaction.Time1 <- as.Date(cust_seg$Transaction.Time1, 
                                      format = '%Y-%m-%d %H:%M')
str(cust_seg)
table(is.na(cust_seg))

cust_seg <- na.omit(cust_seg)
table(is.na(cust_seg))

# Creating group of customer
agg_cust_id <- aggregate(cust_seg, by=list(cust_seg$CustomerID), 
                         function(x) 
                           length(unique(x)))

agg_cust_id_new <- aggregate(cust_seg$Cost ~ CustomerID, cust_seg, sum)

agg_new <- merge(agg_cust_id, agg_cust_id_new, by.x = c("Group.1"), by.y = "CustomerID")


#Removing columns which aren't required 
agg_new <- agg_new[-c(3,5,7)]

# Renaming aggr column
agg_new <- rename(agg_new, 
                  c("Group.1"="CustomerID", 
                    "RetailStore"="StoreFrequency",
                    "TransactionID"="TransactionFrequency",
                    "ProductID"="ProductFrequency",
                    "cust_seg$Cost"="RevenueValue"))

# Checking for structure of the data set and converting as required
str(agg_new)
agg_new$RevenueValue <- as.integer(agg_new$RevenueValue)


#Checking all types are good to go
sapply(agg_new, class)


#Checking for outliers
boxplot(agg_new)

#Analyze for outliers
boxplot(agg_new[,c(3)])
boxplot(agg_new[,c(4)])


# from the above plots we observe that there are so many entries whose values are in 
# the outlier category. 
# Hence, it would not be a good idea to remove this outlier.

# Since the data attributes are of different varieties their scales are also different. 
# In order to maintain uniform scalability we scale the columns.
agg_new_cl <- scale(agg_new[,2:6])


# Calculating variance and storing at the first index in wss
wss <- (nrow(agg_new_cl)-1)*sum(apply(agg_new_cl,2,var))


# iterate through wss array 15 times and sum up all the variance in every 
# iteration and store it in wss array

for (i in 2:15){
  wss[i] <- sum(fit=kmeans(agg_new_cl,centers=i,15)$withinss)
}

# plot each iteration to display the elbow graph

plot(1:15, wss, type="b",main="15 clusters",
     xlab="Number of Clusters",
     ylab="Within cluster sum of squares")


# As we can see from the above output the slope of the graph changes majorly in 4 iteration, 
# hence we consider the optimized number of cluster as 3 in which we can get the optimum result
set.seed(111)
fit <- kmeans(agg_new, 4)


# Let's check the summary of the kmeans objects
fit$iter


# checking withinss i.e. the intra cluster bond strength factor for each cluster
fit$withinss

# checking betweenss i.e. the inter cluster distance between cluster
fit$betweenss

# Interpreting mined patterns
plot(agg_new_cl,col=fit$cluster,pch=15)
points(fit$centers,col=1:8,pch=6)

plotcluster(agg_new_cl,fit$cluster)
points(fit$centers,col=1:8,pch=16)

clusplot(agg_new_cl, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#Writing the Clusters to a file
myDF <- agg_new
myDF$Cluster_N <- fit$cluster
sapply(myDF, class)

write.csv(myDF, "File1.csv")

# Model Based Clustering
fit2 <- Mclust(agg_new_cl)
plot(fit2) # plot results 
summary(fit2) # display the best model

# Hierarchical clustering
# Ward Hierarchical Clustering
d <- dist(agg_new_cl, method = "euclidean") # distance matrix
fit1 <- hclust(d, method="ward.D") 
plot(fit1) # display dendogram

# comparing 2 cluster solutions
cluster.stats(d, fit$cluster, fit2$cluster)


# Analysis of cluster using cluster by KMeans
barplot(tapply(myDF$TransactionFrequency, 
               myDF$Cluster_N, FUN = sum),
        col = gray.colors(5),
        main = "Plot of Transaction Frequency V/S Cluster Group",
        xlab = "Cluster Group",
        ylab = "Transaction Frequency")

barplot(tapply(myDF$ProductFrequency, myDF$Cluster_N, FUN = sum),
        col = heat.colors(4),
        main = "Plot of Product Frequency V/S Cluster Group",
        xlab = "Cluster Group",
        ylab = "Product Frequency")

barplot(tapply(myDF$RevenueValue, myDF$Cluster_N, FUN = sum),
        col = heat.colors(6),
        main = "Plot of Revenue Generated V/S Cluster Group",
        xlab = "Cluster Group",
        ylab = "Revenue Generated")

# Analyzing for each customer

myDF1 <- sqldf("select Cluster_N, avg(TransactionFrequency) Avg_tr
      from myDF group by Cluster_N")

myDF2 <- sqldf("select Cluster_N, avg(ProductFrequency) Avg_pr
      from myDF group by Cluster_N")

myDF3 <- sqldf("select Cluster_N, avg(RevenueValue) Avg_rv 
               from myDF group by Cluster_N")

# Plotting Data Of Each Cluster
barplot(myDF1$Avg_tr, myDF1$Cluster_N, 
        col = heat.colors(5),
        main = "Plot of Transaction Frequency V/S Cluster Group",
        xlab = "Cluster Group",
        ylab = "Average Transaction Frequency",
        names.arg = c(1,2,3,4))

barplot(myDF2$Avg_pr, myDF2$Cluster_N, 
        col = gray.colors(5),
        main = "Plot of Product Frequency V/S Cluster Group",
        xlab = "Cluster Group",
        ylab = "Average Product Frequency",
        names.arg = c(1,2,3,4))

barplot(myDF3$Avg_rv, myDF3$Cluster_N, 
        col = gray.colors(5),
        main = "Plot of Revenue Generated V/S Cluster Group",
        xlab = "Cluster Group",
        ylab = "Average Revenue Generated",
        names.arg = c(1,2,3,4))


# After analyzing the data based on the plots, we can know categorise the cluster group
# of customer as Starter, Prospective, Advanced and Privileged.
# Cluster 1 is categorised as Prospective customers with people buying less products but more revenue.
# Cluster 2 is categorised as Privileged, group has customers with highest number of customer per revenue.
# Cluster 3 is categorised as Starter, Any new customer joining the system is likely to be put in the group.
# Cluster 4 is categorised as Advanced group has very few customers with buying products of high costs.
