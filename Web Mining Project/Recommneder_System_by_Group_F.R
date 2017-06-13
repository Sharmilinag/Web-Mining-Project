# Set path to the directory containing reference dataset files.
setwd("C:\\Users\\Sharmili\\Desktop")
grouped_data <- read.table("processed_categories_frequency.csv",sep=",",head=TRUE, fileEncoding="UTF-8")

"fpc"# Including required libraries
library(plyr)
library(RCurl)
library(RJSONIO)
library(fpc)
library(mclust)
library(kknn)
library(flexclust)
library(cluster)
library(class)
library(rpart)
library(lsa)

# Data Analysis

originalData <- read.csv("processed_data.csv",head=TRUE)
grouped_data <- read.table("processed_categories_frequency.csv",sep=",",head=TRUE, fileEncoding="UTF-8")
summary(grouped_data)
str(grouped_data)

install.packages("gclus")
library(gclus)
dta <- grouped_data # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors

dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

# Processing Original data to find frequency of the venues based on the check-ins

venueCheckinCount <- function()
{
  # Reading Original data from "processed_data.csv"
  venueData <- read.csv("processed_data.csv", head=TRUE)
  names(venueData) <- c("UserID","VenueID","VenueCategoryID","Subcategory","Category")
  # Sorting the data into table based on the venueIDs. 
  count <- table(venueData$VenueID)
  # Creating data frame of the venue based sorted data.
  count <-as.data.frame(count)
  count<-count[order(-count$Freq),]
  return (list(venueData,count))
}

# Creating Clusters
clusterCreation <- function()
{
  # Reading the processed data containing frequencies based on venues.
  grouped_data <- read.table("processed_categories_frequency.csv",sep=",",head=TRUE, fileEncoding="UTF-8")
  row_count <-nrow(grouped_data)
  col_count <-ncol(grouped_data)
  while(col_count != '0')
  {
    while(row_count != '0')
    {
      # For effective results, setting value to 0 or 1 based on median value
      grouped_data[row_count,col_count] <- if(grouped_data[row_count,col_count] > mean(grouped_data[,col_count], trim = 0.1)) 1 else 0;
      row_count = row_count - 1
    }
    col_count = col_count - 1
  }
  
  # Identifying Test & Training Dataset  
  # Forming Test data which consist of 10% of the total dataset
  test_data <- grouped_data[c(973:1083),]
  training_data <- grouped_data[-c(973:1083),]
  
  
  # Performing Kmeans Clustering, ignoring first column as it has the userID
  kmeans_result <- kmeans(x = scale(training_data[,-c(1)]),centers = 7, nstart = 1, )
  # Drawing 2-D cluster plot  
  clusplot(training_data, kmeans_result$cluster, col.p = kmeans_result$cluster)
  
  # Performing Classification on the Training Dataset 
  training_data$class <- as.factor(kmeans_result$cluster)
  
  # Using Recursive Partitioning and Classification Trees
  classification <- rpart(training_data$class ~., data = training_data[,c(2:10)],
                          method = "class", parms = list(split='information'),
                          control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))
  
  # Model prediction using classification algorithm and test dataset
  predicted_clusters <- predict(classification, test_data )
  
  # Classes Identification
  classes_identified <- apply(predicted_clusters, 1, which.max)  
  return(list(grouped_data,training_data,classes_identified))
}

# Finding Similar user from the user set present in the training dataset using classes identified.
findingSimilarUsers <- function(grouped_data, training_data, classes_identified)
{
  
  grouped_data_subcategory <- read.table("processed_data_subcategories.txt",sep=",",head=TRUE)
  
  # Forming Test data out of sub categories which consist of 10% of the total dataset
  test_data_subcategory <- grouped_data_subcategory[c(973:1083),]
  grouped_data_subcategory <- grouped_data_subcategory[-c(973:1083),]
  
  # Cosine similarity detection
  cosinesim <- data.frame(numeric(0), numeric(0), numeric(0))
  colnames(cosinesim) <- c("SimUser","Sim","User")
  
  test_count <-nrow(test_data_subcategory)
  
  while(test_count != '0')
  {
    groupedData_subset <- grouped_data[training_data$class==classes[test_count],1]
    groupedData_subset <- grouped_data_subcategory[groupedData_subset,]
    
    usersim <- rep(NA, nrow(groupedData_subset))
    groupedData_subset
    group_count <-nrow(groupedData_subset)
    
    while(group_count != '0')
    {
      # Forming a vector usersim, containing cosine similarity between the grouped dataset and the test data subcategory dataset.
      usersim[group_count] <- cosine(unlist(groupedData_subset[group_count,-1]),unlist(test_data_subcategory[test_count,-1]))
      group_count = group_count - 1
    }
    
    result<-data.frame(groupedData_subset[,1],usersim)
    result <- result[order(-usersim),]
    result <- result[1:5,]
    result$User <- test_count
    cosinesim <- rbind(cosinesim,result)
    test_count = test_count - 1
  }
  return(cosinesim)
}


recommendedResults <- function(userNum, originalData, cosinesim, frequencies, category) 
{
  j <- userNum
  test_data <- originalData[originalData$UserID==1070+j & originalData$Category==category,]
  test_data <- test_data$VenueID
  # Finding unique test data
  test_data <- unique(test_data)
  # Finding places visited by similar user
  similarUserVenues <- data.frame()
  for(i in 1:5)
  {
    similarUserVenuesNew <- originalData[originalData$UserID==cosinesim[5*(j-1)+i,1] & originalData$Category==category,]
    similarUserVenuesNew <- similarUserVenuesNew$VenueID
    similarUserVenues <- rbind(similarUserVenues,as.data.frame(similarUserVenuesNew))
  }
  
  # Finding unique similar user venues 
  similarUserVenues <- unique(similarUserVenues)
  
  
  # Finding unvisited venues by the user
  unvisited_venues <- subset(similarUserVenues, !similarUserVenuesNew %in% test_data)
  unvisited_venues <- frequencies[frequencies$Var1 %in% unvisited_venues$similarUserVenuesNew,]
  cat_subcat <- unique(originalData[,4:5])
  unvisited_venues <- unvisited_venues[0:0,]
  unvisited_venues <- unvisited_venues[complete.cases(unvisited_venues),]
  
  if(nrow(unvisited_venues) < 5)
  {
    # Finding unique venues by categories 
    VenueByCat <- unique(originalData[originalData$Category==category,]$VenueID)
    VenueByCat <- frequencies[frequencies$Var1 %in% VenueByCat,]
    # Finding venues not visited by the user in the test data
    VenueByCat <- VenueByCat[!VenueByCat$Var1 %in% test_data,]
    VenueByCat <- VenueByCat[!VenueByCat$Var1 %in% unvisited_venues$Var,]
    # Number of venues to be added
    x <- 5-nrow(unvisited_venues)
    # Add new venues to unvisited
    unvisited_venues <- rbind(unvisited_venues,VenueByCat[1:x,])
  }
  return(unvisited_venues)
}


# Fetching venue names from the Foursquare Api
getNames <- function(venues) {
  names <- NA
  for(i in 1:nrow(unvisit)){
    vId <- unvisit[i,]$Var1
    req <- paste("https://api.foursquare.com/v2/venues/",vId,"?oauth_token=0KAKWZJ0R11DRVSY2JR0WVYRD55PARANMLEOYM45MLHJSCK3&v=20170506",sep="")
    res <- getURL(req)
    obj <- fromJSON(res)
    names[i] = obj$response$venue$name
  }
  return(names)
}


# Main Function

processedData <- venueCheckinCount()
clusterList <- clusterCreation()
grouped_data <- as.data.frame(clusterList[[1]])
training_data <- as.data.frame(clusterList[[2]])
classes <- as.vector(clusterList[[3]])
print(classes)
cosinesimtopusers <- findingSimilarUsers(grouped_data,training_data, classes)
print(cosinesimtopusers)
categories <- read.csv("categories.csv",head=FALSE)
categories <- as.character(categories[,1])
again = 'y'

while(again != 'n')
{
  cat("\nLogin ID: ")
  userNum <- scan(what=numeric(),nmax=1,quiet=TRUE)
  cat("\n1:  Shop & Service                   2:  Outdoors & Recreation      3:  Residence
       \n4:  Professional & Other Places      5:  Food                       6:  Travel & Transport
       \n7:  Arts & Entertainment             8:  College & University       9:  Nightlife Spot
       \n10: Athletic & Sport")
  
  cat("\n\nPlease enter category number: ")
  category <- scan(what=numeric(),nmax=1,quiet=TRUE)
  category <- categories[category]
  unvisit <- recommendedResults(userNum,processedData[[1]],cosinesimtopusers,processedData[[2]],category)
  withNames <- getNames(unvisit)
  
  cat("Top recommendations based on User History:\n")
  for(i in 1:nrow(unvisit))
    {
    cat(i,"] ",withNames[i],"\n",sep="")
    
    }
  
  cat("\nDo you wish to continue (y/n): ")
  again <- scan(what=character(),nmax=1,quiet=TRUE)
}

