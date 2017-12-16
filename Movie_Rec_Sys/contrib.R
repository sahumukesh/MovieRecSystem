# Program to build a Movie Recommendation System

# Remove all objects and variables.
rm(list = ls())

# Install required libraries.
library(recommenderlab)
library(reshape2)
library(proxy)

# Now load the necessary dataset to implement model.
movies <- read.csv("movies.csv", header = TRUE, stringsAsFactors=FALSE)
ratings <- read.csv("ratings.csv", header = TRUE)
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]

movie_recommendation <- function(input,input2,input3) {
  #input = "Gladiator (2000)"
  #input2 = "Aeon Flux (2005)"
  #input3 = "Alexander (2004)"
  row_num <- which(movies2[,2] == input)
  row_num2 <- which(movies2[,2] == input2)
  row_num3 <- which(movies2[,2] == input3)
  userSelect <- matrix(NA,10325)
  userSelect[row_num] <- 5 
  userSelect[row_num2] <- 4 
  userSelect[row_num3] <- 3 
  userSelect <- t(userSelect)
  
  rating_mat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
  rating_mat <- rating_mat[,-1]
  colnames(userSelect) <- colnames(rating_mat)
  rating_mat2 <- rbind(userSelect,rating_mat)
  rating_mat2 <- as.matrix(rating_mat2)
  
  #Convert rating matrix into a sparse matrix
  rating_mat2 <- as(rating_mat2, "realRatingMatrix")
  
  #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
  recommender_model <- Recommender(rating_mat2, method = "UBCF",param=list(method="Cosine",nn=30))
  recom <- predict(recommender_model, rating_mat2[1], n=10)
  recom_list <- as(recom, "list")
  no_result <- data.frame(matrix(NA,1))
  recom_result <- data.frame(matrix(NA,10))
  if (as.character(recom_list[1])=='character(0)'){
    no_result[1,1] <- "Sorry, there is not enough information in our database on the movies you've selected. Try to select different movies you like."
    colnames(no_result) <- "No results"
    return(no_result) 
  } else {
    for (i in c(1:10)){
      recom_result[i,1] <- as.character(subset(movies, 
                                               movies$movieId == as.integer(recom_list[[1]][i]))$title)
    }
  colnames(recom_result) <- "User-Based Collaborative Filtering Recommended Titles"
  return(recom_result)
  }
}