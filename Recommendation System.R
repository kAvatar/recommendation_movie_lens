#Project -Recommendation System
#Created on - 26/03/2018
#Created by Kanika Sharma



##############loading the libraries
library(data.table)
library(tidyverse)
library(ggplot2)
library(recommenderlab)
library(reshape2)

#Read data
movies = read.csv("D://Desktop/DirectMarketing/MoviesTry/movies.csv", header = TRUE) 
ratings = read.csv("D://Desktop/DirectMarketing/MoviesTry/ratings.csv", header = TRUE)
head(movies)
head(ratings)
total <- merge(ratings,movies,by="movieId")
head(total)
newdata <- total[c(2,5,3)]
head(newdata)
str(newdata)
newdata2 <- newdata[1:5000,]
str(newdata2)
g<-acast(newdata2, userId~title)
# Check the class of g
class(g)
# Convert it as a matrix
R<-as.matrix(g)
class(R)
# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
MovieLense <- as(R, "realRatingMatrix")
class(MovieLense)
dim(MovieLense)
vec <- as.vector(MovieLense@data)
unique(vec)
rating <- table(vec)
rating
#0 means missing value in this table, so lets remove it
vec <- vec[vec !=0]
a <-qplot(vec)
a + ggtitle("checking the plot") 
# + stat_bin(bins = 70)
#lets find out the nonmissing values in columns and avg value
movieviews<- colCounts(MovieLense)
movieavg <- colMeans(MovieLense)
#sort the movie by number of views
tb <- data.frame(movie= names(movieviews),views = movieviews)
tb <- tb [order(tb$views,decreasing = TRUE),]
head(tb)
a <-ggplot(tb[1:6,] ,aes(x=movie, y=views))
a +geom_bar(stat="identity") + ggtitle("Number of views")+ theme(axis.text.x =element_text(angle = 45, hjust = 1))
a <- qplot(movieavg)
a + stat_bin(binwidth = 0.1) +ggtitle("average of Movie Views")
#removing outliers

similarity_items <- similarity(MovieLense[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)
## look at the first few ratings of the first user
head(as(MovieLense[1,], "list")[[1]])


## visualize part of the matrix
image(MovieLense[1:100,1:100])

## number of ratings per user
hist(rowCounts(MovieLense))

## number of ratings per movie
hist(colCounts(MovieLense))

## mean rating (averaged over users)
mean(rowMeans(MovieLense))

## available movie meta information
# head(MovieLenseMeta)


relevant_rating  <- movieavg[movieviews >100]
a <- qplot(relevant_rating)
a + stat_bin(binwidth = 0.1) +ggtitle("average of Movie Views")
#heatmap
image(MovieLense,main ="Users rating before")
#zoom in the imahe, hard to read
image(MovieLense[1:10, 1:15], main = "Heatmap of the first rows and
columns")
#lets pick relevant movies
#minimum number of movies per user
min_n_movies <- quantile(rowCounts(MovieLense), 0.99)
min_n_movies
#minimum  number of users per movie
min_n_users <- quantile(colCounts(MovieLense), 0.99)
min_n_users
#selecting the user and movie per criteria
image(MovieLense[rowCounts(MovieLense)> min_n_movies, colCounts(MovieLense)> min_n_users], main ="Users rating-after")
#selcting more precise criteria like user who have voted at least 50 movie and moviies that have been watched at leat 100 times
ratings_movies <- MovieLense[rowCounts(MovieLense) > 10, colCounts(MovieLense) > 10]
ratings_movies
#taking only top 2% of it
min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)
image(ratings_movies[rowCounts(ratings_movies) > min_movies, colCounts(ratings_movies) > min_users], main = "Heatmap of the top users and movies")
average_ratings_per_user <- rowMeans(ratings_movies)
qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) +ggtitle("Distribution of the average rating per user")
#normalize data
norm <- normalize(ratings_movies)
# ratings_movies_norm <-sum(rowMeans(norm)> 0.00001)
# image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies, colCounts(ratings_movies_norm) > min_users], main = "Heatmap of the top users and movies")


# #Content based
# genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
# genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
# colnames(genres2) <- c(1:7)
# genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
# 
# genre_matrix <- matrix(0,8571,18) #empty matrix
# genre_matrix[1,] <- genre_list #set first row to genre list
# colnames(genre_matrix) <- genre_list #set column names to genre list
# 
# #iterate through matrix
# for (i in 1:nrow(genres2)) {
#   for (c in 1:ncol(genres2)) {
#     genmat_col = which(genre_matrix[1,] == genres2[i,c])
#     genre_matrix[i+1,genmat_col] <- 1
#   }
# }
# 
# #convert into dataframe
# genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
# for (c in 1:ncol(genre_matrix2)) {
#   genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
# } #convert from characters to integers
# binaryratings <- ratings
# for (i in 1:nrow(binaryratings)){
#   if (binaryratings[i,3] > 3){
#     binaryratings[i,3] <- 1
#   }
#   else{
#     binaryratings[i,3] <- -1
#   }
# }
# binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
# for (i in 1:ncol(binaryratings2)){
#   binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
# }
# binaryratings2 = binaryratings2[,-1] #r
# #Remove rows that are not rated from movies dataset
# movieIds <- length(unique(movies$movieId)) #8570
# ratingmovieIds <- length(unique(ratings$movieId)) #8552
# movies2 <- movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
# rownames(movies2) <- NULL
# #Remove rows that are not rated from genre_matrix2
# genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
# rownames(genre_matrix3) <- NULL
# #Calculate dot product for User Profiles
# result = matrix(0,18,706)
# for (c in 1:ncol(binaryratings2)){
#   for (i in 1:ncol(genre_matrix3)){
#     result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
#   }
# }
# 
# #Convert to Binary scale
# for (i in 1:nrow(result)){
#   if (result[i] < 0){
#     result[i] <- 0
#   }
#   else {
#     result[i] <- 1
#   }
# }
# result2 <- result[1,] #First user's profile
# sim_mat <- rbind.data.frame(result2, genre_matrix3)
# sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer
# 
# #Calculate Jaccard distance between user profile and all movies
# library(proxy)
# sim_results <- dist(sim_mat, method = "Jaccard")
# sim_results <- as.data.frame(as.matrix(sim_results[1:8552]))
# rows <- which(sim_results == min(sim_results))
# #Recommended movies
# movies[rows,2]



#binarizing the data
#if movie has been watched, then 1 if the rating is atleast 1
ratings_bin <- binarize(ratings_movies, minRating =1)
min_movies_binary <- quantile(rowCounts(ratings_movies), 0.95)
min_users_binary <- quantile(colCounts(ratings_movies), 0.95)
image(ratings_bin[rowCounts(ratings_movies) > min_movies_binary,colCounts(ratings_movies) > min_users_binary], main = "Heatmap of the top users and movies")







#train and test
sample <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies),  replace = TRUE, prob = c(0.8, 0.2))
train  <- ratings_movies[sample]
test  <- ratings_movies[! sample]






#IBCF
#exploring the recommendation Models-finding the parameters for IBCF
rm <- recommenderRegistry$get_entries(dataType= "realRatingMatrix")
names(rm)
#explore these models
lapply(rm, "[[", "description")
rm$IBCF_realRatingMatrix$parameters# see more info
#Building the recommender system 
recmodel <- Recommender(data =train,method="IBCF", parameter=list(k=30)) #k 30 is default
recmodel
class(recmodel)
whatisthismodel <- getModel(recmodel)
whatisthismodel$description
whatisthismodel$k
whatisthismodel$sim
image(whatisthismodel$sim[1:20, 1:20],main = "Heatmap of the first rows and columns")
#applying the recommender model on  test
qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of
the column count")
#lets pick a value that hoa many items to be recommended
topusers <- 6
#for top 6 users
prediction <- predict(object = recmodel, newdata = test, n = topusers)
prediction
class(prediction)
slotNames(prediction)
prediction@items
prediction@itemLabels
#create a matrix of recommendation for each user
recc_matrix <- sapply(prediction@items, function(x){  colnames(ratings_movies)[x]})
dim(recc_matrix)
recc_matrix
recc_matrix[,1:4]
recc_user_1 <- prediction@items[[1]]
movies_user_1 <- prediction@itemLabels[recc_user_1]
movies_user_1
#identify the best one
noofitems <- factor(table(recc_matrix))
title <- "distribution of  number of items  for IBCF"
qplot(noofitems) + ggtitle(title)
#lets see the popular movies
number_of_items_sorted <- sort(noofitems, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(names(number_of_items_top),
                        number_of_items_top)
table_top






#UBCF
#exploring the recommendation Models-finding the parameters for IBCF
rm_ubcf <- recommenderRegistry$get_entries(dataType= "realRatingMatrix")
names(rm_ubcf)
rm_ubcf$UBCF_realRatingMatrix$parameters# see more info
#Building the recommender system 
recmodel_ubcf <- Recommender(data =train,method="UBCF") #k 30 is default
recmodel_ubcf
class(recmodel_ubcf)
#get details about the model
whatisthismodel_ubcf <- getModel(recmodel_ubcf)
whatisthismodel_ubcf$description
whatisthismodel_ubcf$nn
names(whatisthismodel_ubcf)
#seeing the data inside it
whatisthismodel_ubcf$data
# image(whatisthismodel$sim[1:20, 1:20],main = "Heatmap of the first rows and columns")
#applying the recommender model on  test
#lets pick a value that hoa many items to be recommended
topusers_ubcf <- 6
#for top 6 users
prediction_ubcf <- predict(object = recmodel_ubcf, newdata = test, n = topusers_ubcf)
prediction_ubcf
class(prediction_ubcf)
slotNames(prediction_ubcf)
prediction_ubcf@items
prediction_ubcf@itemLabels
recc_user_1 <- prediction_ubcf@items[[1]]

movies_user_1 <- prediction_ubcf@itemLabels[recc_user_1]
movies_user_1
#create a matrix of recommendation for each user
recc_matrix_ubcf <- sapply(prediction_ubcf@items, function(x){  colnames(ratings_movies)[x]})
dim(recc_matrix_ubcf)
#check out top 4
recc_matrix_ubcf[,1:4]
#identify the best one by finding the frequency and building the histogram
noofitems_ubcf <- factor(table(recc_matrix_ubcf))
title <- "Recommendation Frequency by UBCF"
qplot(noofitems_ubcf) + ggtitle(title)
# Compared with the IBCF, the distribution has a longer tail. This means that there are
# some movies that are recommended much more often than the others. The maximum
# is 29, compared with 11 for IBCF
#lets see the popular movies
number_of_items_sorted_ubcf <- sort(noofitems_ubcf, decreasing = TRUE)
number_of_items_top_ubcf <- head(number_of_items_sorted_ubcf, n = 4)
table_top_ubcf <- data.frame(names(number_of_items_top_ubcf),
                             number_of_items_top_ubcf)
table_top_ubcf



#POPULAR MODEL
#Building the recommender system 
recmodel_pop <- Recommender(data =train, method = "POPULAR", param=list(normalize = "center"))
recmodel_pop
class(recmodel_pop)
whatisthismodel_pop <- getModel(recmodel_pop)
# image(whatisthismodel$sim[1:20, 1:20],main = "Heatmap of the first rows and columns")
#applying the recommender model on  test
#lets pick a value that hoa many items to be recommended
topusers <- 6
#for top 6 users
prediction_pop <- predict(object = recmodel_pop, train[1:5], type="ratings")
prediction_pop
class(prediction_pop)
as(prediction_pop, "matrix")[,1:5]




##################################FOllowing different data set
set.seed(1)
eval <- evaluationScheme(ratings_movies, method="split", train=0.8, given=2)
eval
# eval <- evaluationScheme(ratings_movies, method="split", train=0.8, given =1)
#5 ratings of 20% of users are excluded for testing
model_pop <- Recommender(getData(eval, "train"), "POPULAR", NULL)
prediction_pop2 <- predict(model_pop, getData(eval, "known"),10, type="ratings")
qplot(rowCounts(prediction_pop2)) + geom_histogram(binwidth = 10) +
  ggtitle("Distribution of movies per user by IBCF")
rmse_popular_pop2 <- calcPredictionAccuracy(prediction_pop2, getData(eval, "unknown"), TRUE)[1]
rmse_popular_pop2
########################################WORK ON IT
# qplot(rmse_popular_pop2[, "RMSE"]) + geom_histogram(binwidth = 0.1) +
# ggtitle("Distribution of the RMSE by user by Popular Method ")
# Estimating RMSE
set.seed(1)
model2 <- Recommender(getData(eval, "train"), method = "UBCF",  param=list(normalize = "center", method="Cosine", nn=50))
prediction_ubcf2 <- predict(model2, getData(eval, "known"), type="ratings")
qplot(rowCounts(prediction_ubcf2)) + geom_histogram(binwidth = 10) +
  ggtitle("Distribution of movies per user by IBCF")
rmse_ubcf2 <- calcPredictionAccuracy(prediction_ubcf2, getData(eval, "unknown"))[1]
rmse_ubcf2
# qplot(rmse_ubcf2[, "RMSE"]) + geom_histogram(binwidth = 0.1) +
# ggtitle("Distribution of the RMSE by user by UBCF method")
set.seed(1)
model1 <- Recommender(getData(eval, "train"), method = "IBCF",  param=list(normalize = "center", method="Cosine", nn=50))
prediction_ibcf <- predict(model1, getData(eval, "known"), type="ratings")
qplot(rowCounts(prediction_ibcf)) + geom_histogram(binwidth = 10) +
  ggtitle("Distribution of movies per user by IBCF")
rmse_ibcf <- calcPredictionAccuracy(prediction_ibcf, getData(eval, "unknown"))[1]
rmse_ibcf


