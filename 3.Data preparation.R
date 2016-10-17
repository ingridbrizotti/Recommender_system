
library(data.table)
library(ggplot2)
library(recommenderlab)
library(countrycode)

load("~/Desktop/Ryerson/3.Data_Analytics_Capstone/MovieTweetings/latest/ratings.Rda")
load("~/Desktop/Ryerson/3.Data_Analytics_Capstone/MovieTweetings/latest/movies4.Rda")

########################  PREPARE THE DATA    ##################################

# most common movie
t_m <- aggregate(cbind(count = rating) ~ movie_id, 
                 data = ratings, 
                 FUN = function(x){NROW(x)})
# merge
r <- merge(x=ratings, y=t_m ,by="movie_id", all.x=TRUE)

# select movies with more than 50 ratings
r <- r[r$count >= 50,]

# count movies per user
t_m_u <- aggregate(cbind(count_movie = movie_id) ~ user_id, 
                   data = r, 
                   FUN = function(x){NROW(x)})
# merge
r2 <- merge(x=r, y=t_m_u ,by="user_id", all.x=TRUE)

# select users with more than 20 movies
r2 <- r2[r2$count_movie >= 20,]


# delete count and timestamp
r2 <- subset(r2, , -c(rating_timestamp,count,count_movie))

# convert it into a data table
r2 <- data.table(r2)


