# uses the data set recc_data_test from step 5. evaluate binary CF

###################### COMPARING MODELS #############################

# It will be compared IBCF and UBCF using Jaccard index, 
# and a random recommendations just to have a comparison base line
models_to_evaluate <- list(
  IBCF_jac = list(name = "IBCF", param = list(method = "jaccard")),
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  IBCF_euc = list(name = "IBCF", param = list(method = "euclidean")),
  
  UBCF_jac = list(name = "UBCF", param = list(method = "jaccard")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine"))
 #random = list(name = "RANDOM", param=NULL)
)

# models_to_evaluate
n_recommendations <- c(1, 5, seq(10, 30, 5))

# We are ready to run and evaluate the models. 
# The only difference from code 5.evaluate binary CF is now the input method is a list of models
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate,
                         n = n_recommendations)

class(list_results)
# [1] "evaluationResultList"
# attr(,"package")
# [1] "recommenderlab"

plot(list_results, legend = "topleft",ylim = c(0, 0.20)) 
title("ROC curve - 50 movies per user")


plot(list_results, "prec/rec", legend = "bottomleft")
title("Precision-recall binary models")


# precision just for the best models (user-based)
models_to_evaluate <- list(
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_jac = list(name = "UBCF", param = list(method = "jaccard")))


list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate,
                         n = n_recommendations)

plot(list_results, "prec/rec", legend="bottomleft")
title("Precision-recall rating - user-based models")

plot(list_results, legend="topleft")
title("ROC curve - rating user-based models")

#################################################################################################
###       COMPARE THE BEST COMBINATION FOR THE FILTERS: USERS/MOVIE AND RATINGS/MOVIE         ###
#################################################################################################

### Combinations tested:
#  1 movie/user - 50 ratings/movie
#  5 movies/user - 50 ratings/movie
# 10 movies/user - 50 ratings/movie
# 20 movies/user - 50 ratings/movie
# 50 movies/user - 50 ratings/movie
# 100 movies/user - 50 ratings/movie

# The best combination found: 10/movies/user

### Combinations tested:
# 10 movies/user - 20 ratings/movie
# 10 movies/user - 90 ratings/movie
# 10 movies/user - 120 ratings/movie
# 10 movies/user - 150 ratings/movie
# 10 movies/user - 200 ratings/movie
# 10 movies/user - 250 ratings/movie
# 10 movies/user - 300 ratings/movie
# 10 movies/user - 500 ratings/movie


# most common movie
t_m <- aggregate(cbind(count = rating) ~ movie_id, 
                 data = ratings, 
                 FUN = function(x){NROW(x)})
# merge
r <- merge(x=ratings, y=t_m ,by="movie_id", all.x=TRUE)

# select movies with more than 50 ratings
r <- r[r$count >= 500,]

# count movies per user
t_m_u <- aggregate(cbind(count_movie = movie_id) ~ user_id, 
                   data = r, 
                   FUN = function(x){NROW(x)})
# merge
r2 <- merge(x=r, y=t_m_u ,by="user_id", all.x=TRUE)

# select users with more than 10 movies (BEFORE 20)
r2 <- r2[r2$count_movie >= 90,]


# delete count and timestamp
r2 <- subset(r2, , -c(rating_timestamp,count,count_movie))

# convert it into a data table
r2 <- data.table(r2)


# delete rating
r_binary <- subset(r2, , -c(rating))

# reshape
r_binary[, value := 1]
r_wide <- reshape(data = r_binary,
                  direction = "wide",
                  idvar = "user_id",
                  timevar = "movie_id",
                  v.names = "value",
                  drop = NULL)

head(r_wide[, 1:5, with = FALSE])
# user_id value.816711 value.2726560 value.3079380 value.1091191
# 1:       2            1             1             1             1
# 2:      18           NA            NA            NA             1
# 3:      26            1            NA            NA            NA
# 4:      38           NA            NA            NA            NA
# 5:      48           NA            NA            NA            NA
# 6:      49           NA            NA             1            NA


# keep only the columns containing ratings
# the user name will be the matrix row names, so we need to store them in the vector_users vector
vector_users <- r_wide[, user_id]
r_wide <- r_wide[ ,user_id := NULL]


# have the column names equal to the item names
setnames(x = r_wide,
         old = names(r_wide),
         new = substring(names(r_wide), 7))

# store the rating matrix within a recommenderlab object: 
# 1) convert r_wide in a matrix 
# 2) set the row names equal to the user names
matrix_wide <- as.matrix(r_wide)
rownames(matrix_wide) <- vector_users
head(matrix_wide[, 1:6])

# replace NA for zero
matrix_wide[is.na(matrix_wide)] <- 0
head(matrix_wide[, 1:6])
# 816711 2726560 3079380 1091191 2381249 1398426
# 2       1       1       1       1       1       1
# 18      0       0       0       1       0       0
# 26      1       0       0       0       0       0
# 38      0       0       0       0       0       0
# 48      0       0       0       0       0       0
# 49      0       0       1       0       0       0


# coercing matrix_wide into a binary rating matrix 
ratings_matrix <- as(matrix_wide, "binaryRatingMatrix")
ratings_matrix
# 4938 x 1788 rating matrix of class ‘binaryRatingMatrix’ with 287862 ratings
# 10 movies per user - 300 ratings per movie
# 6088 x 339 rating matrix of class ‘binaryRatingMatrix’ with 180853 ratings.
# 10 movies per user - 500 ratings per movie
# 4859 x 177 rating matrix of class ‘binaryRatingMatrix’ with 124501 ratings.

########################  DIVIDE TRAIN AND TEST DATA SETS    ##################################

# split the data into the training and the test set
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.7, 0.3))
recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]


percentage_training <- 0.7

# minimum number of movies watched by any user
min(rowCounts(ratings_matrix))
# [1] 20

# keep 15 movies
items_to_keep <- 90

# the rating = 1 (watched the movie) will be considered good to evaluate the model
rating_threshold <- 1

# how many times we want to run the evaluation
n_eval <- 1

eval_sets <- evaluationScheme(data = ratings_matrix, 
                              method = "split",
                              train = percentage_training, 
                              given = items_to_keep, 
                              goodRating =rating_threshold, 
                              k = n_eval) 
eval_sets
# Evaluation scheme with 15 items given
# Method: ‘split’ with 1 run(s).
# Training set proportion: 0.700
# Good ratings: >=1.000000
# Data set: 4938 x 1788 rating matrix of class ‘realRatingMatrix’ with 287862 ratings.


# number of nearest neighbors of IBCF
number_neighbors <- 30

models_to_evaluate <- list(
  IBCF_jac = list(name = "IBCF", param = list(method = "jaccard")),
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  IBCF_euc = list(name = "IBCF", param = list(method = "euclidean")),
  
  UBCF_jac = list(name = "UBCF", param = list(method = "jaccard")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine"))
  #random = list(name = "RANDOM", param=NULL)
)

# models_to_evaluate
n_recommendations <- c(1, 5, seq(10, 30, 5))

# We are ready to run and evaluate the models. 
# The only difference from code 5.evaluate binary CF is now the input method is a list of models
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate,
                         n = n_recommendations)

class(list_results)
# [1] "evaluationResultList"
# attr(,"package")
# [1] "recommenderlab"

plot(list_results, legend = "topleft",ylim = c(0, 0.80)) 
title("ROC-binary -90 movies per user - 500 ratings per movie")


plot(list_results, "prec/rec", legend = "bottomleft")
title("Precision-recall - binary -90 movies per user - 500 ratings per movie")




