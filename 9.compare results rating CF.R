###################### COMPARING MODELS #############################

models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  IBCF_jac = list(name = "IBCF", param = list(method = "jaccard")),
  IBCF_euc = list(name = "IBCF", param = list(method = "euclidean")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  UBCF_jac = list(name = "UBCF", param = list(method = "jaccard")),
  UBCF_euc = list(name = "UBCF", param = list(method = "euclidean")),
  random = list(name = "RANDOM", param=NULL)
)

# recommend up to 30 movies to each user.

n_recommendations <- c(1, 5, seq(10, 30, 5))

# We are ready to run and evaluate the models. Like in the previous chapter, the
# function is evaluate. The only difference is that now the input method is a list of models:
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate,
                         n = n_recommendations)
class(list_results)
# IBCF run fold/sample [model time/prediction time]
# 1  [130.559sec/1.721sec] 
# IBCF run fold/sample [model time/prediction time]
# 1  [71.293sec/0.879sec] 
# UBCF run fold/sample [model time/prediction time]
# 1  
# UBCF run fold/sample [model time/prediction time]
# 1  [0.047sec/28.99sec] 
# RANDOM run fold/sample [model time/prediction time]
# 1  


# take a look in the first element
class(list_results[[1]])

sapply(list_results, class) == "evaluationResults"

# average confusion matrix
avg_matrices <- lapply(list_results, avg)


# IBCF cosine distance
head(avg_matrices$IBCF_cos[, 1:8])
# TP        FP       FN       TN  precision      recall         TPR         FPR
# 1  0.07489879  0.917004 32.10121 1739.907 0.07551020 0.002916196 0.002916196 0.000526282
# 5  0.36842105  4.591093 31.80769 1736.233 0.07428571 0.012442795 0.012442795 0.002633885
# 10 0.74797571  9.171053 31.42814 1731.653 0.07540816 0.025851038 0.025851038 0.005261398
# 20 1.40587045 18.432186 30.77024 1722.392 0.07086735 0.047464625 0.047464625 0.010574907
# 30 2.08299595 27.674089 30.09312 1713.150 0.07000000 0.071254373 0.071254373 0.015877475

# IBCF Pearson correlation
head(avg_matrices$IBCF_cor[, 1:8])

# IBCF jaccard
head(avg_matrices$IBCF_jac[, 1:8])

# IBCF Eucliden
head(avg_matrices$IBCF_euc[, 1:8])

# UBCF cosine distance
head(avg_matrices$UBCF_cos[, 1:8])

# UBCF pearson correlation
head(avg_matrices$UBCF_cor[, 1:8])

# UBCF jaccard
head(avg_matrices$UBCF_jac[, 1:8])

# UBCF euclidean
head(avg_matrices$UBCF_euc[, 1:8])


plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")


#################################################################################################
###       COMPARE THE BEST COMBINATION FOR THE FILTERS: USERS/MOVIE AND RATINGS/MOVIE         ###
#################################################################################################


### Combinations tested:
#  1 movie/user - 50 ratings/movie
# 10 movies/user - 50 ratings/movie
# 50 movies/user - 50 ratings/movie
# 70 movies/user - 50 ratings/movie
# 90 movies/user - 50 ratings/movie
# 120 movies/user - 50 ratings/movie

# The best combination found: 90/movies/user

### Combinations tested:
# 90 movies/user - 20 ratings/movie
# 90 movies/user - 120 ratings/movie
# 90 movies/user - 150 ratings/movie
# 90 movies/user - 200 ratings/movie
# 90 movies/user - 300 ratings/movie
# 90 movies/user - 400 ratings/movie
# 90 movies/user - 500 ratings/movie


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

# reshape
r_wide <- reshape(data = r2,
                  direction = "wide",
                  idvar = "user_id",
                  timevar = "movie_id",
                  drop = NULL)

head(r_wide[, 1:5, with = FALSE])


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


# coercing matrix_wide into a binary rating matrix 
ratings_matrix <- as(matrix_wide, "realRatingMatrix")
ratings_matrix
# 1 - 50: 39787 x 1788 rating matrix of class ‘realRatingMatrix’ with 407883 ratings.
# 




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
rating_threshold <- 8
n_eval <- 1

eval_sets <- evaluationScheme(data = ratings_matrix, 
                              method = "split",
                              train = percentage_training, 
                              given = items_to_keep, 
                              goodRating =rating_threshold, 
                              k = n_eval) 
eval_sets
# Evaluation scheme with 1 items given
# Method: ‘split’ with 1 run(s).
# Training set proportion: 0.700
# Good ratings: >=7.000000
# Data set: 39787 x 1788 rating matrix of class ‘realRatingMatrix’ with 407883 ratings.

# number of nearest neighbors of IBCF
number_neighbors <- 30

models_to_evaluate <- list(
  IBCF_jac = list(name = "IBCF", param = list(method = "jaccard")),
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  IBCF_euc = list(name = "IBCF", param = list(method = "euclidean")),
  
  UBCF_jac = list(name = "UBCF", param = list(method = "jaccard")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  UBCF_euc = list(name = "UBCF", param = list(method = "euclidean"))
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
title("ROC- rating - 90 movies per user - 500 ratings per movie")


plot(list_results, "prec/rec", legend = "bottomleft")
title("Precision-recall - rating - 90 movies per user - 500 ratings per movie")



