
# uses the data set recc_data_test from step 4.binary CF

####################   APPLYING THE RECOMMENDER MODEL ON TEST SET   #################

# n_recommended that specifies the number of items to recommend to each user
n_recommended <- 6

# the algorithm identifies the top n recommendations
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
recc_predicted

# The recc_predicted object contains the recommendations. Let's take a look at its structure:
class(recc_predicted)
slotNames(recc_predicted)

# these are the recommendations for the first user:
recc_predicted@items[[1]]
# [1] 454 512 554 949 946 428

# second user
recc_predicted@items[[2]]
# [1] 372 354 143 327  97 136

# extract the recommended movies from recc_predicted@item labels:
recc_user_1 <- recc_predicted@items[[1]]
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]
movies_user_1

list_m <- c(1431045,1596363,1663202,3498820,3460252,1386697)
list_m <- movies4[movies4$movie_id_n %in% list_m,]
list_m

# define a matrix with the recommendations for each user:
recc_matrix <- sapply(recc_predicted@items, function(x){colnames(ratings_matrix)[x]})
recc_users2 <- as.data.table(recc_matrix)
recc_users3 <- t(recc_users2)

# identify the most recommended movies
number_of_items <- factor(table(recc_users3))
chart_title <- "Distribution of the number of items for IBCF"
qplot(number_of_items) + ggtitle(chart_title)

# Most of the movies have been recommended only a few times, and a few movies
# have been recommended many times. Let's see which are the most popular movies
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 5)
table_top <- data.frame(names(number_of_items_top),
                        number_of_items_top)
table_top
# names.number_of_items_top. number_of_items_top
# 1454468                    1454468                 394
# 1877832                    1877832                 377
# 1535109                    1535109                 339
# 993846                      993846                 337
# 1431045                    1431045                 326


# search these items on movies data set
list_m <- c(1454468,1877832,1535109,993846,1431045)
list_m <- movies4[movies4$movie_id_n %in% list_m,]
list_m[,2]
# [1] "The Wolf of Wall Street (2013)"    "Deadpool (2016)"                  
# [3] "Gravity (2013)"                    "Captain Phillips (2013)"          
# [5] "X-Men: Days of Future Past (2014)"


#######################      EVALUATING       #################################

percentage_training <- 0.8

# minimum number of movies watched by any user
min(rowCounts(ratings_matrix))
# [1] 20

items_to_keep <- 15
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
# Training set proportion: 0.800
# Good ratings: >=1.000000
# Data set: 4938 x 1788 rating matrix of class ‘realRatingMatrix’ with 287862 ratings.

# build the model
number_neighbors <- 30
recc_model <- Recommender(data = getData(eval_sets, "train"),
                          method = "IBCF",
                          parameter = list(method = "Jaccard",
                                           k = number_neighbors))

# extract the rating-based distance matrix from the recc_model model:
dist_ratings <- as(recc_model@model$sim, "matrix")
vector_items <- rownames(dist_ratings)

recc_model@model$sim <- as(dist_ratings, "dgCMatrix")

# Predict the test set users with known purchases
items_to_recommend <- 10
eval_prediction <- predict(object = recc_model,
                           newdata = getData(eval_sets,"known"),
                           n = items_to_recommend,
                           type = "topNList")


# Evaluate the model performance
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        given = items_to_recommend,
                                        byUser =FALSE) 
eval_accuracy

# Results 
model_to_evaluate <- "IBCF"
results <- evaluate(x = eval_sets, method = model_to_evaluate, n =seq(10, 100, 10))
class(results)
# IBCF run fold/sample [model time/prediction time]

# take a look first element
head(getConfusionMatrix(results)[[1]])
# TP        FP       FN       TN precision     recall        TPR         FPR
# 10  2.851215  7.148785 43.42510 1719.575 0.2851215 0.09273463 0.09273463 0.004112782
# 20  5.066802 14.933198 41.20951 1711.790 0.2533401 0.15487968 0.15487968 0.008597142
# 30  6.953441 23.046559 39.32287 1703.677 0.2317814 0.20586848 0.20586848 0.013272561
# 40  8.553644 31.446356 37.72267 1695.277 0.2138411 0.24668758 0.24668758 0.018117313
# 50 10.040486 39.959514 36.23583 1686.764 0.2008097 0.28495002 0.28495002 0.023028613
# 60 11.395749 48.604251 34.88057 1678.119 0.1899291 0.31855632 0.31855632 0.028017429

# • True Positives (TP): These are recommended movies that have been watched
# • False Positives (FP): These are recommended movies that haven't been watched
# • False Negatives(FN): These are not recommended movies that have been watched
# • True Negatives (TN): These are not recommended movies that haven't been watched

# ROC curve
plot(results, annotate = TRUE, main = "ROC curve")

# • True Positive Rate (TPR): This is the percentage of watched movies that have been recommended. 
# It's the number of TP divided by the number of watched movies (TP + FN).

# • False Positive Rate (FPR): This is the percentage of NOT wacthed movies that have been recommended.
# It's the number of FP divided by the number of NOT watched movies (FP + TN).


# Precision and recall
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")

