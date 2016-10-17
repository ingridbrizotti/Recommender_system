
# uses the data set recc_data_test from step 7.rating CF

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
# [1] 182 186 546 606 652 661

# extract the recommended movies from recc_predicted@item labels:
recc_user_1 <- recc_predicted@items[[1]]
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]
movies_user_1
# ",405159" ".60196"  ".327056" ".95327"  ".371746" ".167404"

list_m <- c(405159,60196,327056,95327,371746,167404)
list_m <- movies4[movies4$movie_id_n %in% list_m,]
list_m[,2]
# [1] "Il buono, il brutto, il cattivo. (1966)" "Hotaru no haka (1988)"                  
# [3] "The Sixth Sense (1999)"                  "Mystic River (2003)"                    
# [5] "Iron Man (2008)"                         "Million Dollar Baby (2004)" 

# define a matrix with the recommendations for each user:
recc_matrix <- sapply(recc_predicted@items, function(x){colnames(ratings_matrix)[x]})
recc_users2 <- as.data.table(recc_matrix)
recc_users3 <- t(recc_users2)

# identify the most recommended movies
number_of_items <- factor(table(recc_users3))
chart_title <- "Distribution of the number of items for IBCF"
qplot(number_of_items) + ggtitle(chart_title)

# check the most popular movies
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 5)
table_top <- data.frame(names(number_of_items_top),
                        number_of_items_top)
table_top
# names.number_of_items_top. number_of_items_top
# .1375666                   .1375666                 142
# .454876                     .454876                 117
# .1675434                   .1675434                 116
# .111161                     .111161                 106
# .338013                     .338013                 106


list_m <- c(13756666,454876,1675434,111161,338013)
list_m <- movies4[movies4$movie_id_n %in% list_m,]
list_m[,2]
# [1] "The Shawshank Redemption (1994)"              "Eternal Sunshine of the Spotless Mind (2004)"
# [3] "Life of Pi (2012)"                            "Intouchables (2011)"


#######################      EVALUATING       #################################

####   EVALUATING THE RATING:
# In order to recommend items to new users, collaborative filtering estimates the
# ratings of items that are not yet purchased. Then, it recommends the top-rated
# items.
# We can evaluate the model by comparing the estimated ratings with the real ones.

percentage_training <- 0.8

# minimum number of movies watched by any user
min(rowCounts(ratings_matrix))
# [1] 20

items_to_keep <- 15
rating_threshold <- 7 # consider a good rating (subjective)

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
# Good ratings: >=7.000000
# Data set: 4938 x 1788 rating matrix of class ‘realRatingMatrix’ with 287862 ratings.

# • train: This is the training set
# • known: This is the test set, with the item used to build the recommendations
# • unknown: This is the test set, with the item used to test the recommendations

getData(eval_sets, "train")
# 3950 x 1788 rating matrix of class ‘realRatingMatrix’ with 230770 ratings.

nrow(getData(eval_sets, "train")) / nrow(ratings_matrix)
# 0.79
# As expected, about 80 percent of the users are in the training set

# take a look
getData(eval_sets, "known")
getData(eval_sets, "unknown")

nrow(getData(eval_sets, "known")) / nrow(ratings_matrix)

# parameters to evaluate
model_to_evaluate <- "IBCF"
model_parameters <- NULL

# build the model
eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, 
                                parameter = model_parameters)

# The IBCF can recommend new items and predict their ratings. In order to build the
# model, we need to specify how many items we want to recommend, for example, 10,
# even if we don't need to use this parameter in the evaluation:

items_to_recommend <- 10

eval_prediction <- predict(object = eval_recommender, 
                           newdata = getData(eval_sets, "known"), 
                           n = items_to_recommend, 
                           type = "ratings")
class(eval_prediction)


# visualize the distribution of the number of movies per user
qplot(rowCounts(eval_prediction)) + geom_histogram(binwidth = 10) +
  ggtitle("Distribution of movies per user")
# The number of movies per user is roughly between 200 and 450.

# measure accuracy
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser =TRUE)
head(eval_accuracy)
# RMSE       MSE      MAE
# 26  2.794674  7.810201 1.895684
# 48  1.914554  3.665516 1.587321
# 139 1.812108  3.283736 1.506497
# 167 2.294700  5.265649 1.863722
# 321 1.316195  1.732369 1.150538
# 353 3.559026 12.666667 3.333333


# take a look at the RMSE by a user
qplot(eval_accuracy[, "RMSE"]) + geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")
# Most of the RMSEs are in the range of 0 to 2. 

# We evaluated the model for each user. In order to have a performance 
# index of the whole model, we need to compute the average indices, specifying byUser = FALSE:
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser =FALSE) 
eval_accuracy
#     RMSE      MSE      MAE 
# 1.671750 2.794747 1.191004
# Use these measures to compare the performance of different models on the same data.

######################################################################

####   EVALUATING THE RECOMMENDATIONS:
# Another way to measure accuracies is by comparing the recommendations with
# the purchases having a positive rating

# Positive rating is considerate >= 7

# Results 
results <- evaluate(x = eval_sets, method = model_to_evaluate, n =seq(10, 100, 10))
class(results)

head(getConfusionMatrix(results)[[1]])
#          TP        FP       FN       TN  precision     recall        TPR         FPR
# 10 0.742915  9.226721 29.95445 1733.076 0.07451777 0.02578868 0.02578868 0.005289131
# 20 1.408907 18.530364 29.28846 1723.772 0.07065990 0.05043787 0.05043787 0.010624314
# 30 2.119433 27.789474 28.57794 1714.513 0.07086294 0.07558289 0.07558289 0.015933161
# 40 2.762146 37.116397 27.93522 1705.186 0.06926396 0.09901428 0.09901428 0.021282077
# 50 3.413968 46.434211 27.28340 1695.868 0.06848731 0.12272646 0.12272646 0.026625209
# 60 4.057692 55.760121 26.63968 1686.543 0.06783418 0.14828890 0.14828890 0.031973936


# • True Positives (TP): These are recommended movies that have been watched
# • False Positives (FP): These are recommended movies that haven't been watched
# • False Negatives(FN): These are not recommended movies that have been watched
# • True Negatives (TN): These are not recommended movies that haven't been watched

# let's build the ROC curve. It displays these factors:

# • True Positive Rate (TPR): This is the percentage of watched movies that have been recommended. 
# It's the number of TP divided by the number of watched movies (TP + FN).

# • False Positive Rate (FPR): This is the percentage of NOT wacthed movies that have been recommended.
# It's the number of FP divided by the number of NOT watched movies (FP + TN).

plot(results, annotate = TRUE, main = "ROC curve")


# Two accuracy metrics:

#   • Precision: This is the percentage of recommended movies that have been
# watched. It's the number of TP divided by the total number of positives (TP + FP).

# • Recall (same as True Positive Rate): This is the percentage of purchased movies that have been
# recommended. It's the number of TP divided by the total number of
# movies watched (TP + FN).

plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")
