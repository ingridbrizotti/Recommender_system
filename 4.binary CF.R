
# Item based collaborative filtering (IBCF) 
# the algorithm is based on items and the steps to identify recommendations were as follows:
# • Identify which movies are similar in terms of having been rated by the same people
# • Recommend to a new user the movies that are similar to its rated


### BINARY MODEL:
# - Define a matrix having 1 if the user rated the movie, and 0 otherwise. 
# - Since we are not taking account of the ratings, the result will be less accurate.

##############################################################################
# uses the data set (r2) from step 3.Data preparation

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


# visualize the matrix: As expected, the matrix is sparse
image(ratings_matrix[1:100, 1:100], main = "Binary rating matrix")


# number of users watching the same movie
n_users <- colCounts(ratings_matrix)
qplot(n_users) + stat_bin(binwidth = 5) + ggtitle("Distribution of the number of users")


# without outliers
qplot(n_users[n_users < 500]) + stat_bin(binwidth = 5) +
  ggtitle("Distribution of the number of users")


########################  DIVIDE TRAIN AND TEST DATA SETS    ##################################

# split the data into the training and the test set
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.7, 0.3))
recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]


########################  BUILD THE MODEL - ITEM-BASED    ##################################

# IBCF: item-based collaborative filtering
# Using Jaccard index (binary attribute)
recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))

# extract some details about the model
model_details <- getModel(recc_model)
model_details$description
# [1] "IBCF: Reduced similarity matrix"


class(model_details$sim)
dim(model_details$sim)
# The matrix belongs to the dgCMatrix class, and it is square

# build the heat map
n_items_top <- 20
image(model_details$sim[1:n_items_top, 1:n_items_top],
      main = "Heatmap of the first rows and columns")

# Check more details
model_details$k
row_sums <- rowSums(model_details$sim > 0)
table(row_sums)
# as expected, each row has 30 elements greater than 0.

# distribution chart
col_sums <- colSums(model_details$sim > 0)
qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of
                                                   the column count")

# As expected, there are a few movies that are similar to many others.
# Let's see which are the movies with the most elements:
which_max <- order(col_sums, decreasing = TRUE)[1:6]
rownames(model_details$sim)[which_max]
which_max

list_m <- c(1951266,2004420,103064,478970,105695,361748)
movies4[movies4$movie_id_n %in% list_m,]

# All the distances are between 0 and 0.70
range(recc_model@model$sim)
# [1] 0.000000 0.702381


