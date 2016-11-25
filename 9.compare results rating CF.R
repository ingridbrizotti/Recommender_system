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
# 40 2.77125506 36.904858 29.40486 1703.919 0.06984694 0.095020044 0.095020044 0.021173994

# IBCF Pearson correlation
head(avg_matrices$IBCF_cor[, 1:8])
# TP         FP       FN       TN  precision      recall         TPR          FPR
# 1  0.03846154  0.9412955 32.13765 1739.883 0.03925620 0.001455301 0.001455301 0.0005406165
# 5  0.18724696  4.3572874 31.98887 1736.467 0.03868802 0.005865463 0.005865463 0.0025026871
# 10 0.29858300  8.1670040 31.87753 1732.657 0.03200553 0.009373788 0.009373788 0.0046941387
# 20 0.49190283 15.0394737 31.68421 1725.784 0.02854275 0.014504238 0.014504238 0.0086497682
# 30 0.62955466 21.2722672 31.54656 1719.552 0.02606220 0.018053169 0.018053169 0.0122395665
# 40 0.72469636 27.1133603 31.45142 1713.711 0.02402387 0.020650880 0.020650880 0.0156048547


# UBCF cosine distance
head(avg_matrices$UBCF_cos[, 1:8])
# TP        FP       FN       TN precision     recall        TPR          FPR
# 1  0.264170  0.735830 31.91194 1740.088 0.2641700 0.01123808 0.01123808 0.0004210936
# 5  1.118421  3.881579 31.05769 1736.942 0.2236842 0.04784207 0.04784207 0.0022216885
# 10 1.953441  8.046559 30.22267 1732.777 0.1953441 0.08030221 0.08030221 0.0046077085
# 20 3.182186 16.817814 28.99393 1724.006 0.1591093 0.12735813 0.12735813 0.0096363388
# 30 4.118421 25.881579 28.05769 1714.942 0.1372807 0.15935620 0.15935620 0.0148351196
# 40 4.870445 35.129555 27.30567 1705.694 0.1217611 0.18326466 0.18326466 0.0201406761

head(avg_matrices$UBCF_cor[, 1:8])
# TP         FP       FN       TN precision      recall         TPR          FPR
# 1  0.2287449  0.7631579 31.94737 1740.061 0.2306122 0.008942471 0.008942471 0.0004367162
# 5  0.9726721  3.9868421 31.20344 1736.837 0.1961224 0.036337424 0.036337424 0.0022818095
# 10 1.7216599  8.1973684 30.45445 1732.627 0.1735714 0.063309174 0.063309174 0.0046934275
# 20 2.9372470 16.9008097 29.23887 1723.923 0.1480612 0.104751205 0.104751205 0.0096799495
# 30 3.8542510 25.9028340 28.32186 1714.921 0.1295238 0.134728008 0.134728008 0.0148418405
# 40 4.5688259 35.1072874 27.60729 1705.717 0.1151531 0.160205819 0.160205819 0.0201222853


plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")




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

