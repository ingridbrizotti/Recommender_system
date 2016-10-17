

# uses the data set recc_data_test from step 5. evaluate binary CF

###################### COMPARING MODELS #############################

# It will be compared IBCF and UBCF using Jaccard index, 
# and a random recommendations just to have a comparison base line
models_to_evaluate <- list(
  IBCF_jac = list(name = "IBCF", param = list(method = "jaccard")),
  UBCF_jac = list(name = "UBCF", param = list(method = "jaccard")),
  random = list(name = "RANDOM", param=NULL)
)

# models_to_evaluate
n_recommendations <- c(1, 5, seq(10, 100, 10))

# We are ready to run and evaluate the models. 
# The only difference from code 5.evaluate binary CF is now the input method is a list of models
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate,
                         n = n_recommendations)
# IBCF run fold/sample [model time/prediction time]
# 1  [65.267sec/1.07sec] 
# UBCF run fold/sample [model time/prediction time]
# 1  [0.002sec/32.649sec] 
# RANDOM run fold/sample [model time/prediction time]
# 1  [0.002sec/3.461sec] 

class(list_results)
# [1] "evaluationResultList"
# attr(,"package")
# [1] "recommenderlab"

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")



