source("utils.R")
source("model_selection.R")
source("pdi_measures.R")
source("performance_calculation.R")
source("profile_dissimilarity_calculation.R")
source("plot.R")
source("rashomon_detect.R")

library(ranger)
library(randomForest)
library(gbm)

X <- matrix(rnorm(2000*10), ncol=10)
y <- as.numeric(rowSums(X) + X[,1]^2 - X[,2]^2 + rnorm(2000) > 0)

train_indices <- sample(1:length(y), 1600)

df_train <- data.frame(X[train_indices,], y=y[train_indices])
df_test <- data.frame(X[-train_indices,], y=y[-train_indices])


#--- Modeling ----

model1 = glm(y ~ ., data = df_train, family = binomial)
model2 = ranger(y ~ ., data = df_train, classification = TRUE, num.trees=100)
model3 = randomForest(factor(y) ~ ., data = df_train, ntree=100)
model4 = gbm(y ~ ., data = df_train, distribution = "bernoulli")

explainers <- make_explainer_list(model1, model2, model3, model4, 
                                  data = df_test[, 1:10], y = df_test[, 11])

res <- rashomon_detect(explainers, 
                       k=3, 
                       pdi_method = derivative_euclidean_distance, 
                       comparison = "last"
                      )
res

plot_summary_matrix(res)

