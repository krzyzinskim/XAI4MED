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

model1 <- ranger(survived ~ gender + age + class + embarked +
                             fare + sibsp + parch,  data = titanic_imputed,
                           classification = TRUE)
model2 <- glm(survived ~ gender + age + class + embarked +
                fare + sibsp + parch,  data = titanic_imputed, family = binomial)
model3 <- randomForest(factor(survived) ~ gender + age + class + embarked +
                         fare + sibsp + parch,  data = titanic_imputed, ntree=100)
model4 <- gbm(survived ~ gender + age + class + embarked + fare + sibsp + parch,
              data = titanic_imputed, distribution = "bernoulli")

explainers <- make_explainer_list(model1, model2, model3, model4, 
                                  data = titanic_imputed[, -8], y = titanic_imputed$survived)

res <- rashomon_detect(explainers, 
                       k=3, 
                       pdi_method_numerical = derivative_euclidean_distance, 
                       comparison = "last"
                      )
res

plot_summary_matrix(res)

