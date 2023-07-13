library(DALEX)

make_explainer_list <- function(model1, ..., data, y, verbose = FALSE){
  model_list <- list(model1, ...)
  lapply(model_list, explain, data = data, y = y, verbose = verbose)
}
