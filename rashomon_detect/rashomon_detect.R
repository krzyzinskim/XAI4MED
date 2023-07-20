rashomon_detect <- function(explainers_list,
                            performance_measure = NULL,
                            k = 2,
                            pdi_method_numerical = derivative_fraction_sign_difference,
                            pdi_method_categorical = categorical_distance,
                            comparison = "last",
                            include_categorical_variables = TRUE,
                            N = NULL,
                            variable_splits = NULL) {
  if (is.null(performance_measure)) {
    task_type <- explainers[[1]]$model_info$type
    performance_measure <-
      ifelse(task_type == "classification", "auc", "mse")
  }
  
  stopifnot(length(explainers_list) > k)
  
  performance_vals <-
    get_performance_vals(explainers_list, performance_measure)
  best_performance_val <-
    get_best_performance_val(performance_vals, performance_measure)
  best_performing_model_index <-
    get_best_performing_model_index(performance_vals, best_performance_val)
  
  res <- list()
  res$explainers <- explainers_list
  res$performances <- performance_vals
  
  profiles_numerical <- lapply(
    explainers_list,
    model_profile,
    N = N,
    variable_splits = variable_splits
  )
  res$profiles_numerical <- profiles_numerical
  
  profiles_categorical <- NULL
  if (include_categorical_variables & !all(sapply(explainers_list[[1]]$data, is.numeric))){
    profiles_categorical <- lapply(
      explainers_list,
      model_profile,
      N = N,
      variable_type = "categorical"
    )
    res$profiles_categorical <- profiles_categorical
  }

  if (comparison == "all") {
    distances <- calculate_all_distances(profiles_numerical, 
                                         profiles_categorical, 
                                         pdi_method_numerical, 
                                         pdi_method_categorical)
    selected_models_indices <-
      select_most_different_models_all(distances,
                                       k,
                                       best_performing_model_index)
  } else if (comparison == "last") {
    tmp <- select_most_different_models_last(profiles_numerical,
                                             profiles_categorical,
                                             pdi_method_numerical,
                                             pdi_method_categorical,
                                             k,
                                             best_performing_model_index)
    distances <- tmp$distances
    selected_models_indices <- tmp$selected_models_indices
  } else {
    stop("Only comparison = 'last' and comparison = 'all' are implemented.")
  }
  
  res$distances <- distances
  res$selected_models_indices <- selected_models_indices
  res$selected_models_distance_matrix <-
    get_final_distance_matrix(distances, selected_models_indices)
  res
}
