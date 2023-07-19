rashomon_detect <- function(explainers_list,
                            performance_measure = NULL,
                            k = 2,
                            pdi_method = derivative_fraction_sign_difference,
                            comparison = "all",
                            variables = NULL,
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
  
  profiles <- lapply(
    explainers_list,
    model_profile,
    variables = variables,
    N = N,
    variable_splits = variable_splits
  )
  
  res <- list()
  res$explainers <- explainers_list
  res$profiles <- profiles
  res$performances <- performance_vals
  res$rejected_due_to_performance_models_indices <- numeric(0)
  
  if (comparison == "all") {
    distances <- calculate_all_distances(profiles, pdi_method)
    selected_models_indices <-
      select_most_different_models_all(distances,
                                       k,
                                       best_performing_model_index)
  } else if (comparison == "last") {
    tmp <- select_most_different_models_last(profiles,
                                             pdi_method,
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
