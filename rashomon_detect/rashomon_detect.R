rashomon_detect <- function(explainers_list, 
                            performance_measure = NULL,  
                            epsilon, 
                            k = 2, 
                            pdi_method = derivative_fraction_sign_difference, 
                            comparison="all",  
                            variables = NULL, N = NULL, variable_splits = NULL){
  if (is.null(performance_measure)){
    task_type <- explainers[[1]]$model_info$type
    performance_measure <- ifelse(task_type == "classification", "auc", "mse")
  }
  performance_vals <- get_performance_vals(explainers_list, performance_measure)
  best_performance_val <- get_best_performance_val(performance_vals, performance_measure)
  rashomon_models_ind <- which(abs(best_performance_val - performance_vals) < epsilon)
  rashomon_models_ind <- rashomon_models_ind[
    order(performance_vals[rashomon_models_ind], decreasing = TRUE)
  ]
  
  explainers_list <- explainers_list[rashomon_models_ind]
  performances <- performance_vals[rashomon_models_ind]
  
  profiles <- lapply(explainers_list, 
                     model_profile, 
                     variables = variables, 
                     N = N, 
                     variable_splits = variable_splits) 
  
  dist_matrix <- calculate_distances(profiles, pdi_method)
  selected_models_indices <- find_k_most_different_models(explainers_list, dist_matrix, k, comparison)
  
  res <- list()
  res$explainers <- explainers_list[selected_models_indices]
  res$profiles <- profiles[selected_models_indices]
  res$performances <- performances
  res$distance_matrix <- get_final_distance_matrix(dist_matrix, selected_models_indices)
  res
}
