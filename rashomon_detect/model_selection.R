find_k_most_different_models <- function(explainers_list,
                                         dist_matrix,
                                         k = k, 
                                         comparison = comparison){
  
  selected_models_indices <- 1
  counter <- 1
  
  unique_models <- unique(c(dist_matrix$model_ind_1, dist_matrix$model_ind_2))
  
  if (length(unique_models) <= k){
    selected_models_indices <- 1:length(unique_models)
  } else {
    while (counter < k){
      next_model_index <- find_next_model_index(dist_matrix, unique_models, selected_models_indices, comparison)
      selected_models_indices <- c(selected_models_indices, next_model_index)
      counter <- counter + 1
    }
  }
  selected_models_indices
}

find_next_model_index <- function(dist_matrix, unique_models, selected_models_indices, comparison){
  available_models <- setdiff(unique_models, selected_models_indices)
  if (comparison == "all"){
    average_distances <- sapply(available_models, function(model_index) {
      avg_distance <- mean(dist_matrix$avg_pdi[(dist_matrix$model_ind_1 %in% selected_models_indices &
                                                  dist_matrix$model_ind_2 == model_index) |
                                                 (dist_matrix$model_ind_2 %in% selected_models_indices &
                                                    dist_matrix$model_ind_1 == model_index)])
      return(avg_distance)
    })
    return(available_models[which.max(average_distances)])
  } else if (comparison == "last"){
    last_model_index <- selected_models_indices[length(selected_models_indices)]
    distances_to_last <- sapply(available_models, function(model_index) {
      distance <- dist_matrix$avg_pdi[(dist_matrix$model_ind_1 == last_model_index &
                                         dist_matrix$model_ind_2 == model_index) |
                                        (dist_matrix$model_ind_1 == model_index &
                                           dist_matrix$model_ind_2 == last_model_index)]
      return(distance)
    })
    return(available_models[which.max(distances_to_last)])
  }
  
}
