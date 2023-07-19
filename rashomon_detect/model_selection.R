select_most_different_models_all <- function(dist_matrix,
                                             k,
                                             best_performing_model_index) {
  selected_models_indices <- best_performing_model_index
  counter <- 1
  
  unique_models <-
    unique(c(dist_matrix$model_ind_1, dist_matrix$model_ind_2))
  
  while (counter < k) {
    next_model_index <-
      find_next_model_index(dist_matrix, unique_models, selected_models_indices)
    selected_models_indices <-
      c(selected_models_indices, next_model_index)
    counter <- counter + 1
  }
  selected_models_indices
}

find_next_model_index_all <-
  function(dist_matrix,
           unique_models,
           selected_models_indices,
           comparison) {
    available_models <- setdiff(unique_models, selected_models_indices)
    average_distances <-
      sapply(available_models, function(model_index) {
        avg_distance <-
          mean(dist_matrix$avg_pdi[(
            dist_matrix$model_ind_1 %in% selected_models_indices &
              dist_matrix$model_ind_2 == model_index
          ) |
            (
              dist_matrix$model_ind_2 %in% selected_models_indices &
                dist_matrix$model_ind_1 == model_index
            )])
        return(avg_distance)
      })
    available_models[which.max(average_distances)]
  }


select_most_different_models_last <- function(profiles_list,
                                              pdi_method,
                                              k,
                                              best_performing_model_index) {
  selected_models_indices <- best_performing_model_index
  counter <- 1
  
  unique_models <- 1:length(profiles_list)
  
  all_vnames <-
    unique(unlist(lapply(profiles_list, function(df)
      unique(df$agr_profiles$`_vname_`))))
  num_vars <- length(all_vnames)
  distances <- matrix(
    0,
    nrow = 0,
    ncol = 2 + num_vars,
    dimnames = list(NULL, c(
      "model_ind_1", "model_ind_2", all_vnames
    ))
  )
  
  while (counter < k) {
    new_dist <- calculate_distances_to_last_model(
      profiles_list,
      pdi_method,
      num_vars,
      all_vnames,
      unique_models,
      selected_models_indices
    )
    distances <- rbind(distances, new_dist)
    next_model_index <- find_next_model_index_last(new_dist)
    selected_models_indices <-
      c(selected_models_indices, next_model_index)
    counter <- counter + 1
  }
  
  list(distances = distances,
       selected_models_indices = selected_models_indices)
}


find_next_model_index_last <- function(new_dist) {
  new_dist$model_ind_2[which.max(new_dist$avg_pdi)]
}
