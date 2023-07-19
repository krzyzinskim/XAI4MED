calculate_single_distance <-
  function(profiles_i, profiles_j, vname, pdi_method) {
    profiles_i_var <-
      profiles_i[profiles_i$`_vname_` == vname, "_yhat_"]
    profiles_j_var <-
      profiles_j[profiles_j$`_vname_` == vname, "_yhat_"]
    profiles_var_vals <-
      profiles_i[profiles_i$`_vname_` == vname, "_x_"]
    stopifnot(all(profiles_var_vals == profiles_j[profiles_j$`_vname_` == vname, "_x_"]))
    
    
    if (length(profiles_i) == 0) {
      profiles_i <- rep(0, length(profiles_j))
    }
    
    if (length(profiles_j) == 0) {
      profiles_j <- rep(0, length(profiles_i))
    }
    
    distance <-
      pdi_method(profiles_i_var, profiles_j_var, profiles_var_vals)
    distance
  }

calculate_all_distances <-
  function(profiles_list, pdi_method = derivative_fraction_sign_difference) {
    num_models <- length(profiles_list)
    all_vnames <-
      unique(unlist(lapply(profiles_list, function(df)
        unique(df$agr_profiles$`_vname_`))))
    num_vars <- length(all_vnames)
    
    # Create an empty matrix to store the distances
    dist_matrix <-
      matrix(
        0,
        nrow = num_models * (num_models - 1) / 2,
        ncol = 2 + num_vars,
        dimnames = list(NULL, c(
          "model_ind_1", "model_ind_2", all_vnames
        ))
      )
    
    # Initialize row counter
    row_counter <- 1
    
    # Iterate over each pair of models
    for (i in 1:(num_models - 1)) {
      for (j in (i + 1):num_models) {
        profiles_i <- profiles_list[[i]]$agr_profiles
        profiles_j <- profiles_list[[j]]$agr_profiles
        
        for (k in 1:num_vars) {
          vname <- all_vnames[k]
          distance <-
            calculate_single_distance(profiles_i, profiles_j, vname, pdi_method)
          
          dist_matrix[row_counter, "model_ind_1"] <- i
          dist_matrix[row_counter, "model_ind_2"] <- j
          dist_matrix[row_counter, vname] <- distance
        }
        row_counter <- row_counter + 1
      }
    }
    
    dist_matrix <- as.data.frame(dist_matrix)
    dist_matrix$avg_pdi <- rowMeans(dist_matrix[, -c(1:2)])
    dist_matrix
  }

calculate_distances_to_last_model <- function(profiles_list,
                                              pdi_method,
                                              num_vars,
                                              all_vnames,
                                              unique_models,
                                              selected_models_indices) {
  last_model_index <-
    selected_models_indices[length(selected_models_indices)]
  available_models <-
    setdiff(unique_models, selected_models_indices)
  
  dist_matrix <-
    matrix(
      0,
      nrow = length(available_models),
      ncol = 2 + num_vars,
      dimnames = list(NULL, c(
        "model_ind_1", "model_ind_2", all_vnames
      ))
    )
  
  for (j in 1:length(available_models)) {
    model_to_test <- available_models[j]
    profiles_i <- profiles_list[[last_model_index]]$agr_profiles
    profiles_j <- profiles_list[[model_to_test]]$agr_profiles
    
    for (k in 1:num_vars) {
      vname <- all_vnames[k]
      distance <-
        calculate_single_distance(profiles_i, profiles_j, vname, pdi_method)
      
      dist_matrix[j, "model_ind_1"] <- last_model_index
      dist_matrix[j, "model_ind_2"] <- model_to_test
      dist_matrix[j, vname] <- distance
    }
  }
  dist_matrix <- as.data.frame(dist_matrix)
  dist_matrix$avg_pdi <- rowMeans(dist_matrix[, -c(1:2)])
  dist_matrix
}

get_final_distance_matrix <-
  function(dist_matrix, selected_models_indices) {
    num_selected_models <- length(selected_models_indices)
    dist_matrix_selected <-
      matrix(
        0,
        nrow = num_selected_models,
        ncol = num_selected_models,
        dimnames = list(selected_models_indices, selected_models_indices)
      )
    
    # Iterate over the upper triangle of the matrix
    for (i in 1:(num_selected_models - 1)) {
      for (j in (i + 1):num_selected_models) {
        model_ind_1 <- selected_models_indices[i]
        model_ind_2 <- selected_models_indices[j]
        
        avg_distance <-
          dist_matrix$avg_pdi[(
            dist_matrix$model_ind_1 == model_ind_1 &
              dist_matrix$model_ind_2 == model_ind_2
          ) |
            (
              dist_matrix$model_ind_1 == model_ind_2 &
                dist_matrix$model_ind_2 == model_ind_1
            )]
        
        dist_matrix_selected[i, j] <- avg_distance
        dist_matrix_selected[j, i] <- avg_distance
      }
    }
    dist_matrix_selected
  } 