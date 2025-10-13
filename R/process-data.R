##########################################################################
##########################################################################
## Main: support functions to made the models 
##  AND analisys of distribution 
##
## date: 23/05/2023 #nolint
##
## Autor: Fabian Francisco Coloma Velez
##########################################################################
#########################################################################

########################################################################
########################################################################
#   MULTIPLE FOLDER CREATION
########################################################################
########################################################################

#' Match and reorder variable names to desired labels
#'
#' This function takes a vector of variable names (e.g., internal codes) and returns
#' their corresponding labels from a reference table loaded via `load_variables_names_to_match()`.
#' If any name is not found in the matching table, it returns a string with the prefix `"NO_FIND_"`.
#'
#' @param vector_name A character vector of variable names to be matched (e.g., internal names).
#' @param what_name A character string specifying which column to return from the lookup table.
#'        Default is `"Short label"`. Other options depend on the structure of `load_variables_names_to_match()`.
#'
#' @return A character vector with the corresponding labels (in the same order as `vector_name`).
#'         Names not found are returned as `"NO_FIND_<original_name>"`.
#'
#' @examples
#' \dontrun{
#'   put_in_order_names_vect(c("var1", "var2", "var3"))
#'   put_in_order_names_vect(c("varA", "varB"), what_name = "Long label")
#' }
#'
#' @export
put_in_order_names_vect <- function(vector_name, key_col = "pca_names", what_name = "Short label") {
  data_match <- load_variables_names_to_match()
#   key_col <- "pca_names"

  match_index <- match(vector_name, data_match[[key_col]])
  result <- data_match[[what_name]][match_index]

  # Replace NAs with "NO_FIND_<original_name>"
  result[is.na(result)] <- paste0("NO_FIND_", vector_name[is.na(result)])

  return(result)
}


#' Create multiple directories
#'
#' This utility function takes a vector of directory paths and creates each directory.
#' It does not show warnings if a directory already exists, and it will create
#' any necessary parent directories as well.
#'
#' @title Create multiple folders from a list of paths
#'
#' @param list_path A character vector of directory paths to create.
#'
#' @return No return value. This function is used for its side effect: creating folders.
#'
#' @author Fabian Coloma
#'
#' @export
#'
#' @examples
#' \dontrun{
#' multiple_folder_creation(c("outputs/figures", "outputs/tables", "logs"))
#' }
multiple_folder_creation <- function(list_path) {
  for (i in list_path) {
    dir.create(i, 
               showWarnings = FALSE, 
               recursive = TRUE)
  }
}


########################################################################
########################################################################
#   SPIDER PLOT
########################################################################
########################################################################

#' Generate and save radar plots for each environment and cluster
#'
#' This function creates radar (spider) plots to visualize the average values
#' of selected environmental variables for each cluster within a dataset.
#' It groups variables by predefined environmental categories and saves the plots
#' as PNG files in structured directories.
#'
#' @title Plot radar charts by environment and cluster
#'
#' @param data A data frame containing clustering results and environmental variables.
#' @param names_cat Character vector with names of categorical variables (not used directly in this function but typically part of `data`).
#' @param names_num Character vector with names of numerical variables (not used directly in this function but typically part of `data`).
#' @param environ_list A named list where each element is a character vector containing variable name patterns corresponding to an environment (e.g., `"climate"`, `"pollution"`).
#' @param where_to_save Directory path where the radar plots will be saved.
#' @param prefix Character prefix added to the folder name (e.g., `"RADAR_<prefix>"`).
#'
#' @return A character vector of the full paths to the saved radar plot PNG files.
#'
#' @author Fabian Coloma
#'
#' @examples
#' \dontrun{
#' environ_list <- list(
#'   climate = c("temp", "humidity"),
#'   pollution = c("pm25", "no2")
#' )
#' plot_radar_by_env_and_cluster(
#'   data = clustered_data,
#'   names_cat = names_cat,
#'   names_num = names_num,
#'   environ_list = environ_list,
#'   where_to_save = "outputs/plots",
#'   prefix = "ENV"
#' )
#' }
#'
#' @export
#' Plot radar charts by environment and cluster

#'
#' @description
#' Creates and saves radar plots that summarize environmental features by cluster.
#' Variables are grouped by environmental category, and radar plots are saved in structured folders.
#'
#' @param data Data frame containing the variables and cluster labels.
#' @param names_cat Character vector of categorical variable names (optional).
#' @param names_num Character vector of numeric variable names (optional).
#' @param environ_list Named list where each element contains variable name patterns per environment.
#' @param where_to_save Path to the directory where plots will be saved.
#' @param prefix Prefix for naming folders under the save directory.
#'
#' @return A character vector with the full paths to the saved radar plot PNG files.
#' @export
#'
#' @author Fabian Coloma
#'
plot_radar_by_env_and_cluster <- function(
  data,
  names_cat,
  names_num,
  environ_list,
  where_to_save,
  prefix
) {
  require(fmsb)
  require(dplyr)
  require(stringr)
  require(grDevices)
  require(viridis)  # para paletas de respaldo

    # Paleta Okabe-Ito segura para daltonismo
  okabe_ito <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999",
    "#000000", "#FFFFFF"
  )

  # Prepare environment folder names
  env_names <- names(environ_list)
  radar_folder_name <- paste0("RADAR_", prefix)
  paths_by_env <- file.path(where_to_save, radar_folder_name, env_names)

  # Create output directories
  multiple_folder_creation(paths_by_env)

  # Initialize list to store output file paths
  saved_plot_paths <- c()



  # Loop over each cluster
  for (cluster_id in unique(data$Cluster)) {
    
    # Loop over each environment group
    for (env in env_names) {

      # Directory for this radar plot
      env_path <- paths_by_env[str_detect(paths_by_env, env)]

      # Variables matching the current environment
      vars_to_plot <- names(data)[str_detect(names(data), paste(environ_list[[env]], collapse = "|"))]

        # Subset data for the current cluster and selected vars
        cluster_data_toget_maxmin <- data %>%
                                    select(all_of(vars_to_plot))

      # Subset data for the current cluster and selected vars
      cluster_data <- data %>%
        filter(Cluster == cluster_id) %>%
        select(all_of(vars_to_plot))

      # Rename columns using short labels
      cleaned_names <- put_in_order_names_vect(vector_name = names(cluster_data), what_name = "Short label")
      names(cluster_data) <- cleaned_names

      # Calculate summary statistics
      max_values <- apply(cluster_data_toget_maxmin, 2, max, na.rm = TRUE)
      min_values <- apply(cluster_data_toget_maxmin, 2, min, na.rm = TRUE)
      mean_values <- apply(cluster_data, 2, mean, na.rm = TRUE)
      # Poner a 0 donde haya NA o NaN
      mean_values[is.na(mean_values)] <- min_values[is.na(mean_values)]

      radar_df <- as.data.frame(rbind(max_values, min_values, mean_values))
      rownames(radar_df) <- c("Max", "Min", "Cluster")

      # Construct output file path
      plot_filename <- file.path(env_path, paste0("radar_", env, "_cluster_", cluster_id, ".png"))
      saved_plot_paths <- c(saved_plot_paths, plot_filename)

      # Plot and save radar chart
      png(plot_filename, width = 900, height = 800)
      radarchart(
        radar_df,
        axistype = 4,
        pcol = "blue",
        pfcol = rgb(0, 0, 1, 0.3),
        plwd = 2,
        plty = 1,
        cglcol = "gray",
        cglty = 1,
        cglwd = 0.8,
        axislabcol = "black",
        vlcex = 0.8
      )
      dev.off()
    }
  }

  return(saved_plot_paths)
}

plot_radar_by_env_and_cluster_v2 <- function(
  data,
  names_cat = NULL,
  names_num = NULL,
  environ_list,
  where_to_save,
  prefix
) {
  require(fmsb)
  require(dplyr)
  require(stringr)
  require(grDevices)  # para paletas de colores
  require(viridis)      # paletas amigables para daltónicos

  env_names <- names(environ_list)
  radar_folder_name <- paste0("RADAR_", prefix)
  paths_by_env <- file.path(where_to_save, radar_folder_name, env_names)
  multiple_folder_creation(paths_by_env)
  saved_plot_paths <- c()

  # Número de clusters únicos
  cluster_ids <- sort(unique(data$Cluster))
  n_clusters <- length(cluster_ids)

  # Generar colores
#   cols_line <- rainbow(n_clusters)
#   cols_fill <- sapply(cols_line, function(col) adjustcolor(col, alpha.f = 0.3))

  # Generar paleta colorblind-friendly
    # Paleta Okabe-Ito segura para daltonismo
  okabe_ito <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999",
    "#000000", "#FFFFFF"
  )


  # Selección de paleta: Okabe-Ito o viridis si hay más clusters
  if (n_clusters <= length(okabe_ito)) {
    cols_line <- okabe_ito[1:n_clusters]
  } else {
    cols_line <- viridis::viridis(n_clusters, option = "D")
  }
  # Transparencia para relleno
  cols_fill <- sapply(cols_line, function(col) adjustcolor(col, alpha.f = 0.3))

#   env = env_names[3]

  for (env in env_names) {

    env_path <- paths_by_env[str_detect(paths_by_env, env)]
    vars_to_plot <- names(data)[str_detect(names(data), paste(environ_list[[env]], collapse = "|"))]

    # Si no hay variables para este entorno, saltar al siguiente
    if (length(vars_to_plot) == 0) {
      next
    }

        # Si hay menos de 3 variables, no tiene sentido el radar; saltar al siguiente
    if (length(vars_to_plot) < 3) {
      warning(paste0("Entorno '", env, 
                     "' con solo ", length(vars_to_plot), 
                     " variable(s); se omite radar."))
      next
    }
    
    # Calcular medias por cluster para este entorno
    summary_by_cluster <- data %>%
      filter(Cluster %in% cluster_ids) %>%
      group_by(Cluster) %>%
      summarise(across(all_of(vars_to_plot), ~ mean(.x, na.rm = TRUE))) %>%
      arrange(Cluster)


    # Renombrar variables
    clean_names <- put_in_order_names_vect(vector_name = names(summary_by_cluster)[-1], what_name = "Short label")
    


    names(summary_by_cluster)[-1] <- clean_names

    # Construir matriz para fmsb: max, min, luego cada cluster
    data_mat <- summary_by_cluster[,-1]

    # max_vals <- apply(data_mat, 2, max, na.rm = TRUE)

    max_vals <- data %>%
                filter(Cluster %in% cluster_ids) %>%
                summarise(
                    across(
                    all_of(vars_to_plot),
                    ~ max(.x, na.rm = TRUE)
                    )
                )

    names(max_vals) <-  put_in_order_names_vect(vector_name = names(max_vals), what_name = "Short label")

    # min_vals <- apply(data_mat, 2, min, na.rm = TRUE)

    min_vals <- data %>%
                filter(Cluster %in% cluster_ids) %>%
                summarise(
                    across(
                    all_of(vars_to_plot),
                    ~ min(.x, na.rm = TRUE)
                    )
                )

    names(min_vals) <-  put_in_order_names_vect(vector_name = names(min_vals), what_name = "Short label")

    radar_df <- rbind(Max = max_vals, Min = min_vals, as.data.frame(data_mat))

    rownames(radar_df)[3 + seq_len(n_clusters) - 1] <- paste0("Cluster_", summary_by_cluster$Cluster)

    # Ruta de salida y guardado
    plot_filename <- file.path(env_path, paste0("radar_", env, "_all_clusters.png"))
    saved_plot_paths <- c(saved_plot_paths, plot_filename)

    if(env == "HE_variables"){
        
            width_to <- 1000
    } else { 
        width_to <- 1100
    }

    png(plot_filename, width = width_to, height = 900)
    radarchart(
      radar_df,
    #   radar_df[c(1,2,3,4,5), ],
      axistype = 4,
      # líneas
      pcol = cols_line,
      # áreas
      pfcol = cols_fill,
      plwd = 2,
      plty = 1,
      cglcol = "gray",
      cglty = 1,
      cglwd = 0.8,
      axislabcol = "black",
      vlcex = 0.8
    )
    legend(
      x = "topright",
      legend = paste0("C", summary_by_cluster$Cluster),
      col = cols_line,
      lty = 1,
      lwd = 6,
      bty = "n"
    )
    dev.off()
  }

  return(saved_plot_paths)
}


########################################################################
########################################################################
#   OPTIMA K BY NB CLUSTER
########################################################################
########################################################################


#' Determine the optimal number of clusters from NbClust results
#'
#' @description
#' This function extracts the best number of clusters (`Best.nc`) from a list of NbClust objects
#' and returns the most frequently recommended number (i.e., the statistical mode).
#'
#' @param set_list A list of objects returned by the `NbClust` function.
#' @param n_order Integer indicating which position in the `Best.nc` vector to use from each object. Defaults to `1`.
#'
#' @return An integer representing the most frequently suggested number of clusters across the list.
#' @export
#'
#' @author Fabian Coloma
#'
#' @examples
#' \dontrun{
#' result_list <- list(obj1, obj2, obj3)  # Each from NbClust
#' optimal_k <- get_optimal_k_by_nbcluster(result_list)
#' }
get_optimal_k_by_nbcluster <- function(set_list, n_order = 1) {
  # Extract the selected value from Best.nc for each element
  best_nc_values <- sapply(set_list, function(x) x$Best.nc[n_order])

  # Compute the mode (most frequent value)
  most_frequent_k <- as.numeric(names(sort(table(best_nc_values), decreasing = TRUE)[1]))

  # Optional: print diagnostics
  print(best_nc_values)
  message("Most frequently recommended k: ", most_frequent_k)

  return(most_frequent_k)
}

#' Generate and save correlation plots for grouped variable lists
#'
#' @description
#' This function takes a dataset and a named list of variable patterns. For each group,
#' it identifies the relevant variables in the data using pattern matching, computes the correlation matrix,
#' and saves a clustered heatmap plot as a PNG file.
#'
#' @param data A data frame containing the full dataset.
#' @param variable_list A named list where each element is a character vector of regex patterns to match variable names.
#' @param where_to_save A string specifying the directory path where the plots will be saved.
#'
#' @return A named list of file paths for the saved correlation plots.
#' @export
#'
#' @author Fabian Coloma
#'
#' @examples
#' \dontrun{
#' variable_patterns <- list(
#'   socio = c("age", "income"),
#'   health = c("bmi", "cholesterol")
#' )
#' output_paths <- descriptive_by_variable_list_corr_part(
#'   data = my_dataframe,
#'   variable_list = variable_patterns,
#'   where_to_save = "outputs/cor_plots"
#' )
#' }
descriptive_by_variable_list_corr_part <- function(
  data,
  variable_list,
  where_to_save
) {
  # Ensure the output directory exists
  dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)

  require(corrplot)
  require(stringr)
  require(here)

  variable_group_names <- names(variable_list)
  column_names <- names(data)
  output_paths <- list()

  for (group in variable_group_names) {
    patterns <- variable_list[[group]]

    # Select variables matching the group patterns
    selected_vars <- column_names[
      str_detect(column_names, paste(patterns, collapse = "|"))
    ]

    data_subset <- data[, selected_vars, drop = FALSE]
    
    # Rename columns using short labels
    cleaned_names <- put_in_order_names_vect(vector_name = names(data_subset),key_col = "correlation", what_name = "Short label")
    names(data_subset) <- cleaned_names
    
    # Output path
    file_path <- here::here(where_to_save, paste0(group, ".png"))
    output_paths[[group]] <- file_path

    # Save correlation plot
    png(file_path, width = 2000, height = 2000, res = 150, units = "px")
    corrplot(cor(data_subset, use = "pairwise.complete.obs"), order = "hclust")
    dev.off()
  }

  return(output_paths)
}

#' Generate optimal number of clusters (k) using NbClust and Gower distance
#'
#' @description
#' This function computes the optimal number of clusters for a given set of variables using 
#' the Gower distance and selected clustering indices (e.g., silhouette, Dunn, cindex). 
#' It handles mixed data types by separating numeric and categorical variables, applying 
#' one-hot encoding and scaling where appropriate, and then calculating distances and evaluating cluster quality.
#'
#' @title generator_optimal_k_generator
#'
#' @param data A data frame containing the full dataset.
#' @param list_variables A character vector of variable names to include in clustering.
#'
#' @return A named list of `NbClust` results, one per selected clustering index.
#' @author Fabian Coloma
#' @export
#'
#' @examples
#' \dontrun{
#' selected_vars <- c("income", "education", "region", "age")
#' optimal_k <- generator_optimal_k_generator(my_data, selected_vars)
#' }
generator_optimal_k_generator <- function(data, list_variables) {
  require(caret)
  require(cluster)
  require(NbClust)
  require(dplyr)

  # Subset dataset with selected variables
  data_subset <- data[, list_variables]

  # Separate numeric and categorical variables
  data_numeric <- data_subset %>% select(where(is.numeric))
  data_character <- data_subset %>% select(where(is.character))

  # Convert categorical variables to dummy variables
  dummy_vars <- caret::dummyVars("~ .", data = data_character, fullRank = TRUE)
  data_dummies <- predict(dummy_vars, newdata = data_character) %>% as.data.frame()

  # Scale numeric variables
  scaled_data_numeric <- scale(data_numeric)

  # Combine numeric and dummy variables
  data_combined <- cbind(scaled_data_numeric, data_dummies)
  names_dummies <- names(data_dummies)

  # Compute Gower distance for mixed-type data
  dist_matrix <- cluster::daisy(
    data_combined,
    metric = "gower",
    type = list(asymm = names_dummies)
  ) %>% as.dist()

  # Safe indices that are compatible with Gower distance
  safe_indices <- c("silhouette", "dunn", "cindex")
  nbclust_results_list <- list()

  # Run NbClust for each safe index
  for (index_name in safe_indices) {
    message("Running NbClust with index: ", index_name)

    result <- NbClust::NbClust(
      diss = dist_matrix,
      distance = NULL,
      min.nc = 2,
      max.nc = 10,
      method = "average",
      index = index_name
    )

    nbclust_results_list[[index_name]] <- result
  }

  return(nbclust_results_list)
}

##########
##########
## other-block
##########
##########


#' Generate standardized numeric and dummy variables for modeling
#'
#' @description
#' This function prepares a dataset for modeling by:
#' (1) scaling numeric variables to have mean 0 and standard deviation 1, and
#' (2) converting categorical variables (character or factor) into dummy (one-hot encoded) 
#' variables using full rank encoding.
#'
#' @title generator_dummy_vars_and_std_num_vars
#'
#' @param data A data.frame containing the dataset.
#' @param general_variables A character vector of variable names to include from `data`.
#'
#' @return A data.frame with standardized numeric variables and dummy-coded categorical variables.
#' @author Fabian Coloma
#' @export
#'
#' @examples
#' \dontrun{
#' variables <- c("age", "gender", "education")
#' transformed_data <- generator_dummy_vars_and_std_num_vars(my_data, variables)
#' }
generator_dummy_vars_and_std_num_vars <- function(data, general_variables) {
  require(caret)
  require(dplyr)

  # Subset the dataset with selected variables
  data_subset <- data[, general_variables]

  # Separate numeric and categorical variables
  data_numeric <- data_subset %>% select(where(is.numeric))
  data_character <- data_subset %>% select(where(~ is.character(.) || is.factor(.)))

  # Convert categorical variables to dummy variables (full-rank encoding)
  dummy_model <- caret::dummyVars("~ .", data = data_character, fullRank = TRUE)
  data_dummies <- predict(dummy_model, newdata = data_character) %>% as.data.frame()

  # Scale numeric variables
  scaled_data_numeric <- scale(data_numeric)

  # Combine scaled numeric and dummy variables
  processed_data <- cbind(scaled_data_numeric, data_dummies)

  return(processed_data)
}

#' Preprocess mixed-type data for clustering
#'
#' This function scales numeric variables and creates dummy variables
#' for categorical features in the input dataset.
#'
#' @param data A data.frame with mixed numeric and categorical variables.
#' @return A list with combined data, numeric variable names, dummy variable names.
#' @export
preprocess_mixed_data <- function(data, variables_to_use) {
  data <- data[, variables_to_use]
  require(caret)
  require(dplyr)

  data_numeric <- data %>% select(where(is.numeric))
  data_categorical <- data %>% select(where(~ is.character(.) || is.factor(.)))

  scaled_numeric <- scale(data_numeric)

  if (ncol(data_categorical) > 0) {
    dummy_vars <- dummyVars("~ .", data = data_categorical, fullRank = TRUE)
    data_dummies <- predict(dummy_vars, newdata = data_categorical) %>% as.data.frame()
  } else {
    data_dummies <- data.frame()
  }

  combined <- cbind(scaled_numeric, data_dummies)

  return(list(
    combined = combined,
    numeric_names = names(data_numeric),
    dummy_names = names(data_dummies),
    all_names = names(combined)
  ))
}


#' Compute combined distance matrix from numeric and categorical distances
#'
#' @param numeric_data A numeric matrix or data.frame.
#' @param dummy_data A dummy-encoded data.frame.
#' @param alpha Weight between numeric and categorical distance (0-1).
#' @return A normalized and combined distance matrix of class 'dist'.
#' @export
compute_combined_distance <- function(numeric_data, dummy_data, alpha = 0.5) {
  dist_num <- dist(numeric_data, method = "canberra")
  dist_cat <- dist(dummy_data, method = "manhattan")

  norm <- function(x) (x - min(x)) / (max(x) - min(x))

  dist_mix <- alpha * norm(as.matrix(dist_num)) + (1 - alpha) * norm(as.matrix(dist_cat))

  return(as.dist(dist_mix))
}


#' Estimate epsilon parameter for DBSCAN using elbow method
#'
#' @param dist_matrix A distance matrix of class 'dist'.
#' @param k Number of nearest neighbors to consider.
#' @return Estimated epsilon value.
#' @export
estimate_eps_elbow <- function(dist_matrix, k) {
  dist_mat <- as.matrix(dist_matrix)
  kth_dist <- apply(dist_mat, 1, function(x) sort(x)[k + 1])
  ordered <- sort(kth_dist)
  curvature <- diff(diff(ordered))
  elbow_idx <- which.max(curvature) + 1
  return(ordered[elbow_idx])
}


#' Perform DBSCAN clustering and generate multiple visual outputs
#'
#' This function applies DBSCAN clustering on a dataset using a mixed-type distance matrix
#' (numeric variables scaled and categorical variables converted to dummy variables).
#' It generates a PCA projection, dendrogram, descriptive plots, and geographic cluster maps.
#'
#' @title dbscan_clustering_plot
#' @param data A data.frame containing the dataset.
#' @param all_environment_variables A character vector with variable names to be used in clustering.
#' @param where_to_save A string indicating the directory where plots and outputs will be saved.
#' @param max_clusters Integer specifying the max number of clusters to evaluate (default = 10).
#' @param manual_k Optional fixed number of clusters to force the dendrogram cut.
#' @param manhica A spatial sf object with the map background.
#' @param health_facilities Spatial data of health facilities.
#' @param roads Spatial data of roads.
#' @param prefix A string prefix to name files.
#'
#' @return A list containing:
#'   - data_w_cluster: dataset with cluster assignments
#'   - clust_pca_names_used_variables: variable names used
#'   - optimal_k: selected number of clusters
#'   - plot_tree_path: path to dendrogram plot
#'   - proyection_plot_path: path to PCA projection plot
#'   - list_geo_map_clust_plot: list of cluster maps
#'   - list_num_desc: numeric descriptive plots
#'   - list_cat_desc: categorical descriptive plots
#'   - plot_cluster: model results for Cluster variable
#'   - pca_info: PCA object
#'   - cluster_of_data: vector of assigned clusters
#'
#' @author Fabian Coloma
#' @export
#'
#' @examples
#' \dontrun{
#' dbscan_clustering_plot(data, vars, "outputs/", 10, NULL, manhica, facilities, roads, "prefix")
#' }
dbscan_clustering_plot <- function(
  data,
  all_environment_variables,
  where_to_save,
  max_clusters = 10,
  manual_k = NULL,
  manhica,
  health_facilities,
  roads,
  prefix = NULL
) {
  dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)

  # Step 1: Preprocess Data
  prep_result <- preprocess_mixed_data(data, all_environment_variables)

  # Step 2: Compute Combined Distance Matrix
  dist_matrix <- compute_combined_distance(prep_result$data_scaled, prep_result$data_dummies)

  # Step 3: Estimate Epsilon (eps) for DBSCAN
  eps_auto <- estimate_eps_elbow(dist_matrix)
  min_pts <- length(all_environment_variables) + 1

  # Step 4: Run DBSCAN
  db_result <- dbscan::dbscan(dist_matrix, eps = eps_auto, minPts = min_pts)
  data$Cluster <- as.character(db_result$cluster)

  # Step 5: Run Hierarchical Clustering and PCA
  hc_result <- run_hierarchical_clustering(prep_result$data_combined, manual_k, max_clusters)
  data$Cluster <- as.character(hc_result$clusters)

  # Step 6: PCA Projection and Save
  pca_result <- save_pca_and_cluster_plot(
    data, prep_result$data_combined, prefix, where_to_save, hc_result$optimal_k
  )

  # Step 7: Descriptive Plots and Mapping
  desc_num <- generator_boxplot_by_list(data, prep_result$num_names, where_to_save, prefix)
  desc_cat <- generator_bar_stacked_plot(data, prep_result$cat_names, where_to_save, prefix)
  geo_maps <- creating_map_separate_by_class(
    manhica = manhica,
    health_facilities = health_facilities,
    data = data,
    var_to_split = "Cluster",
    roads = roads,
    where_to_save = file.path(where_to_save, paste0(prefix, "_hierarchical")),
    pre_fix = paste0(prefix, "_hierarchical")
  )

  # Step 8: Cluster Modeling
  cluster_model <- mod_complete_sig_variables(
    data = data,
    covariables = c(
      "offset(log(fu_months))",
      "as.factor(gender)",
      "healthdist_final_std",
      "as.factor(healthdist_name_facility)",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
    ),
    significative_variables = c("Cluster"),
    set_model = "zero inflated",
    outcome = "h_visits",
    name_facility_in_order = TRUE,
    STD_variable = TRUE
  )

  # Return results
  list(
    data_w_cluster = pca_result$data_pca,
    clust_pca_names_used_variables = prep_result$combined_names,
    optimal_k = hc_result$optimal_k,
    plot_tree_path = hc_result$tree_path,
    proyection_plot_path = pca_result$projection_path,
    list_geo_map_clust_plot = geo_maps,
    list_num_desc = desc_num,
    list_cat_desc = desc_cat,
    plot_cluster = cluster_model,
    pca_info = pca_result$pca_obj,
    cluster_of_data = data$Cluster
  )
}

#' Perform hierarchical clustering and visualization on mixed-type data
#'
#' This function performs hierarchical clustering on a dataset that includes both
#' numeric and categorical variables. It uses Gower distance to handle mixed data types
#' and applies PCA for projection visualization. It generates dendrogram, cluster assignment,
#' descriptive plots, spatial visualizations, and evaluates cluster quality.
#'
#' @title hierarchical_clustering_plot
#' @param data A data.frame containing the dataset to cluster.
#' @param all_environment_variables Character vector of variable names to include in clustering.
#' @param where_to_save Directory path where outputs will be saved.
#' @param max_clusters Maximum number of clusters to evaluate if `manual_k` is not provided.
#' @param manual_k Optional fixed number of clusters.
#' @param manhica sf object representing the study area background.
#' @param health_facilities Spatial data for health facilities.
#' @param roads Spatial data for roads.
#' @param prefix Prefix for naming output files.
#'
#' @return A list with cluster information, plots, PCA results, and model output.
#' @author Fabian Coloma
#' @export
hierarchical_clustering_plot <- function(
  data,
  all_environment_variables,
  where_to_save,
  max_clusters = 10,
  manual_k = NULL,
  manhica,
  health_facilities,
  roads,
  prefix = NULL
) {
  require(caret)
  require(cluster)
  require(ggplot2)
  require(ggdendro)
  require(dplyr)
  require(stringr)
  require(here)
  require(sf)
  require(tmap)

  dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)

  # Subset relevant variables
  data_subset <- data[, all_environment_variables]
  data_numeric <- data_subset %>% select(where(is.numeric))
  data_character <- data_subset %>% select(where(is.character))

  names_numeric_columns <- names(data_numeric)
  names_character_columns <- names(data_character)

  # Convert categorical variables to dummy variables
  dummy_vars <- dummyVars("~ .", data = data_character, fullRank = TRUE)
  data_dummies <- predict(dummy_vars, newdata = data_character) %>% as.data.frame()
  names_data_dummies <- names(data_dummies)

  # Scale numeric variables and combine
  scaled_data_numeric <- scale(data_numeric)
  data_combined <- cbind(scaled_data_numeric, data_dummies)
  names_data_combined <- names(data_combined)

  # Compute Gower distance
  dist_matrix <- as.dist(daisy(data_combined, metric = "gower", type = list(asymm = names_data_dummies)))

  # Hierarchical clustering using complete linkage
  hclust_result <- hclust(dist_matrix, method = "complete")

  # Determine optimal number of clusters
  if (is.null(manual_k)) {
    silhouette_scores <- sapply(2:max_clusters, function(k) {
      clusters <- cutree(hclust_result, k)
      mean(silhouette(clusters, dist_matrix)[, 3])
    })
    optimal_k <- which.max(silhouette_scores) + 1
  } else {
    optimal_k <- manual_k
  }

  # Assign cluster labels
  clusters <- cutree(hclust_result, k = optimal_k)
  data$Cluster <- as.character(clusters)

  # Silhouette score
  sil <- silhouette(as.numeric(data$Cluster), daisy(data_combined, type = list(asymm = names_data_dummies)))
  silhouette_score <- mean(sil[, 3])

  # Dendrogram plot
  dendro_data <- as.dendrogram(hclust_result) %>% dendro_data()
  dendrogram_plot <- ggdendrogram(dendro_data, rotate = FALSE, size = 1) +
    labs(title = "Hierarchical Clustering Dendrogram", x = "Observations", y = "Height") +
    theme_minimal(base_size = 14)

  dendrogram_path <- here(where_to_save, paste0(prefix, "_cluster_plot_tree.png"))
  ggsave(dendrogram_path, dendrogram_plot, dpi = 600, width = 12, height = 8, units = "in")

  # PCA projection
  pca_path <- here(where_to_save, paste0(prefix, "_paper2_pca_out_1"))
  data_pca_in <- add_pca_and_save_v3(
    data_combined,
    all_scaled = FALSE,
    where_to_save = pca_path,
    list_variables = names_data_combined,
    environment_end = prefix,
    many_dim = 10,
    clusters_var = data$Cluster
  )

  data_fin <- cbind(
    data,
    data_pca_in$data_all_pc %>% select(str_detect(names(.), prefix))
  )

  data_pca_in$data_all_pc <- NULL

  name_dim_1 <- paste0(prefix, "_DIM_01")
  name_dim_2 <- paste0(prefix, "_DIM_02")

  # Descriptive plots
  list_num_desc <- generator_boxplot_by_list(data_fin, names_numeric_columns, where_to_save, prefix)
  list_cat_desc <- generator_bar_stacked_plot(data_fin, names_character_columns, where_to_save, prefix)

  # PCA projection clustering plot
  cluster_plot <- ggplot(data_fin, aes(x = .data[[name_dim_1]], y = .data[[name_dim_2]], color = factor(Cluster))) +
    geom_point(size = 3, alpha = 0.7) +
    labs(title = paste("Hierarchical Clustering (Optimal K =", optimal_k, ")"),
         x = "PC 1", y = "PC 2", color = "Cluster") +
    theme_minimal()

  proj_path <- here(where_to_save, paste0(prefix, "_hierarquical_proyection_cluster_plot.png"))
  ggsave(proj_path, cluster_plot, dpi = 600, width = 10, height = 8, units = "in")

  # Geo maps
  geo_path <- here(where_to_save, paste0(prefix, "_hierarchical"))
  list_geo_map_clust <- creating_map_separate_by_class(
    manhica = manhica,
    health_facilities = health_facilities,
    data = data,
    var_to_split = "Cluster",
    roads = roads,
    where_to_save = geo_path,
    pre_fix = paste0(prefix, "_hierarchical")
  )

  # Cluster modeling
  plot_cluster <- mod_complete_sig_variables(
    data = data_fin,
    covariables = c(
      "offset(log(fu_months))",
      "as.factor(gender)",
      "healthdist_final_std",
      "as.factor(healthdist_name_facility)",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
    ),
    significative_variables = c("Cluster"),
    set_model = "zero inflated",
    outcome = "h_visits",
    name_facility_in_order = TRUE,
    STD_variable = TRUE
  )

  return(list(
    clust_pca_names_used_variables = names_data_combined,
    optimal_k = optimal_k,
    plot_tree_path = dendrogram_path,
    proyection_plot_path = proj_path,
    list_geo_map_clust_plot = list_geo_map_clust,
    list_num_desc = list_num_desc,
    list_cat_desc = list_cat_desc,
    plot_cluster = plot_cluster,
    pca_info = data_pca_in,
    cluster_of_data = data$Cluster,
    silhouette_score = silhouette_score
  ))
}


##########
##########
## otro bloque
##########
##########

#' Compute the Most Frequent (Modal) Value in a Vector
#'
#' @description
#' This function returns the mode (most frequent value) from a given vector.
#' It handles both numeric and character/factor inputs.
#'
#' @param x A vector (numeric, character, or factor) from which to extract the most frequent value.
#'
#' @return The most frequent value in the vector `x`. If multiple values have the same maximum frequency,
#' the first occurring one is returned.
#'
#' @author Fabian Coloma
#' @export
#'
#' @examples
#' most_freq_value_fun(c(1, 2, 2, 3, 3, 3, 1))  # Returns 3
#' most_freq_value_fun(c("apple", "banana", "apple", "apple"))  # Returns "apple"
most_freq_value_fun <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##' K-Prototypes clustering with evaluation, PCA visuals, maps, and profiling
##'
##' Runs K-Prototypes on mixed-type data (numeric + categorical), evaluates
##' candidate solutions across multiple cluster counts (WCSS, Silhouette, Dunn),
##' selects a final \emph{k}, and produces PCA scatterplots, geographic maps,
##' and descriptive profiles (boxplots for numeric, stacked bars for categorical).
##' Optionally, a fixed \code{manual_k} can be supplied to skip the search.
##'
##' @title Perform K-Prototypes clustering with evaluation and visualization
##'
##' @param data A \code{data.frame} containing all variables referenced below.
##' @param all_environment_variables Character vector with the variable names
##'   (both numeric and categorical) to include in clustering.
##' @param where_to_save Directory path where figures/outputs will be saved.
##'   Created recursively if it doesn't exist.
##' @param max_clusters Integer. Maximum number of clusters to evaluate (default \code{10}).
##'   Used only when \code{manual_k} is \code{NULL}.
##' @param manual_k Integer or \code{NULL}. If numeric, forces a fixed number of clusters
##'   and skips the WCSS/Silhouette/Dunn search (evaluation plot paths will be \code{NULL}).
##' @param manhica \code{sf} object for the region polygon(s).
##' @param health_facilities \code{sf} point layer of health facility locations.
##' @param roads \code{sf} line layer of road networks.
##' @param buffer_by_facilities Any spatial or attribute layer required by
##'   \code{creating_map_separate_by_class()} (project-specific).
##' @param zona Spatial layer used in geographic mapping (project-specific).
##' @param districts Spatial layer used in geographic mapping (project-specific).
##' @param prefix Character prefix for saved filenames/plot labels; may also be
##'   used in PCA dimension naming downstream.
##' @param list_entorno \code{list} defining variable domains/groups
##'   (e.g., \code{list(SRE_variables=..., SEA_variables=..., ...)}),
##'   passed to radar/spider plot helpers.
##'
##' @return A \code{list} with:
##' \itemize{
##'   \item \code{best_model}: the selected \code{kproto} object.
##'   \item \code{optimal_k}: chosen number of clusters.
##'   \item \code{evaluation_plot_path}, \code{evaluation_plot_path_sil},
##'         \code{evaluation_plot_path_dun}: file paths to WCSS, Silhouette, and Dunn plots
##'         (or \code{NULL} if \code{manual_k} supplied).
##'   \item \code{clustering_plot_path}: PCA scatter plot path.
##'   \item \code{list_geo_map_clust}: outputs from \code{creating_map_separate_by_class()}.
##'   \item \code{list_num_desc}, \code{list_cat_desc}: descriptive plot objects/paths
##'         (numeric and categorical respectively).
##'   \item \code{plot_cluster}: result of modeling with clusters as predictors
##'         via \code{mod_complete_sig_variables()}.
##'   \item \code{pca_info}: list as returned by \code{add_pca_and_save_v3()}.
##'   \item \code{cluster_of_data}: character vector with assigned cluster IDs per row.
##'   \item \code{silhouette_score}: mean silhouette of the final clustering.
##'   \item \code{table_cluster}: \code{tibble} with cluster sizes.
##'   \item \code{spiders_path}, \code{spiders_path_all_together}: radar/spider plot paths.
##' }
##'
##' @details
##' \strong{Pipeline}
##' \enumerate{
##'   \item Subset to \code{all_environment_variables}; split numeric vs categorical.
##'   \item Scale numeric, factorize categorical, build dummy matrix for evaluation metrics.
##'   \item For \code{k=2..max_clusters} (unless \code{manual_k} provided), fit K-Prototypes,
##'         compute WCSS, Silhouette, and Dunn; pick \emph{k} via a simple consensus rule.
##'   \item Compute PCA (via \code{add_pca_and_save_v3()}), save plots.
##'   \item Profile clusters: numeric (boxplots), categorical (stacked bars).
##'   \item Map clusters geographically via \code{creating_map_separate_by_class()}.
##'   \item Fit a downstream count model including cluster membership as a predictor
##'         (\code{mod_complete_sig_variables()}).
##'   \item Create radar/spider plots by domain (\code{plot_radar_by_env_and_cluster()},
##'         \code{plot_radar_by_env_and_cluster_v2()}).
##' }
##'
##' \strong{Dependencies/Helpers (project-specific)}:
##' \code{most_freq_value_fun()}, \code{get_cluster_labels()}, \code{add_pca_and_save_v3()},
##' \code{generator_boxplot_by_list()}, \code{generator_bar_stacked_plot()},
##' \code{creating_map_separate_by_class()}, \code{plot_radar_by_env_and_cluster()},
##' \code{plot_radar_by_env_and_cluster_v2()}, and \code{mod_complete_sig_variables()}.
##'
##' \strong{Colors}: Uses Okabe–Ito palette (color-blind safe) up to 10 clusters,
##' with a \code{viridis} fallback for larger \emph{k}.
##'
##' @author Fabian Coloma
##'
##' @seealso
##'  \code{\link[clustMixType]{kproto}},
##'  \code{\link[cluster]{silhouette}},
##'  \code{\link[clusterCrit]{intCriteria}},
##'  \code{\link{mod_complete_sig_variables}}
##'
##' @importFrom dplyr select mutate across group_by summarise relocate distinct
##' @importFrom ggplot2 ggplot aes geom_point geom_line geom_label_repel labs theme_minimal ggsave
##' @importFrom ggrepel geom_label_repel
##' @importFrom caret dummyVars
##' @importFrom cluster silhouette daisy
##' @importFrom clusterCrit intCriteria
##' @importFrom stringr str_detect
##' @importFrom here here
##'
##' @examples
##' \dontrun{
##' # Example sketch (assumes project helpers, sf layers, and data exist):
##' out <- kpprototype_clustering_plot(
##'   data = df,
##'   all_environment_variables = c("age","gender","road_type","elevation"),
##'   where_to_save = here::here("outputs","clustering"),
##'   max_clusters = 8,
##'   manual_k = NULL,
##'   manhica = manhica_sf,
##'   health_facilities = hf_sf,
##'   roads = roads_sf,
##'   buffer_by_facilities = buffers_sf,
##'   zona = zonas_sf,
##'   districts = districts_sf,
##'   prefix = "env",
##'   list_entorno = list(SRE_variables = sre_vars,
##'                       SEA_variables = sea_vars,
##'                       HE_variables  = he_vars,
##'                       FI_variables  = fi_vars)
##' )
##' out$optimal_k
##' out$clustering_plot_path
##' }
##'
##' @export
kpprototype_clustering_plot <- function(
  data,
  all_environment_variables,
  where_to_save,
  max_clusters = 10,
  manual_k = NULL,
  manhica,
  health_facilities,
  roads,
  buffer_by_facilities,
  zona,
  districts,
  prefix = NULL,
  list_entorno = NULL
) {

     # ########################################################################
  # ########################################################################
  # #   debug
  # ########################################################################
  # ########################################################################


    # source("R/read-data.R")
    # source("R/clean-data.R")
    # source("R/process-data.R")
    # source("R/report-execution.R")
    # source("Y:/fcoloma/package_fabi/R/basics_analisis.R")
    # source("Y:/fcoloma/package_fabi/R/hr_ic.R")
    # source("Y:/fcoloma/package_fabi/R/plot_hr_ic.R")

    # data = tar_read(data_end_90_perce_distance_quantile_paper2)

    # all_environment_variables =  tar_read(select_cluster_variables_report_part)
    # where_to_save = here::here(
    #     "outputs",
    #     "CLUST_v4",
    #     "01_CLUSTER_B")
    # max_clusters = 10  # Evaluar de 2 a 10 clusters
    # manual_k = 3
    # manhica = tar_read(manhica)
    # health_facilities = tar_read(health_facilities)
    # roads = tar_read(roads)
    # buffer_by_facilities = tar_read(buffer_by_facilities)
    # zona = tar_read(postos)
    # districts = tar_read(districts)
    # prefix = "ela_prot_nolim"
    # list_entorno  = list( SRE_variables = tar_read(SRE_variables),
    #                 SEA_variables = tar_read(SEA_variables),
    #                 HE_variables = tar_read(HE_variables),
    #                 FI_variables = tar_read(FI_variables))

 set.seed(3333)  

  # -------------------------------------------------------------------------
  # Setup & dependencies
  # -------------------------------------------------------------------------
  require(clustMixType)
  require(dplyr)
  require(ggplot2)
  require(ggrepel)
  require(here)
  require(caret)
  require(sf)
  require(tmap)
  require(cluster)
  require(clusterCrit)
  require(stringr)  # used for str_detect()
  require(viridis)  # used for viridis() fallback palette
  require(RColorBrewer)

  # Create output directory if it doesn't exist
  dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)

  # -------------------------------------------------------------------------
  # Step 1: Prepare data
  # -------------------------------------------------------------------------

  # Subset to selected environmental variables
  data_subset <- data[, all_environment_variables]

  # Separate numeric and character variables
  data_numeric   <- data_subset %>% dplyr::select(where(is.numeric))
  data_character <- data_subset %>% dplyr::select(where(is.character))

  if (ncol(data_numeric) == 0 || ncol(data_character) == 0) {
    stop("The dataset must contain both numeric and categorical variables.22")
  }

  # Scale numeric variables
  data_scaled <- data_numeric %>% scale() %>% as.data.frame()

  # Factorize character variables and generate dummies for evaluation metrics
  data_character <- data_character %>% dplyr::mutate(across(everything(), as.factor))
  dummy_vars  <- caret::dummyVars("~ .", data = data_character, fullRank = TRUE)
  data_dummies <- predict(dummy_vars, newdata = data_character) %>% as.data.frame()

  # Combined data for: (a) kproto (mixed), (b) distances/criteria (numeric-only)
  data_combined    <- cbind(data_scaled, data_character)
  data_combined_pc <- cbind(data_scaled, data_dummies)

  # -------------------------------------------------------------------------
  # Step 2: Evaluate clustering over different k (or use manual_k)
  # -------------------------------------------------------------------------
  total_withinss    <- c()
  silhouette_scores <- c()
  dunn_indices      <- c()
  kproto_models     <- list()

  # Gower-like mixed distance via daisy (dummy-encoded cats)
  data_distance_matrix <- cluster::daisy(
    data_combined_pc, type = list(asymm = names(data_dummies))
  )

  lam <- clustMixType::lambdaest(data_combined)  # estima lambda con el mismo set mixto


  if (is.numeric(manual_k)) {
    # Fixed k (skip evaluation curves)

    # kproto_result <- clustMixType::kproto(
    #     data_combined,
    #     k       = manual_k)


# decide hiperparámetros según kk
if (manual_k >= 6) {
  p_start  <- 0.20
  iter.max <- 50
} else if (manual_k >= 5) {
  p_start  <- 0.70
  iter.max <- 50
} else if (manual_k >= 4) {
  p_start  <- 0.80
  iter.max <- 100
} else {
  p_start  <- 0.95
  iter.max <- 100
}

    kproto_result <- clustMixType::kproto(
        data_combined,
        k       = manual_k,
        lambda  = lam,            # usa la lambda fija
        type    = "huang",        # explícito (lambda solo aplica aquí)
        init   = "nstart.m",    # opcional si quieres una inicialización concreta
        p_nstart.m = p_start,      # opcional si usas init = "nstart.m"
        na.rm   = "yes",
        iter.max = iter.max,
        keep.data = TRUE
    ) # , nstart = 7, "nbh.dens", "sel.cen" or "nstart.m")
    
    # kproto_result <- clustMixType::kproto(data_combined, k = manual_k, init = "nbh.dens") # "nbh.dens", "sel.cen" or "nstart.m")
    optimal_k     <- manual_k
    best_model    <- kproto_result
    best_model_sil  <- kproto_result
    best_model_dunn <- kproto_result
  } else {
    # Search k in [2, max_clusters]
    for (k in 2:max_clusters) {

        print("estamos en el cluster numer :::::::")
        print(k)
    #   set.seed(1234)

if (k >= 6) {
  p_start  <- 0.20
  iter.max <- 50
} else if (k >= 5) {
  p_start  <- 0.70
  iter.max <- 50
} else if (k >= 4) {
  p_start  <- 0.80
  iter.max <- 100
} else {
  p_start  <- 0.95
  iter.max <- 100
}

      kproto_result <- clustMixType::kproto(data_combined, k = k,
        lambda  = lam,            # usa la lambda fija
        type    = "huang",        # explícito (lambda solo aplica aquí)
        init   = "nstart.m",    # opcional si quieres una inicialización concreta
        p_nstart.m = p_start,      # opcional si usas init = "nstart.m"
        na.rm   = "yes",
        iter.max = iter.max,
        keep.data = TRUE
    )
      kproto_models[[k]] <- kproto_result
      total_withinss     <- c(total_withinss, kproto_result$tot.withinss)

      # Silhouette (mean)
      sil <- cluster::silhouette(as.numeric(kproto_result$cluster), data_distance_matrix)
      silhouette_scores <- c(silhouette_scores, mean(sil[, 3]))

      # Dunn index
      dunn_val <- clusterCrit::intCriteria(
        as.matrix(data_combined_pc),
        as.integer(kproto_result$cluster),
        c("Dunn")
      )
      dunn_indices <- c(dunn_indices, dunn_val$dunn)
    }

    # Offsets of +1 because loop starts at k=2
    optimal_k      <- which.min(total_withinss) + 1
    optimal_k_sil  <- which.max(silhouette_scores) + 1
    optimal_k_dunn <- which.max(dunn_indices) + 1

    # Majority vote / most frequent among the three candidates
    opt <- most_freq_value_fun(c(optimal_k, optimal_k_sil, optimal_k_dunn))
    optimal_k <- opt

    best_model      <- kproto_models[[optimal_k]]
    best_model_sil  <- kproto_models[[optimal_k]]
    best_model_dunn <- kproto_models[[optimal_k]]
  }

  # Attach cluster to original data and to numeric/dummy matrix
#   vector_clust <-  as.numeric(best_model$cluster)
#   vector_clust <- 4 - as.numeric(best_model$cluster)
if(optimal_k == 3){
    vector_clust <- 4 - as.numeric(best_model$cluster)
} else if(optimal_k == 2) {
  vector_clust <- abs(as.numeric(best_model$cluster) - 3)
} else {
    vector_clust <- as.numeric(best_model$cluster)
}
  
  data$Cluster             <- as.character(vector_clust)
  data_combined_pc$Cluster <- as.character(vector_clust)

  # Simple cluster size table
  table_cluster <- data %>%
    dplyr::select(Cluster) %>%
    dplyr::group_by(Cluster) %>%
    dplyr::summarise(child_number = dplyr::n())

  # -------------------------------------------------------------------------
  # Cluster labeling & palette
  # -------------------------------------------------------------------------
  n_clusters    <- optimal_k
  cluster_labels <- get_cluster_labels(n_clusters)                 # helper
  cluster_map    <- setNames(cluster_labels, as.character(1:n_clusters))
  data$ClusterLabel <- cluster_map[data$Cluster]

  # Okabe–Ito palette (<=10); otherwise viridis fallback
  okabe_ito <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999",
    "#000000", "#FFFFFF"
  )

  if (n_clusters > length(okabe_ito)) {
    palette_colors <- viridis::viridis(n_clusters, option = "D")
  } else {
    palette_colors <- okabe_ito[1:n_clusters]
  }
  names(palette_colors) <- cluster_labels

  # Mean silhouette of final clustering
  sil <- cluster::silhouette(as.numeric(data$Cluster), data_distance_matrix)
  silhouette_score <- mean(sil[, 3])

  # -------------------------------------------------------------------------
  # Step 3: Evaluation plots (when k is not fixed)
  # -------------------------------------------------------------------------
  if (!is.numeric(manual_k)) {
    eval_wcss_plot <- ggplot2::ggplot(
      data.frame(Clusters = 2:max_clusters, TotalWithinSS = total_withinss),
      ggplot2::aes(x = Clusters, y = TotalWithinSS)
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
    #   ggrepel::geom_label_repel(
    #     ggplot2::aes(label = ifelse(Clusters == optimal_k, paste0("Optimal: ", Clusters), ""))
    #   ) +
      ggplot2::labs(
        title = "Cluster Evaluation: Within-Cluster Sum of Squares",
        x = "Number of Clusters", y = "Total WithinSS"
      ) +
      ggplot2::theme_minimal()

    eval_sil_plot <- ggplot2::ggplot(
      data.frame(Clusters = 2:max_clusters, SilhouetteScore = silhouette_scores),
      ggplot2::aes(x = Clusters, y = SilhouetteScore)
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
    #   ggrepel::geom_label_repel(
    #     ggplot2::aes(label = ifelse(Clusters == optimal_k, paste0("Optimal: ", Clusters), ""))
    #   ) +
      ggplot2::labs(
        title = "Cluster Evaluation: Silhouette Score",
        x = "Number of Clusters", y = "Mean Silhouette Score"
      ) +
      ggplot2::theme_minimal()

    eval_dunn_plot <- ggplot2::ggplot(
      data.frame(Clusters = 2:max_clusters, DunnIndex = dunn_indices),
      ggplot2::aes(x = Clusters, y = DunnIndex)
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
    #   ggrepel::geom_label_repel(
    #     ggplot2::aes(label = ifelse(Clusters == optimal_k, paste0("Optimal: ", Clusters), ""))
    #   ) +
      ggplot2::labs(
        title = "Cluster Evaluation: Dunn Index",
        x = "Number of Clusters", y = "Dunn Index"
      ) +
      ggplot2::theme_minimal()

    path_wcss <- here::here(where_to_save, paste0(prefix, "_evaluation_plot_wcss.png"))
    ggplot2::ggsave(filename = path_wcss, plot = eval_wcss_plot, dpi = 600, width = 10, height = 8, units = "in")

    path_sil <- here::here(where_to_save, paste0(prefix, "_evaluation_plot_sil.png"))
    ggplot2::ggsave(filename = path_sil, plot = eval_sil_plot, dpi = 600, width = 10, height = 8, units = "in")

    path_dun <- here::here(where_to_save, paste0(prefix, "_evaluation_plot_dunn.png"))
    ggplot2::ggsave(filename = path_dun, plot = eval_dunn_plot, dpi = 600, width = 10, height = 8, units = "in")
  } else {
    path_wcss <- NULL
    path_sil  <- NULL
    path_dun  <- NULL
  }

  # -------------------------------------------------------------------------
  # Step 4: PCA (dimensionality reduction) & merge back into data
  # -------------------------------------------------------------------------
  path_pca <- here::here(where_to_save, paste0(prefix, "_pca"))
  data_pca_in <- add_pca_and_save_v3(
    data_combined_pc,
    all_scaled    = FALSE,
    where_to_save = path_pca,
    list_variables = names(data_combined_pc %>% dplyr::select(-Cluster)),
    environment_end = prefix,
    many_dim = 5,
    clusters_var = data$Cluster
  )

  data_fin <- cbind(
    data,
    data_pca_in$data_all_pc %>% dplyr::select(dplyr::contains(prefix))
  )
  data_pca_in$data_all_pc <- NULL

  # -------------------------------------------------------------------------
  # Step 5: Cluster profiling (numeric & categorical)
  # -------------------------------------------------------------------------
  data_character$Cluster <- data$Cluster
  list_num_desc <- generator_boxplot_by_list(data_fin, names(data_numeric), where_to_save, prefix)
  list_cat_desc <- generator_bar_stacked_plot(data_character, names(data_character), where_to_save, prefix)

  # -------------------------------------------------------------------------
  # Step 6: PCA scatter plot by cluster
  # -------------------------------------------------------------------------
  dim1 <- paste0(prefix, "_DIM_01")
  dim2 <- paste0(prefix, "_DIM_02")

  pca_cluster_plot <- ggplot2::ggplot(
    data_fin,
    ggplot2::aes(x = .data[[dim1]], y = .data[[dim2]], color = factor(ClusterLabel))
  ) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::labs(
      title = paste("K-Prototypes Clustering (Optimal K =", optimal_k, ")"),
      x = "PC 1", y = "PC 2", color = "Cluster"
    ) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(
    here::here(where_to_save, paste0(prefix, "_kprototypes_cluster_plot.png")),
    plot = pca_cluster_plot, dpi = 600, width = 10, height = 8, units = "in"
  )

  # -------------------------------------------------------------------------
  # Step 7: Geographic plot by cluster
  # -------------------------------------------------------------------------
  geo_path <- here::here(where_to_save, paste0(prefix, "_kprototypes"))
  list_geo_map_clust <- creating_map_separate_by_class(
    manhica          = manhica,
    health_facilities = health_facilities,
    data             = data,
    var_to_split     = "Cluster",
    roads            = roads,
    where_to_save    = geo_path,
    pre_fix          = paste0(prefix, "_kprototypes"),
    palette_colors   = palette_colors,
    buffer_by_facilities = buffer_by_facilities,
    zona             = zona,
    districts        = districts
  )

  # -------------------------------------------------------------------------
  # Step 8: Downstream modeling with cluster as predictor
  # -------------------------------------------------------------------------
  plot_cluster <- mod_complete_sig_variables(
    data = data_fin,
    covariables = c(
      "offset(log(fu_months))", "as.factor(gender)", "healthdist_final_std",
      "as.factor(healthdist_name_facility)", "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
    ),
    significative_variables = c("Cluster"),
    set_model = "zero inflated",
    outcome = "h_visits",
    name_facility_in_order = TRUE,
    STD_variable = TRUE,
    add_decriptive = TRUE
  )

  # -------------------------------------------------------------------------
  # Step 9: Spider/radar plots by domain
  # -------------------------------------------------------------------------
  # Patch to ensure "No maternal education" dummy exists if others present
  if (any(stringr::str_detect(names(data_dummies), "mother_education"))) {

    data_dummies <- data_dummies %>%
      dplyr::mutate(`mother_education.No maternal education` = ifelse(
        (`mother_education.Primary` == 0 | is.na(`mother_education.Primary`)) &
          (`mother_education.Secondary or tertiary` == 0 | is.na(`mother_education.Secondary or tertiary`)),
        1, 0
      )) %>%
      dplyr::relocate(
        `mother_education.No maternal education`,
        .after = `mother_education.Secondary or tertiary`
      )

    data_combined_pc <- data_combined_pc %>%
      dplyr::mutate(`mother_education.No maternal education` = ifelse(
        (`mother_education.Primary` == 0 | is.na(`mother_education.Primary`)) &
          (`mother_education.Secondary or tertiary` == 0 | is.na(`mother_education.Secondary or tertiary`)),
        1, 0
      )) %>%
      dplyr::relocate(
        `mother_education.No maternal education`,
        .after = `mother_education.Secondary or tertiary`
      )
  }

  spiders_path <- plot_radar_by_env_and_cluster(
    data = data_combined_pc,
    names_cat = names(data_dummies),
    names_num = names(data_numeric),
    environ_list = list_entorno,
    where_to_save = where_to_save,
    prefix = prefix
  )

  spiders_path_all_together <- plot_radar_by_env_and_cluster_v2(
    data = data_combined_pc,
    names_cat = names(data_dummies),
    names_num = names(data_numeric),
    environ_list = list_entorno,
    where_to_save = where_to_save,
    prefix = prefix
  )

  # -------------------------------------------------------------------------
  # Step 10: Return all results
  # -------------------------------------------------------------------------
  return(list(
    best_model = best_model,
    optimal_k = optimal_k,
    evaluation_plot_path = path_wcss,
    evaluation_plot_path_sil = path_sil,
    evaluation_plot_path_dun = path_dun,
    clustering_plot_path = here::here(where_to_save, paste0(prefix, "_kprototypes_cluster_plot.png")),
    list_geo_map_clust_plot = list_geo_map_clust,
    list_num_desc = list_num_desc,
    list_cat_desc = list_cat_desc,
    plot_cluster = plot_cluster,
    pca_info = data_pca_in,
    cluster_of_data = data$Cluster,
    silhouette_score = silhouette_score,
    table_cluster = table_cluster,
    spiders_path = spiders_path,
    spiders_path_all_together = spiders_path_all_together
  ))
}


# # # # ##' Perform K-Prototypes clustering with evaluation and visualization
# # # # ##'
# # # # ##' This function performs K-Prototypes clustering on mixed-type data (numerical and categorical).
# # # # ##' It evaluates clustering performance using Within-Cluster Sum of Squares (WCSS), Silhouette Score,
# # # # ##' and Dunn Index. It generates and saves evaluation plots, PCA visualizations, and descriptive
# # # # ##' analyses (boxplots, maps, radar plots).
# # # # ##'
# # # # ##' @param data Data frame with mixed-type variables for clustering.
# # # # ##' @param all_environment_variables Character vector with names of variables to use in clustering.
# # # # ##' @param where_to_save Directory path to save results.
# # # # ##' @param max_clusters Integer. Maximum number of clusters to evaluate (default is 10).
# # # # ##' @param manual_k Integer. Optionally provide a fixed number of clusters.
# # # # ##' @param manhica Spatial object (sf) representing the region.
# # # # ##' @param health_facilities Spatial object (sf) of health facility locations.
# # # # ##' @param roads Spatial object (sf) of road networks.
# # # # ##' @param prefix Prefix for saved filenames.
# # # # ##' @param list_entorno List of variable domains (e.g. SRE, SEA, etc.) for radar plots.
# # # # ##'
# # # # ##' @return A list containing the clustering model, optimal k, plots, and descriptive outputs.
# # # # ##' @export
# # # # # kpprototype_clust
# # # # kpprototype_clustering_plot <- function(
# # # #   data,
# # # #   all_environment_variables,
# # # #   where_to_save,
# # # #   max_clusters = 10,
# # # #   manual_k = NULL,
# # # #   manhica,
# # # #   health_facilities,
# # # #   roads,
# # # #   buffer_by_facilities,
# # # #   zona,
# # # #   districts,
# # # #   prefix = NULL,
# # # #   list_entorno = NULL
# # # # ) {

# # # #   # ########################################################################
# # # #   # ########################################################################
# # # #   # #   debug
# # # #   # ########################################################################
# # # #   # ########################################################################


# # # #     # source("R/read-data.R")
# # # #     # source("R/clean-data.R")
# # # #     # source("R/process-data.R")
# # # #     # source("R/report-execution.R")
# # # #     # source("Y:/fcoloma/package_fabi/R/basics_analisis.R")
# # # #     # source("Y:/fcoloma/package_fabi/R/hr_ic.R")
# # # #     # source("Y:/fcoloma/package_fabi/R/plot_hr_ic.R")

# # # #     # data = tar_read(data_end_90_perce_distance_quantile_paper2)

# # # #     # all_environment_variables =  tar_read(select_cluster_variables_report_part)
# # # #     # where_to_save = here::here(
# # # #     #     "outputs",
# # # #     #     "CLUST_v4",
# # # #     #     "01_CLUSTER_B")
# # # #     # # max_clusters = 10  # Evaluar de 2 a 10 clusters
# # # #     # manual_k = 3
# # # #     # manhica = tar_read(manhica)
# # # #     # health_facilities = tar_read(health_facilities)
# # # #     # roads = tar_read(roads)
# # # #     # buffer_by_facilities = tar_read(buffer_by_facilities)
# # # #     # zona = tar_read(postos)
# # # #     # districts = tar_read(districts)
# # # #     # prefix = "ela_prot_nolim"
# # # #     # list_entorno  = list( SRE_variables = tar_read(SRE_variables),
# # # #     #                 SEA_variables = tar_read(SEA_variables),
# # # #     #                 HE_variables = tar_read(HE_variables),
# # # #     #                 FI_variables = tar_read(FI_variables))

# # # # # # set.seed(1234)  


# # # #   # Load required libraries
# # # #   require(clustMixType)
# # # #   require(dplyr)
# # # #   require(ggplot2)
# # # #   require(ggrepel)
# # # #   require(here)
# # # #   require(caret)
# # # #   require(sf)
# # # #   require(tmap)
# # # #   require(cluster)
# # # #   require(clusterCrit)

# # # #   # Create output directory if it doesn't exist
# # # #   dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)

# # # #   ##### Step 1: Prepare data #####

# # # #   # Subset only selected environmental variables
# # # #   data_subset <- data[, all_environment_variables]

# # # #   # Separate numeric and character variables
# # # #   data_numeric <- data_subset %>% select(where(is.numeric))
# # # #   data_character <- data_subset %>% select(where(is.character))

# # # #   if (ncol(data_numeric) == 0 || ncol(data_character) == 0) {
# # # #     stop("The dataset must contain both numeric and categorical variables.")
# # # #   }

# # # #   # Scale numeric variables
# # # #   data_scaled <- data_numeric %>% scale() %>% as.data.frame()

# # # #   # Convert character variables to factors and create dummy variables
# # # #   data_character <- data_character %>% mutate(across(everything(), as.factor))
# # # #   dummy_vars <- dummyVars("~ .", data = data_character, fullRank = TRUE)
# # # #   data_dummies <- predict(dummy_vars, newdata = data_character) %>% as.data.frame()

# # # #   # Combine scaled numeric and categorical data
# # # #   data_combined <- cbind(data_scaled, data_character)
# # # #   data_combined_pc <- cbind(data_scaled, data_dummies)

# # # #   ##### Step 2: Evaluate clustering for different k #####

# # # #   total_withinss <- c()
# # # #   silhouette_scores <- c()
# # # #   dunn_indices <- c()
# # # #   kproto_models <- list()

# # # #   data_distance_matrix <- cluster::daisy(data_combined_pc, type = list(asymm = names(data_dummies)))

# # # # if(is.numeric(manual_k)){
# # # #     kproto_result <- clustMixType::kproto(data_combined, k = manual_k)

# # # #     optimal_k <- manual_k

# # # #     best_model <- kproto_result
# # # #     best_model_sil <- kproto_result
# # # #     best_model_dunn <- kproto_result
# # # # } else {
# # # #   for (k in 2:max_clusters) {
# # # #     set.seed(1234)
# # # #     kproto_result <- clustMixType::kproto(data_combined, k = k)
# # # #     kproto_models[[k]] <- kproto_result
# # # #     total_withinss <- c(total_withinss, kproto_result$tot.withinss)

# # # #     # Silhouette
# # # #     sil <- silhouette(as.numeric(kproto_result$cluster), data_distance_matrix)
# # # #     silhouette_scores <- c(silhouette_scores, mean(sil[, 3]))

# # # #     # Dunn Index
# # # #     dunn_val <- clusterCrit::intCriteria(as.matrix(data_combined_pc), as.integer(kproto_result$cluster), c("Dunn"))
# # # #     dunn_indices <- c(dunn_indices, dunn_val$dunn)
# # # #   }

# # # #   # le ponemos al min un mas uno porque empezamos desde dos clusters!
# # # #   optimal_k <- if (is.numeric(manual_k)) manual_k else which.min(total_withinss) +1
# # # #   optimal_k_sil <- if (is.numeric(manual_k)) manual_k else which.max(silhouette_scores) + 1
# # # #   optimal_k_dunn <- if (is.numeric(manual_k)) manual_k else which.max(dunn_indices) +1

# # # #   opt <- most_freq_value_fun(c(optimal_k, optimal_k_sil, optimal_k_dunn))
# # # #     optimal_k <- opt
# # # #   best_model <- kproto_models[[optimal_k]]
# # # #   best_model_sil <- kproto_models[[optimal_k]]
# # # #   best_model_dunn <- kproto_models[[optimal_k]]

# # # # }

# # # #   vector_clust <-  best_model$cluster 
# # # # #   vector_clust <-  4 - best_model$cluster 
# # # # #   vector_clust <- recode(best_model$cluster, `1` = 3, `3` = 1)
# # # #   data$Cluster             <- as.character(vector_clust)
# # # #   data_combined_pc$Cluster <- as.character(vector_clust)

# # # # #   table(data$Cluster)
# # # #   table_cluster <- data %>%
# # # #                     select(Cluster) %>%
# # # #                     group_by(Cluster) %>%
# # # #                     summarise(child_number = n())

# # # #     # Renombrar clusters
# # # #     n_clusters <- optimal_k
# # # #     cluster_labels <- get_cluster_labels(n_clusters)

# # # #     # Asignar etiquetas descriptivas
# # # #     cluster_map <- setNames(cluster_labels, as.character(1:n_clusters))
# # # #     data$ClusterLabel <- cluster_map[data$Cluster]

# # # #     library(RColorBrewer)

# # # #     # Paleta Okabe-Ito (hasta 10 colores, segura para todos los tipos de daltonismo)
# # # #     okabe_ito <- c(
# # # #     "#E69F00", "#56B4E9", "#009E73", "#F0E442",
# # # #     "#0072B2", "#D55E00", "#CC79A7", "#999999",
# # # #     "#000000", "#FFFFFF"
# # # #     )

# # # #     n_clusters <- length(cluster_labels)

# # # #     # Recortamos o expandimos si es necesario
# # # #     if (n_clusters > length(okabe_ito)) {
# # # #         palette_colors <- viridis::viridis(n_clusters, option = "D")
# # # #     } else {
# # # #         palette_colors <- okabe_ito[1:n_clusters]
# # # #     }

# # # #     names(palette_colors) <- cluster_labels

# # # #     # Asignar nombres (los labels descriptivos)
# # # #     names(palette_colors) <- cluster_labels

# # # #   sil <- silhouette(as.numeric(data$Cluster), data_distance_matrix)
# # # #   silhouette_score <- mean(sil[, 3])

# # # #   ##### Step 3: Generate evaluation plots #####
# # # # if(!is.numeric(manual_k)){
# # # #   eval_wcss_plot <- ggplot(data.frame(Clusters = 2:max_clusters, TotalWithinSS = total_withinss),
# # # #     aes(x = Clusters, y = TotalWithinSS)) +
# # # #     geom_line() +
# # # #     geom_point() +
# # # #     geom_label_repel(aes(label = ifelse(Clusters == optimal_k, paste0("Optimal: ", Clusters), ""))) +
# # # #     labs(title = "Cluster Evaluation: Within-Cluster Sum of Squares",
# # # #          x = "Number of Clusters", y = "Total WithinSS") +
# # # #     theme_minimal()

# # # #   eval_sil_plot <- ggplot(data.frame(Clusters = 2:max_clusters, SilhouetteScore = silhouette_scores),
# # # #     aes(x = Clusters, y = SilhouetteScore)) +
# # # #     geom_line() +
# # # #     geom_point() +
# # # #     geom_label_repel(aes(label = ifelse(Clusters == optimal_k_sil, paste0("Optimal: ", Clusters), ""))) +
# # # #     labs(title = "Cluster Evaluation: Silhouette Score",
# # # #          x = "Number of Clusters", y = "Mean Silhouette Score") +
# # # #     theme_minimal()

# # # #   eval_dunn_plot <- ggplot(data.frame(Clusters = 2:max_clusters, DunnIndex = dunn_indices),
# # # #     aes(x = Clusters, y = DunnIndex)) +
# # # #     geom_line() +
# # # #     geom_point() +
# # # #     geom_label_repel(aes(label = ifelse(Clusters == optimal_k_dunn, paste0("Optimal: ", Clusters), ""))) +
# # # #     labs(title = "Cluster Evaluation: Dunn Index",
# # # #          x = "Number of Clusters", y = "Dunn Index") +
# # # #     theme_minimal()

# # # #     path_wcss <- here::here(where_to_save, paste0(prefix, "_evaluation_plot_wcss.png"))

# # # #   ggsave(filename = path_wcss,
# # # #          plot = eval_wcss_plot, dpi = 600, width = 10, height = 8, units = "in")

# # # #     path_sil <- here::here(where_to_save, paste0(prefix, "_evaluation_plot_sil.png"))
# # # #   ggsave(filename = path_sil,
# # # #          plot = eval_sil_plot, dpi = 600, width = 10, height = 8, units = "in")

# # # #     path_dun <- here::here(where_to_save, paste0(prefix, "_evaluation_plot_dunn.png"))
# # # #   ggsave(filename = path_dun,
# # # #          plot = eval_dunn_plot, dpi = 600, width = 10, height = 8, units = "in")
# # # # } else {
    
# # # #     path_wcss <- NULL

# # # #     path_sil <- NULL

# # # #     path_dun <- NULL

# # # # }

# # # #   ##### Step 4: Dimensionality reduction and visualization #####

# # # #   path_pca <- here::here(where_to_save, paste0(prefix, "_pca"))

# # # #   data_pca_in <- add_pca_and_save_v3(
# # # #     data_combined_pc,
# # # #     all_scaled = FALSE,
# # # #     where_to_save = path_pca,
# # # #     list_variables = names(data_combined_pc %>% select(-Cluster)),
# # # #     environment_end = prefix,
# # # #     many_dim = 5,
# # # #     clusters_var = data$Cluster
# # # #   )

# # # #   data_fin <- cbind(data,
# # # #                     data_pca_in$data_all_pc %>% 
# # # #                         select(contains(prefix)))

# # # #   data_pca_in$data_all_pc <- NULL

# # # #   ##### Step 5: Cluster profiling #####

# # # #   data_character$Cluster <- data$Cluster

# # # #   list_num_desc <- generator_boxplot_by_list(data_fin, names(data_numeric), where_to_save, prefix)
# # # #   list_cat_desc <- generator_bar_stacked_plot(data_character, names(data_character), where_to_save, prefix)

# # # #   ##### Step 6: PCA scatter plot by cluster #####

# # # #   dim1 <- paste0(prefix, "_DIM_01")
# # # #   dim2 <- paste0(prefix, "_DIM_02")

# # # #   pca_cluster_plot <- ggplot(data_fin, aes(x = .data[[dim1]], y = .data[[dim2]], color = factor(ClusterLabel))) +
# # # #     geom_point(size = 3, alpha = 0.7) +
# # # #     labs(title = paste("K-Prototypes Clustering (Optimal K =", optimal_k, ")"),
# # # #          x = "PC 1", y = "PC 2", color = "Cluster") +
# # # #     theme_minimal()

# # # #   ggsave(here::here(where_to_save, paste0(prefix, "_kprototypes_cluster_plot.png")),
# # # #          plot = pca_cluster_plot, dpi = 600, width = 10, height = 8, units = "in")

# # # #   ##### Step 7: Geographic plot by cluster #####

# # # #   geo_path <- here::here(where_to_save, paste0(prefix, "_kprototypes"))
  
# # # #   list_geo_map_clust <- creating_map_separate_by_class(    
# # # #       manhica = manhica
# # # #     , health_facilities = health_facilities
# # # #     , data = data
# # # #     , var_to_split = "Cluster"
# # # #     , roads = roads
# # # #     , where_to_save = geo_path
# # # #     , pre_fix = paste0(prefix, "_kprototypes")
# # # #     , palette_colors = palette_colors
# # # #     , buffer_by_facilities = buffer_by_facilities
# # # #     , zona = zona
# # # #     , districts = districts)


# # # #   ##### Step 8: Modeling with clusters as variable #####

# # # #   plot_cluster <- mod_complete_sig_variables(
# # # #     data = data_fin,
# # # #     covariables = c("offset(log(fu_months))", "as.factor(gender)", "healthdist_final_std",
# # # #                     "as.factor(healthdist_name_facility)", "as.factor(year_birth)",
# # # #                     "mother_age_at_birth_of_child_std"),
# # # #     significative_variables = c("Cluster"),
# # # #     # significative_variables = c("Cluster"),
# # # #     set_model = "zero inflated",
# # # #     outcome = "h_visits",
# # # #     name_facility_in_order = TRUE,
# # # #     STD_variable = TRUE,
# # # #     add_decriptive = TRUE
# # # #   )

# # # #   ##### Step 9: Spider plot by domain #####

# # # #   # this is a parche, if you put something after this ,... no maternal education.. willl affect the results
# # # #   if(any(str_detect(names(data_dummies), "mother_education")) ){

# # # #     data_dummies <- data_dummies %>%
# # # #   mutate(`mother_education.No maternal education` = ifelse(
# # # #     (`mother_education.Primary` == 0 | is.na(`mother_education.Primary`)) &
# # # #     (`mother_education.Secondary or tertiary` == 0 | is.na(`mother_education.Secondary or tertiary`)), 
# # # #     1, 0
# # # #   )) %>%
# # # #   relocate(`mother_education.No maternal education`,
# # # #            .after = `mother_education.Secondary or tertiary`)
    
# # # # data_combined_pc <- data_combined_pc %>%
# # # #   mutate(`mother_education.No maternal education` = ifelse(
# # # #     (`mother_education.Primary` == 0 | is.na(`mother_education.Primary`)) &
# # # #     (`mother_education.Secondary or tertiary` == 0 | is.na(`mother_education.Secondary or tertiary`)), 
# # # #     1, 0
# # # #   )) %>%
# # # #   relocate(`mother_education.No maternal education`,
# # # #            .after = `mother_education.Secondary or tertiary`)
# # # #   }

# # # #   spiders_path <- plot_radar_by_env_and_cluster(
# # # #     data = data_combined_pc,
# # # #     names_cat = names(data_dummies),
# # # #     names_num = names(data_numeric),
# # # #     environ_list = list_entorno,
# # # #     where_to_save = where_to_save,
# # # #     prefix = prefix
# # # #   )

# # # #     spiders_path_all_together <- plot_radar_by_env_and_cluster_v2(
# # # #     data = data_combined_pc,
# # # #     names_cat = names(data_dummies),
# # # #     names_num = names(data_numeric),
# # # #     environ_list = list_entorno,
# # # #     where_to_save = where_to_save,
# # # #     prefix = prefix
# # # #   )

# # # #   ##### Step 10: Return all results #####

# # # #   return(list(
# # # #     best_model = best_model,
# # # #     optimal_k = optimal_k,
# # # #     evaluation_plot_path = path_wcss,
# # # #     evaluation_plot_path_sil = path_sil,
# # # #     evaluation_plot_path_dun = path_dun,
# # # #     clustering_plot_path = here::here(where_to_save, paste0(prefix, "_kprototypes_cluster_plot.png")),
# # # #     list_geo_map_clust_plot = list_geo_map_clust,
# # # #     list_num_desc = list_num_desc,
# # # #     list_cat_desc = list_cat_desc,
# # # #     plot_cluster = plot_cluster,
# # # #     pca_info = data_pca_in,
# # # #     cluster_of_data = data$Cluster,
# # # #     silhouette_score = silhouette_score,
# # # #     table_cluster = table_cluster,
# # # #     spiders_path = spiders_path,
# # # #     spiders_path_all_together = spiders_path_all_together
# # # #   ))
# # # # }


##' Creating random forest to see the important features, to select
##' 
##'
##'@title volcano from scratch
##'@param list_coef list from model execution on interative way
##'@param list_variables_interest ADD_2on_PARAMETER_DESCRIPTION
##'@param where_to_save ADD_3r_PARAMETER_DESCRIPTION
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'

forest_plot_generator_paper_2 <- function(
  data,
  environment_variables,
  covariables,
  output_variable,
  where_to_save
){

  dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)

  require(randomForest)


  data = data [, c(output_variable, covariables, environment_variables)]

  data$year_birth <- as.factor(data$year_birth)

  # Split data into training (70%) and testing (30%) sets
  set.seed(123)
  train_index <- sample(1:nrow(data), size = 0.8 * nrow(data))

  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]

  # Fit the Random Forest model
  rf_model <- randomForest(
    h_visits ~ .,             # Formula
    data = data,        # Training data
    ntree = 500,              # Number of trees
    importance = TRUE,        # Calculate variable importance
    mtry = 2                  # Number of variables tried at each split
  )


# Save the plot as a PNG file
png(here::here(where_to_save, "varImpPlot_rf_model.png"), width = 800, height = 600)
varImpPlot(rf_model) # Recreate the plot inside the `png` device
dev.off()


  list(
    random_forest = rf_model, 
    model_importance_plot1 = varImpPlot(rf_model),
    model_importance_plot_path = here::here(where_to_save, "varImpPlot_rf_model.png"),
    model_importance_plot2 = importance(rf_model)
  )

}

##' in this section will be created the volvano plot and
##' we will correct the p-values by BH from stats
##'
##'@title volcano from scratch
##'@param list_coef list from model execution on interative way
##'@param list_variables_interest ADD_2on_PARAMETER_DESCRIPTION
##'@param where_to_save ADD_3r_PARAMETER_DESCRIPTION
##'@param adjust_method c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
##' see more information from stats::p.adjust
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
volcano_from_scratch <- function(
    list_coef,
    list_variebles_interest,
    where_to_save,
    adjust_method = "BH"

    ){


    variable_name_to_plot <- load_variables_names_to_match()

  coef_table <- list_coef$coef_table

  coef_exposures <- coef_table  %>% 
  filter(
    stringr::str_detect(
      term,
      paste(list_variebles_interest, collapse = "|")
    )
  ) %>% left_join(
    variable_name_to_plot, 
    by = join_by("term" == "var_volcano")
  )



  coef_exposures$p_normal_value <- coef_exposures$`Pr(>|z|)`
  coef_exposures$p_normal_value_minlog10 <- -log10(coef_exposures$`Pr(>|z|)`)

  coef_exposures$p_corrected <- stats::p.adjust(coef_exposures$`Pr(>|z|)`, method = adjust_method)
  coef_exposures$p_corrected_minlog10 <- -log10(coef_exposures$p_corrected)

  coef_exposures$Lower <- coef_exposures$Estimate - qnorm(1 - 0.05 / 2) * pull(coef_exposures, "Std. Error")

  coef_exposures$Upper <- coef_exposures$Estimate + qnorm(1 - 0.05 / 2) * pull(coef_exposures, "Std. Error")


  coef_exposures$estima_exp <- exp(coef_exposures$Estimate)

  coef_exposures$beta_exp <- exp( coef_exposures$Estimate)

  coef_exposures$Lower_exp <- exp( coef_exposures$Lower)

  coef_exposures$Upper_exp <- exp( coef_exposures$Upper)

##########
##########
## coeficientes en data.frame
##########
##########

frame_to_save <- coef_exposures %>% filter(mod_info == "ZI count")

##########
##########
## puntos de corte
##########
##########

p_005_normal <-  0.05

p_bonf_cut <- 0.05 / length(list_variebles_interest)

label_cut_bonfe <- paste0("p = 0.05/", length(list_variebles_interest))

##########
##########
## ploting plots
##########
##########
  
plot <- ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_corrected_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_corrected < 0.05, `Short label`, "")),  # Label only if y > threshold
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  # Adjust padding for better readability
  ) +   # Add labels with repel to prevent overlap
  geom_hline(
    yintercept = -log10(0.05),  # Y-value for the horizontal line (adjust as needed)
    linetype = "dashed",  # Line type (dashed line)
    color = "grey",  # Line color
    size = 1  # Line size
  ) +   
  annotate(
    "text", x = 1.75, y = -log10(0.05), 
    label = "p = 0.05", color = "grey", size = 4, hjust = 1
  ) +  # Label for the first geom_hline
  # geom_hline(
  #   yintercept = -log10(p_bonf_cut),  # Y-value for the horizontal line (adjust as needed)
  #   linetype = "dashed",  # Line type (dashed line)
  #   color = "#0300ad",  # Line color
  #   size = 0.5  # Line size
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(p_bonf_cut), 
  #   label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 1
  # ) +  # Label for the second geom_hline
  geom_vline(
    xintercept = 1,  # X-value for the vertical line
    linetype = "dashed",  # Dashed line
    color = "red",  # Line color
    size = 1  # Line thickness
  ) +  # Label for the second geom_hline
  theme_light() +  # Clean theme
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)
  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2.jpeg"),  # Specify file name
  plot = plot,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)

writexl::write_xlsx(frame_to_save, here::here(where_to_save, "volcano_data.xlsx"))



plot22 <- ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_normal_value_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_normal_value < p_bonf_cut, `Short label`, "")),  # Label only if y > threshold
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  # Adjust padding for better readability
  ) +   # Add labels with repel to prevent overlap
  # geom_hline(
  #   yintercept = -log10(0.05),  # Y-value for the horizontal line (adjust as needed)
  #   linetype = "dashed",  # Line type (dashed line)
  #   color = "grey",  # Line color
  #   size = 1  # Line size
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(0.05), 
  #   label = "p = 0.05", color = "grey", size = 4, hjust = 1
  # ) +  # Label for the first geom_hline
  geom_hline(
    yintercept = -log10(p_bonf_cut),  # Y-value for the horizontal line (adjust as needed)
    linetype = "dashed",  # Line type (dashed line)
    color = "#0300ad",  # Line color
    size = 0.5  # Line size
  ) +   
  annotate(
    "text", x = 1.7, y = -log10(p_bonf_cut), 
    label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 5
  ) +  # Label for the second geom_hline
  geom_vline(
    xintercept = 1,  # X-value for the vertical line
    linetype = "dashed",  # Dashed line
    color = "red",  # Line color
    size = 1  # Line thickness
  ) +  # Label for the second geom_hline
  theme_light() +  # Clean theme
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

# plot22

dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)
  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2_bonferroni.jpeg"),  # Specify file name
  plot = plot22,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)

# writexl::write_xlsx(frame_to_save, here::here(where_to_save, "volcano_data2.xlsx"))
##########
##########
## plot 2 w ic var 
##########
##########


plot2 <-ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_corrected_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  geom_errorbarh(
    aes(xmin = Lower_exp, xmax = Upper_exp), 
    height = 0.1, color = "blue", alpha = 0.5
  ) +  # Add horizontal error bars for confidence intervals
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_corrected < 0.05, `Short label`, "")),  
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  
  ) +   
  geom_hline(
    yintercept = -log10(0.05),  
    linetype = "dashed",  
    color = "grey",  
    size = 1  
  ) +   
  annotate(
    "text", x = 1.75, y = -log10(0.05), 
    label = "p = 0.05", color = "grey", size = 4, hjust = 1
  ) +  
  # geom_hline(
  #   yintercept = -log10(p_bonf_cut),  
  #   linetype = "dashed",  
  #   color = "#0300ad",  
  #   size = 0.5  
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(p_bonf_cut), 
  #   label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 1
  # ) +  
  geom_vline(
    xintercept = 1,  
    linetype = "dashed",  
    color = "red",  
    size = 1  
  ) +  
  theme_light() +  
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2_v2.jpeg"),  # Specify file name
  plot = plot2,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)

# writexl::write_xlsx(frame_to_save, here::here(where_to_save, "volcano_data.xlsx"))

##########
##########
## plot 2 w ic var 
##########
##########


plot44 <-ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_normal_value_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  geom_errorbarh(
    aes(xmin = Lower_exp, xmax = Upper_exp), 
    height = 0.1, color = "blue", alpha = 0.5
  ) +  # Add horizontal error bars for confidence intervals
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_normal_value < p_bonf_cut, `Short label`, "")),  
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  
  ) +   
  # geom_hline(
  #   yintercept = -log10(p_bonf_cut),  
  #   linetype = "dashed",  
  #   color = "grey",  
  #   size = 1  
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(p_bonf_cut), 
  #   label = "p = 0.05", color = "grey", size = 4, hjust = 1
  # ) +  
  geom_hline(
    yintercept = -log10(p_bonf_cut),  
    linetype = "dashed",  
    color = "#0300ad",  
    size = 0.5  
  ) +   
  annotate(
    "text", x = 1.75, y = -log10(p_bonf_cut), 
    label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 1
  ) +  
  geom_vline(
    xintercept = 1,  
    linetype = "dashed",  
    color = "red",  
    size = 1  
  ) +  
  theme_light() +  
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2_v2_bonferroni.jpeg"),  # Specify file name
  plot = plot44,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)


########################################################################
########################################################################
#   significative variables to model
########################################################################
########################################################################

# # Remove the wrappers (scale(), as.factor(), etc.) and any trailing parts like ")Yes", ")Primary", etc.
# cleaned_variables <- gsub("^(scale\\(|as\\.factor\\()|\\).*$", "", variables)

vect_normal_cut <- frame_to_save %>% 
  filter(p_corrected <= p_005_normal) %>% 
  pull(., "term") %>% 
  unique %>%
  gsub("^(scale\\(|as\\.factor\\()|\\).*$", "", .)

vect_bonfer_cut <- frame_to_save %>% 
  filter(p_corrected <= p_bonf_cut) %>% 
  pull(., "term") %>% 
  unique %>%
  gsub("^(scale\\(|as\\.factor\\()|\\).*$", "", .)

########################################################################
########################################################################
#   to save on the report
########################################################################
########################################################################
list(
  volcano_excel = coef_exposures,
  volcano_plot = plot,
  volcano_plot_path = here::here(where_to_save, "volcano_plot2.jpeg"),
  volcano_plot_path2 = here::here(where_to_save, "volcano_plot2_v2.jpeg"),
  volcano_plot_path_bf = here::here(where_to_save, "volcano_plot2_bonferroni.jpeg"),
  volcano_plot_path2_bf = here::here(where_to_save, "volcano_plot2_v2_bonferroni.jpeg"),
  normal_cut_variables = vect_normal_cut,
  bonfer_cut_variables = vect_bonfer_cut
)

}

##' in this section will be created the volvano plot and
##' we will correct the p-values by BH from stats
##'
##'@title volcano from scratch
##'@param list_coef list from model execution on interative way
##'@param list_variables_interest ADD_2on_PARAMETER_DESCRIPTION
##'@param where_to_save ADD_3r_PARAMETER_DESCRIPTION
##'@param adjust_method c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
##' see more information from stats::p.adjust
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
volcano_from_scratch_RI_to_join <- function(
    list_coef,
    list_variebles_interest,
    where_to_save,
    adjust_method = "BH"

    ){


    variable_name_to_plot <- load_variables_names_to_match()

  coef_table <- list_coef$coef_table

  coef_exposures <- coef_table  %>% 
  filter(
    stringr::str_detect(
      term,
      paste(list_variebles_interest, collapse = "|")
    )
  ) %>% left_join(
    variable_name_to_plot, 
    by = join_by("term" == "var_volcano")
  )



  coef_exposures$p_normal_value <- coef_exposures$`Pr(>|z|)`
  coef_exposures$p_normal_value_minlog10 <- -log10(coef_exposures$`Pr(>|z|)`)

  coef_exposures$p_corrected <- stats::p.adjust(coef_exposures$`Pr(>|z|)`, method = adjust_method)
  coef_exposures$p_corrected_minlog10 <- -log10(coef_exposures$p_corrected)

  coef_exposures$Lower <- coef_exposures$Estimate - qnorm(1 - 0.05 / 2) * pull(coef_exposures, "Std. Error")

  coef_exposures$Upper <- coef_exposures$Estimate + qnorm(1 - 0.05 / 2) * pull(coef_exposures, "Std. Error")


  coef_exposures$estima_exp <- exp(coef_exposures$Estimate)

  coef_exposures$beta_exp <- exp( coef_exposures$Estimate)

  coef_exposures$Lower_exp <- exp( coef_exposures$Lower)

  coef_exposures$Upper_exp <- exp( coef_exposures$Upper)

##########
##########
## coeficientes en data.frame
##########
##########

frame_to_save <- coef_exposures %>% filter(mod_info == "HRD count")

##########
##########
## puntos de corte
##########
##########

p_005_normal <-  0.05

p_bonf_cut <- 0.05 / length(list_variebles_interest)

label_cut_bonfe <- paste0("p = 0.05/", length(list_variebles_interest))

##########
##########
## ploting plots
##########
##########
  
plot <- ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_corrected_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_corrected < 0.05, `Short label`, "")),  # Label only if y > threshold
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  # Adjust padding for better readability
  ) +   # Add labels with repel to prevent overlap
  geom_hline(
    yintercept = -log10(0.05),  # Y-value for the horizontal line (adjust as needed)
    linetype = "dashed",  # Line type (dashed line)
    color = "grey",  # Line color
    size = 1  # Line size
  ) +   
  annotate(
    "text", x = 1.75, y = -log10(0.05), 
    label = "p = 0.05", color = "grey", size = 4, hjust = 1
  ) +  # Label for the first geom_hline
  # geom_hline(
  #   yintercept = -log10(p_bonf_cut),  # Y-value for the horizontal line (adjust as needed)
  #   linetype = "dashed",  # Line type (dashed line)
  #   color = "#0300ad",  # Line color
  #   size = 0.5  # Line size
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(p_bonf_cut), 
  #   label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 1
  # ) +  # Label for the second geom_hline
  geom_vline(
    xintercept = 1,  # X-value for the vertical line
    linetype = "dashed",  # Dashed line
    color = "red",  # Line color
    size = 1  # Line thickness
  ) +  # Label for the second geom_hline
  theme_light() +  # Clean theme
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)
  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2.jpeg"),  # Specify file name
  plot = plot,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)

writexl::write_xlsx(frame_to_save, here::here(where_to_save, "volcano_data.xlsx"))



plot22 <- ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_normal_value_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_normal_value < p_bonf_cut, `Short label`, "")),  # Label only if y > threshold
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  # Adjust padding for better readability
  ) +   # Add labels with repel to prevent overlap
  # geom_hline(
  #   yintercept = -log10(0.05),  # Y-value for the horizontal line (adjust as needed)
  #   linetype = "dashed",  # Line type (dashed line)
  #   color = "grey",  # Line color
  #   size = 1  # Line size
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(0.05), 
  #   label = "p = 0.05", color = "grey", size = 4, hjust = 1
  # ) +  # Label for the first geom_hline
  geom_hline(
    yintercept = -log10(p_bonf_cut),  # Y-value for the horizontal line (adjust as needed)
    linetype = "dashed",  # Line type (dashed line)
    color = "#0300ad",  # Line color
    size = 0.5  # Line size
  ) +   
  annotate(
    "text", x = 1.7, y = -log10(p_bonf_cut), 
    label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 5
  ) +  # Label for the second geom_hline
  geom_vline(
    xintercept = 1,  # X-value for the vertical line
    linetype = "dashed",  # Dashed line
    color = "red",  # Line color
    size = 1  # Line thickness
  ) +  # Label for the second geom_hline
  theme_light() +  # Clean theme
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

# plot22

dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)
  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2_bonferroni.jpeg"),  # Specify file name
  plot = plot22,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)

# writexl::write_xlsx(frame_to_save, here::here(where_to_save, "volcano_data2.xlsx"))
##########
##########
## plot 2 w ic var 
##########
##########


plot2 <-ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_corrected_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  geom_errorbarh(
    aes(xmin = Lower_exp, xmax = Upper_exp), 
    height = 0.1, color = "blue", alpha = 0.5
  ) +  # Add horizontal error bars for confidence intervals
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_corrected < 0.05, `Short label`, "")),  
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  
  ) +   
  geom_hline(
    yintercept = -log10(0.05),  
    linetype = "dashed",  
    color = "grey",  
    size = 1  
  ) +   
  annotate(
    "text", x = 1.75, y = -log10(0.05), 
    label = "p = 0.05", color = "grey", size = 4, hjust = 1
  ) +  
  # geom_hline(
  #   yintercept = -log10(p_bonf_cut),  
  #   linetype = "dashed",  
  #   color = "#0300ad",  
  #   size = 0.5  
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(p_bonf_cut), 
  #   label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 1
  # ) +  
  geom_vline(
    xintercept = 1,  
    linetype = "dashed",  
    color = "red",  
    size = 1  
  ) +  
  theme_light() +  
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2_v2.jpeg"),  # Specify file name
  plot = plot2,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)

# writexl::write_xlsx(frame_to_save, here::here(where_to_save, "volcano_data.xlsx"))

##########
##########
## plot 2 w ic var 
##########
##########


plot44 <-ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_normal_value_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  geom_errorbarh(
    aes(xmin = Lower_exp, xmax = Upper_exp), 
    height = 0.1, color = "blue", alpha = 0.5
  ) +  # Add horizontal error bars for confidence intervals
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_normal_value < p_bonf_cut, `Short label`, "")),  
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  
  ) +   
  # geom_hline(
  #   yintercept = -log10(p_bonf_cut),  
  #   linetype = "dashed",  
  #   color = "grey",  
  #   size = 1  
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(p_bonf_cut), 
  #   label = "p = 0.05", color = "grey", size = 4, hjust = 1
  # ) +  
  geom_hline(
    yintercept = -log10(p_bonf_cut),  
    linetype = "dashed",  
    color = "#0300ad",  
    size = 0.5  
  ) +   
  annotate(
    "text", x = 1.75, y = -log10(p_bonf_cut), 
    label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 1
  ) +  
  geom_vline(
    xintercept = 1,  
    linetype = "dashed",  
    color = "red",  
    size = 1  
  ) +  
  theme_light() +  
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2_v2_bonferroni.jpeg"),  # Specify file name
  plot = plot44,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)


########################################################################
########################################################################
#   significative variables to model
########################################################################
########################################################################

# # Remove the wrappers (scale(), as.factor(), etc.) and any trailing parts like ")Yes", ")Primary", etc.
# cleaned_variables <- gsub("^(scale\\(|as\\.factor\\()|\\).*$", "", variables)

vect_normal_cut <- frame_to_save %>% 
  filter(p_corrected <= p_005_normal) %>% 
  pull(., "term") %>% 
  unique %>%
  gsub("^(scale\\(|as\\.factor\\()|\\).*$", "", .)

vect_bonfer_cut <- frame_to_save %>% 
  filter(p_corrected <= p_bonf_cut) %>% 
  pull(., "term") %>% 
  unique %>%
  gsub("^(scale\\(|as\\.factor\\()|\\).*$", "", .)

########################################################################
########################################################################
#   to save on the report
########################################################################
########################################################################
list(
  volcano_excel = coef_exposures,
  volcano_plot = plot,
  volcano_plot_path = here::here(where_to_save, "volcano_plot2.jpeg"),
  volcano_plot_path2 = here::here(where_to_save, "volcano_plot2_v2.jpeg"),
  volcano_plot_path_bf = here::here(where_to_save, "volcano_plot2_bonferroni.jpeg"),
  volcano_plot_path2_bf = here::here(where_to_save, "volcano_plot2_v2_bonferroni.jpeg"),
  normal_cut_variables = vect_normal_cut,
  bonfer_cut_variables = vect_bonfer_cut
)

}



##' in this section will be created the volvano plot and
##' we will correct the p-values by BH from stats
##'
##'@title volcano from scratch
##'@param list_coef list from model execution on interative way
##'@param list_variables_interest ADD_2on_PARAMETER_DESCRIPTION
##'@param where_to_save ADD_3r_PARAMETER_DESCRIPTION
##'@param adjust_method c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
##' see more information from stats::p.adjust
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
volcano_from_scratch2 <- function(
    list_coef,
    list_variebles_interest,
    where_to_save,
    adjust_method = "BH"

    ){


    variable_name_to_plot <- load_variables_names_to_match()

  coef_table <- list_coef$coef_table

  coef_exposures <- coef_table  %>% 
  filter(
    stringr::str_detect(
      term,
      paste(list_variebles_interest, collapse = "|")
    )
  ) %>% left_join(
    variable_name_to_plot, 
    by = join_by("term" == "var_volcano")
  )



#   coef_table <- list_coef$coef_table

#   coef_exposures <- coef_table  %>% 
#   filter(
#     stringr::str_detect(
#       term,
#       paste(list_variebles_interest, collapse = "|")
#     )
#   )

  coef_exposures$p_normal_value <- coef_exposures$`Pr(>|z|)`
  coef_exposures$p_normal_value_minlog10 <- -log10(coef_exposures$`Pr(>|z|)`)

  coef_exposures$p_corrected <- stats::p.adjust(coef_exposures$`Pr(>|z|)`, method = adjust_method)
  coef_exposures$p_corrected_minlog10 <- -log10(coef_exposures$p_corrected)

  coef_exposures$Lower <- coef_exposures$Estimate - qnorm(1 - 0.05 / 2) * pull(coef_exposures, "Std. Error")

  coef_exposures$Upper <- coef_exposures$Estimate + qnorm(1 - 0.05 / 2) * pull(coef_exposures, "Std. Error")


  coef_exposures$estima_exp <- exp(coef_exposures$Estimate)

  coef_exposures$beta_exp <- exp( coef_exposures$Estimate)

  coef_exposures$Lower_exp <- exp( coef_exposures$Lower)

  coef_exposures$Upper_exp <- exp( coef_exposures$Upper)

##########
##########
## coeficientes en data.frame
##########
##########

frame_to_save <- coef_exposures %>% filter(mod_info == "ZI count")

##########
##########
## puntos de corte
##########
##########

p_005_normal <-  0.05

p_bonf_cut <- 0.05 / length(list_variebles_interest)

label_cut_bonfe <- paste0("p = 0.05/", length(list_variebles_interest))

##########
##########
## ploting plots
##########
##########
  
plot <- ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_corrected_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_corrected < 0.05, `Short label`, "")),  # Label only if y > threshold
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  # Adjust padding for better readability
  ) +   # Add labels with repel to prevent overlap
  geom_hline(
    yintercept = -log10(0.05),  # Y-value for the horizontal line (adjust as needed)
    linetype = "dashed",  # Line type (dashed line)
    color = "grey",  # Line color
    size = 1  # Line size
  ) +   
  annotate(
    "text", x = 1.75, y = -log10(0.05), 
    label = "p = 0.05", color = "grey", size = 4, hjust = 1
  ) +  # Label for the first geom_hline
  # geom_hline(
  #   yintercept = -log10(p_bonf_cut),  # Y-value for the horizontal line (adjust as needed)
  #   linetype = "dashed",  # Line type (dashed line)
  #   color = "#0300ad",  # Line color
  #   size = 0.5  # Line size
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(p_bonf_cut), 
  #   label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 1
  # ) +  # Label for the second geom_hline
  geom_vline(
    xintercept = 1,  # X-value for the vertical line
    linetype = "dashed",  # Dashed line
    color = "red",  # Line color
    size = 1  # Line thickness
  ) +  # Label for the second geom_hline
  theme_light() +  # Clean theme
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)
  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2.jpeg"),  # Specify file name
  plot = plot,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)

writexl::write_xlsx(frame_to_save, here::here(where_to_save, "volcano_data.xlsx"))



plot22 <- ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_normal_value_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_normal_value < p_bonf_cut, `Short label`, "")),  # Label only if y > threshold
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  # Adjust padding for better readability
  ) +   # Add labels with repel to prevent overlap
  # geom_hline(
  #   yintercept = -log10(0.05),  # Y-value for the horizontal line (adjust as needed)
  #   linetype = "dashed",  # Line type (dashed line)
  #   color = "grey",  # Line color
  #   size = 1  # Line size
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(0.05), 
  #   label = "p = 0.05", color = "grey", size = 4, hjust = 1
  # ) +  # Label for the first geom_hline
  geom_hline(
    yintercept = -log10(p_bonf_cut),  # Y-value for the horizontal line (adjust as needed)
    linetype = "dashed",  # Line type (dashed line)
    color = "#0300ad",  # Line color
    size = 0.5  # Line size
  ) +   
  annotate(
    "text", x = 1.7, y = -log10(p_bonf_cut), 
    label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 5
  ) +  # Label for the second geom_hline
  geom_vline(
    xintercept = 1,  # X-value for the vertical line
    linetype = "dashed",  # Dashed line
    color = "red",  # Line color
    size = 1  # Line thickness
  ) +  # Label for the second geom_hline
  theme_light() +  # Clean theme
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

# plot22

dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)
  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2_bonferroni.jpeg"),  # Specify file name
  plot = plot22,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)

# writexl::write_xlsx(frame_to_save, here::here(where_to_save, "volcano_data2.xlsx"))
##########
##########
## plot 2 w ic var 
##########
##########


plot2 <-ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_corrected_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  geom_errorbarh(
    aes(xmin = Lower_exp, xmax = Upper_exp), 
    height = 0.1, color = "blue", alpha = 0.5
  ) +  # Add horizontal error bars for confidence intervals
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_corrected < 0.05, `Short label`, "")),  
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  
  ) +   
  geom_hline(
    yintercept = -log10(0.05),  
    linetype = "dashed",  
    color = "grey",  
    size = 1  
  ) +   
  annotate(
    "text", x = 1.75, y = -log10(0.05), 
    label = "p = 0.05", color = "grey", size = 4, hjust = 1
  ) +  
  # geom_hline(
  #   yintercept = -log10(p_bonf_cut),  
  #   linetype = "dashed",  
  #   color = "#0300ad",  
  #   size = 0.5  
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(p_bonf_cut), 
  #   label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 1
  # ) +  
  geom_vline(
    xintercept = 1,  
    linetype = "dashed",  
    color = "red",  
    size = 1  
  ) +  
  theme_light() +  
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2_v2.jpeg"),  # Specify file name
  plot = plot2,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)

# writexl::write_xlsx(frame_to_save, here::here(where_to_save, "volcano_data.xlsx"))

##########
##########
## plot 2 w ic var 
##########
##########


plot44 <-ggplot(frame_to_save, 
       aes(x = estima_exp, y = p_normal_value_minlog10)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  geom_errorbarh(
    aes(xmin = Lower_exp, xmax = Upper_exp), 
    height = 0.1, color = "blue", alpha = 0.5
  ) +  # Add horizontal error bars for confidence intervals
  ggrepel::geom_text_repel(
    aes(label = ifelse(p_normal_value < p_bonf_cut, `Short label`, "")),  
    vjust = -1, hjust = 0.5, box.padding = 0.35, point.padding = 0.5  
  ) +   
  # geom_hline(
  #   yintercept = -log10(p_bonf_cut),  
  #   linetype = "dashed",  
  #   color = "grey",  
  #   size = 1  
  # ) +   
  # annotate(
  #   "text", x = 1.75, y = -log10(p_bonf_cut), 
  #   label = "p = 0.05", color = "grey", size = 4, hjust = 1
  # ) +  
  geom_hline(
    yintercept = -log10(p_bonf_cut),  
    linetype = "dashed",  
    color = "#0300ad",  
    size = 0.5  
  ) +   
  annotate(
    "text", x = 1.75, y = -log10(p_bonf_cut), 
    label = label_cut_bonfe, color = "#0300ad", size = 4, hjust = 1
  ) +  
  geom_vline(
    xintercept = 1,  
    linetype = "dashed",  
    color = "red",  
    size = 1  
  ) +  
  theme_light() +  
  labs(
    title = "",
    x = "Exp(Estimate)",
    y = "-Log10(P)"
  )

  # Save the plot to a file in high resolution
ggsave(
  filename = here::here(where_to_save, "volcano_plot2_v2_bonferroni.jpeg"),  # Specify file name
  plot = plot44,               # Save the last plot
  dpi = 600,                        # Set resolution (dots per inch)
  width = 10,                       # Set width of the image (in inches)
  height = 10,                       # Set height of the image (in inches)
  units = "in"                      # Units for width and height
)


########################################################################
########################################################################
#   significative variables to model
########################################################################
########################################################################

# # Remove the wrappers (scale(), as.factor(), etc.) and any trailing parts like ")Yes", ")Primary", etc.
# cleaned_variables <- gsub("^(scale\\(|as\\.factor\\()|\\).*$", "", variables)

vect_normal_cut <- frame_to_save %>% 
  filter(p_corrected <= p_005_normal) %>% 
  pull(., "term") %>% 
  unique %>%
  gsub("^(scale\\(|as\\.factor\\()|\\).*$", "", .)

vect_bonfer_cut <- frame_to_save %>% 
  filter(p_normal_value <= p_bonf_cut) %>% 
  pull(., "term") %>% 
  unique %>%
  gsub("^(scale\\(|as\\.factor\\()|\\).*$", "", .)

########################################################################
########################################################################
#   to save on the report
########################################################################
########################################################################
list(
  volcano_excel = coef_exposures,
  volcano_plot = plot,
  volcano_plot_path = here::here(where_to_save, "volcano_plot2.jpeg"),
  volcano_plot_path2 = here::here(where_to_save, "volcano_plot2_v2.jpeg"),
  volcano_plot_path_bf = here::here(where_to_save, "volcano_plot2_bonferroni.jpeg"),
  volcano_plot_path2_bf = here::here(where_to_save, "volcano_plot2_v2_bonferroni.jpeg"),
  normal_cut_variables = vect_normal_cut,
  bonfer_cut_variables = vect_bonfer_cut
)

}


elastic_net_generator <- function(
    data,
    all_environment_variables,
    covariables,
    output_variable,
    where_to_save
    ){


    #-----------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------#
    #    debug part
    #-----------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------#

    #   data = tar_read(data_end_90_perce_distance_quantile_paper2)
    #   all_environment_variables = tar_read(all_environment_variables)
    #   covariables = c(
    #   #     "offset(log(fu_months))", 
    #       "as.factor(gender)",
    #       "healthdist_final_std",
    #       "as.factor(healthdist_name_facility)",
    #       "as.factor(year_birth)",
    #       "mother_age_at_birth_of_child_std"
    #   )
    #   output_variable = "h_visits"
    #   where_to_save = here::here(
    #     "outputs",
    #     "02_REPORT1")

    #-----------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------#
    #    end debug part
    #-----------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------#

      dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)

      require(glmnet)

      formula_to_x <- as.formula(
        paste0(
          output_variable,
          "~",
          paste(covariables, collapse = " + "),
          "+",
          paste(all_environment_variables, collapse = "+")
        )
      )

      x <- model.matrix(formula_to_x, data = data)[, -1]
        #   y <- data$h_visits # Variable dependiente
      y_offset <- data$h_visits / exp(log(data$fu_months))  # Equivalent to h_visits / fu_months


      # Ajustar modelo Elastic Net

      set.seed(4444) # Reproducibilidad

      cv_fit <- cv.glmnet(
        x, y_offset,
        alpha = 0.5,            # Combina Lasso y Ridge
        family = "poisson",     # Distribución para datos de conteo
        type.measure = "mse", # Usar devianza como métrica de evaluación
        nfolds = 10            # 10-fold Cross-Validation
      )

      plot_kfold <- plot(cv_fit)

      # Ajustar modelo final con el mejor lambda
      elastic_net_model <- glmnet(
        x, y_offset,
        alpha = 0.5,
        lambda = cv_fit$lambda.min,
        family = "poisson"
      )

      # Extract coefficients from the model
      coef_values <- as.matrix(coef(elastic_net_model))

      ##########
      ##########
      ## boots to get IC coef
      ##########
      ##########

      require(glmnet)
      require(boot)

      set.seed(4444)

      # Function to fit elastic net on bootstrap sample
      boot_fun <- function(indices, x, y, lambda_min) {
        x_boot <- x[indices, , drop = FALSE]  # Resample X
        y_boot <- y[indices]                  # Resample Y
        
        # Fit elastic net model
        model <- glmnet(x_boot, y_boot, alpha = 0.5, lambda = lambda_min, family = "poisson")
        
        # Convert sparse coefficients to dense vector
        as.vector(coef(model))
      }

      # Number of bootstrap samples
      R <- 1000

      # Run bootstrapping
      boot_coefs <- replicate(R, boot_fun(sample(1:nrow(x), replace = TRUE), x = x , y = y_offset, lambda_min = cv_fit$lambda.min))

      # Compute confidence intervals (95% percentile interval)
      ci_lower <- apply(boot_coefs, 1, quantile, probs = 0.025, na.rm = TRUE)
      ci_upper <- apply(boot_coefs, 1, quantile, probs = 0.975, na.rm = TRUE)


      # Combine results into a data frame
      # coef_ci <- data.frame(
      #   Estimate = coef_values,
      #   CI_Lower = ci_lower,
      #   CI_Upper = ci_upper
      # )

      # print(coef_ci)

    

    variable_name_to_plot <- load_variables_names_to_match()


      ##########
      ##########
      ## end
      ##########
      ##########

      coef_df <- data.frame(
        Variable = rownames(coef_values),
        Coefficient = coef_values[, 1],
        CI_Lower = ci_lower,
        CI_Upper = ci_upper
      )  %>% left_join(
        variable_name_to_plot, 
        by = join_by("Variable" == "elastic_names")
      )

      

      # Add a column to indicate whether the coefficient is zero or not
      coef_df$Selected <- coef_df$Coefficient != 0

      sig_variables <- coef_df %>% filter(Selected == TRUE) %>% pull(., "Variable")
      non_sig_variables <- coef_df %>% filter(Selected == FALSE) %>% pull(., "Variable")

      # Reorder the Variable factor based on the Coefficient values
    #   coef_df$Variable <- reorder(coef_df$Variable, coef_df$Coefficient)
      coef_df$`Long Label` <- reorder(coef_df$`Long Label`, coef_df$Coefficient)


      significative_variables <- stringr::str_extract(sig_variables, paste(all_environment_variables, collapse = "|")) %>% na.omit(.) %>% as.vector(.) %>% unique()
      non_significative <- stringr::str_extract(non_sig_variables, paste(all_environment_variables, collapse = "|")) %>% na.omit(.) %>% as.vector(.) %>% unique()

      # Plot the coefficients
      plot_to_show_elastic <- ggplot(coef_df, aes(x = .data[["Long Label"]], y = Coefficient)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(
          title = "Variable Coefficients",
          x = "Variables",
          y = "Coefficient Value"
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(
            color = ifelse(coef_df$Selected[order(coef_df$Coefficient)], "black", "red") # Match label colors to sorted values
          )
        )

        # Save the plot to a file in high resolution
      ggsave(
        filename = here::here(where_to_save, "Elastic_net_plott.png"),  # Specify file name
        plot = plot_to_show_elastic,               # Save the last plot
        dpi = 600,                        # Set resolution (dots per inch)
        width = 15,                       # Set width of the image (in inches)
        height = 12,                       # Set height of the image (in inches)
        units = "in"                      # Units for width and height
      )

      list(
        elastic_coef_table = coef_df,
        elastic_k_fold_plot = plot_kfold,
        elastic_plot_coef = plot_to_show_elastic,
        elastic_plot_coef_path = here::here(where_to_save, "Elastic_net_plott.png"),
        list_significative_variables = significative_variables,
        list_non_significative_variables = non_significative
      )



    }

# select_best_alpha_elasticnet <- function(x, y, offset_var = NULL, 
#                                          family = "poisson", 
#                                          penalty.factor = NULL, 
#                                          alphas = seq(0.1, 1, by = 0.1),
#                                          nfolds = 10,
#                                          seed = 1234) {
#   require(glmnet)
#   require(ggplot2)
# #   require(ggplotify)
#   require(gridExtra)

#   if (is.null(penalty.factor)) {
#     penalty.factor <- rep(1, ncol(x))
#   }

#   cv_results <- list()
#   errors <- c()

#   for (a in alphas) {
#     set.seed(seed)
#     cv <- cv.glmnet(
#       x = x,
#       y = y,
#       offset = offset_var,
#       alpha = a,
#       family = family,
#       penalty.factor = penalty.factor,
#       nfolds = nfolds,
#       type.measure = "deviance"
#     )

#     cv_results[[as.character(a)]] <- cv
#     errors <- c(errors, min(cv$cvm))
#   }

#   best_alpha <- alphas[which.min(errors)]
#   best_cv <- cv_results[[as.character(best_alpha)]]

#   # Final model with best alpha and lambda.min
#   final_model <- glmnet(
#     x = x,
#     y = y,
#     offset = offset_var,
#     alpha = best_alpha,
#     family = family,
#     lambda = best_cv$lambda.min,
#     penalty.factor = penalty.factor
#   )

#   # Plot: CV error vs alpha
#   df_alpha <- data.frame(alpha = alphas, cv_error = errors)
#   plot_alpha_error <- ggplot(df_alpha, aes(x = alpha, y = cv_error)) +
#     geom_line() + geom_point() +
#     geom_vline(xintercept = best_alpha, linetype = "dashed", color = "red") +
#     labs(title = "Cross-validated deviance vs alpha",
#          x = "Alpha", y = "CV Deviance") +
#     theme_minimal()

#   # Plot: Coefficients vs log(lambda), captured as ggplot object
# #   coef_plot_obj <- as.ggplot(~{
# #     plot(best_cv$glmnet.fit, xvar = "lambda", label = TRUE)
# #     abline(v = log(best_cv$lambda.min), col = "red", lty = 2)
# #     abline(v = log(best_cv$lambda.1se), col = "blue", lty = 2)
# #     legend("topright", legend = c("lambda.min", "lambda.1se"),
# #            col = c("red", "blue"), lty = 2, bty = "n")
# #   })

#   return(list(
#     best_alpha = best_alpha,
#     lambda_min = best_cv$lambda.min,
#     lambda_1se = best_cv$lambda.1se,
#     cv_fit = best_cv,
#     final_model = final_model,
#     plot_alpha_error = plot_alpha_error #,
#     # plot_lambda_path = coef_plot_obj
#   ))
# }
select_best_alpha_elasticnet <- function(x, y, offset_var = NULL, 
                                         family = "poisson", 
                                         penalty.factor = NULL, 
                                         alphas = seq(0.1, 1, by = 0.1),
                                         nfolds = 10,
                                         seed = 4444) {
  require(glmnet)
  require(ggplot2)
  require(dplyr)

  if (is.null(penalty.factor)) {
    penalty.factor <- rep(1, ncol(x))
  }

  cv_results <- list()
  errors <- c()
  detailed_errors <- data.frame()

  for (a in alphas) {
    set.seed(seed)
    cv <- cv.glmnet(
      x = x,
      y = y,
      offset = offset_var,
      alpha = a,
      family = family,
      penalty.factor = penalty.factor,
      nfolds = nfolds,
      type.measure = "deviance"
    )

    cv_results[[as.character(a)]] <- cv
    errors <- c(errors, min(cv$cvm))

    detailed_errors <- rbind(detailed_errors,
      data.frame(
        alpha = a,
        lambda = cv$lambda,
        deviance = cv$cvm,
        se = cv$cvsd
      )
    )
  }

  best_alpha <- alphas[which.min(errors)]
  best_cv <- cv_results[[as.character(best_alpha)]]

  # Final model
  final_model <- glmnet(
    x = x,
    y = y,
    offset = offset_var,
    alpha = best_alpha,
    family = family,
    lambda = best_cv$lambda.min,
    penalty.factor = penalty.factor
  )

  # Lambda values
  lambda_min <- best_cv$lambda.min
  lambda_1se <- best_cv$lambda.1se
  lambda_avg <- (lambda_min + lambda_1se) / 2

  # Plot 1: CV deviance vs alpha
  df_alpha <- data.frame(alpha = alphas, cv_error = errors)
  plot_alpha_error <- ggplot(df_alpha, aes(x = alpha, y = cv_error)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = best_alpha, linetype = "dashed", color = "red") +
    labs(title = "CV Deviance vs Alpha", x = "Alpha", y = "CV Deviance") +
    theme_minimal()

  # Plot 2: Number of non-zero coefficients vs log(lambda)
  model_path <- best_cv$glmnet.fit
  df_lambda <- data.frame(
    lambda = model_path$lambda,
    n_nonzero = model_path$df
  )

  plot_nvars_lambda <- ggplot(df_lambda, aes(x = log(lambda), y = n_nonzero)) +
    geom_line(color = "steelblue") +
    geom_vline(xintercept = log(lambda_min), linetype = "dashed", color = "red") +
    geom_vline(xintercept = log(lambda_1se), linetype = "dashed", color = "blue") +
    geom_vline(xintercept = log(lambda_avg), linetype = "dotted", color = "purple") +
    labs(title = "Number of Variables vs log(Lambda)",
         x = "log(Lambda)", y = "Number of Non-Zero Coefficients") +
    theme_minimal()

  return(list(
    best_alpha = best_alpha,
    lambda_min = lambda_min,
    lambda_1se = lambda_1se,
    lambda_avg = lambda_avg,
    cv_fit = best_cv,
    final_model = final_model,
    plot_alpha_error = plot_alpha_error,
    plot_nvars_lambda = plot_nvars_lambda
  ))
}



# Crear el vector de nombres que no serán penalizados
get_no_penalizar_vars <- function(covariables, x_matrix) {
  all_vars <- colnames(x_matrix)

  no_penalizar_patterns <- covariables[!grepl("^offset\\(", covariables)]  # excluir offset si está
  no_penalizar_patterns <- gsub("as.factor\\((.*?)\\)", "\\1", no_penalizar_patterns)  # extraer nombre interno

  # Buscar columnas que contienen esos patrones
  no_penalizar <- unlist(lapply(no_penalizar_patterns, function(pattern) {
    grep(pattern, all_vars, value = TRUE)
  }))

  unique(no_penalizar)
}

plot_coef_path <- function(cv_fit, save_path = NULL) {
  if (!is.null(save_path)) {
    png(save_path, width = 800, height = 600)
  }

  plot(cv_fit$glmnet.fit,
       xvar = "lambda",
       label = FALSE,
       main = "Coefficient Paths vs log(Lambda)")
  
  abline(v = log(cv_fit$lambda.min), col = "red", lty = 2)
  abline(v = log(cv_fit$lambda.1se), col = "blue", lty = 2)
  abline(v = log((cv_fit$lambda.min + cv_fit$lambda.1se) / 2), col = "#00ffc8", lty = 2)

  legend("topright", legend = c("lambda.min", "lambda.1se", "avg(min, 1se)"),
         col = c("red", "blue", "#00ffc8"), lty = 2, bty = "n")

  if (!is.null(save_path)) {
    dev.off()
  }
}


# Ejemplo de uso:
elastic_net_generator_w_penalty <- function(
    data,
    all_environment_variables,
    covariables,
    output_variable,
    where_to_save,
    offset_var = "fu_months",
    penalty_vector = NULL,
    kfold_n = 10 # si metemeos que kflod = nrow(x) entonces haremos una LOOCV
    ){


    #-----------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------#
    #    debug part
    #-----------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------#

    #   data = tar_read(data_end_90_perce_distance_quantile_paper2)
    #   all_environment_variables = tar_read(all_environment_variables)
    #   covariables = c(
    #       "offset(log(fu_months))", 
    #       "as.factor(gender)",
    #       "healthdist_final_std",
    #       "as.factor(healthdist_name_facility)",
    #       "as.factor(year_birth)",
    #       "mother_age_at_birth_of_child_std"
    #   )
    #   output_variable = "h_visits"
    #   where_to_save = here::here(
    #     "outputs",
    #     "02_REPORT1")
    
    # offset_var = "fu_months"
    # penalty_vector = TRUE # NULL if want all the variables are equally penalized
    # kfold_n = 10 # si metemeos que kflod = nrow(x) entonces haremos una LOOCV - creo que kfold = 10 es valido


    #-----------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------#
    #    end debug part
    #-----------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------#

      dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)

      require(glmnet)

      formula_to_x <- as.formula(
        paste0(
          output_variable,
          "~",
          paste(covariables, collapse = " + "),
          "+",
          paste(all_environment_variables, collapse = "+")
        )
      )

      x <- model.matrix(formula_to_x, data = data)[, -1]
        #   y <- data$h_visits # Variable dependiente
      y_var <- data$h_visits 

      offset_var <-  log(data$fu_months)  # Equivalent to h_visits / fu_months

        # penalty vector 
        # Penalización: no penalizar var1 y penalizar poco var2

        penalty_vector <- rep(1, ncol(x))
        names(penalty_vector) <- colnames(x)

      if(!is.null(penalty_vector)){

        # Variables que NO penalizás:
        no_penalizar <- get_no_penalizar_vars(covariables, x)

        penalty_vector[no_penalizar] <- 0

      } 

        
        

      # Ajustar modelo Elastic Net

        result_best_cv_parameters <- select_best_alpha_elasticnet(
        x = x,
        y = y_var,
        offset_var = offset_var,
        # penalty.factor = penalty_vector  # o NULL si no usás
        penalty.factor = penalty_vector,  # o NULL si no usás
        nfolds = kfold_n
        )

        # best_alpha = best_alpha,
        # lambda_min = best_cv$lambda.min,
        # lambda_1se = best_cv$lambda.1se,
        # cv_fit = best_cv,
        # final_model = final_model,
        # plot_alpha_error = plot_alpha_error,
        # plot_lambda_path = coef_plot_obj

        # lambda_selected = ( result_best_cv_parameters$lambda_min + result_best_cv_parameters$lambda_1se ) / 2
        # lambda_selected = result_best_cv_parameters$lambda_min
        # lambda_selected = result_best_cv_parameters$lambda_1se

        lambda_min <- result_best_cv_parameters$lambda_min
        lambda_1se <- result_best_cv_parameters$lambda_1se

        # lambda_selected <- mean(c(lambda_min, lambda_1se))
        # lambda_selected <- mean(c(lambda_min, lambda_1se))
        # lambda_selected <-exp(-2)

        lambda_selected <-lambda_1se

        # log(median(c(lambda_min, lambda_1se)))

        # mean(c(lambda_min, lambda_1se))

        # exp(-3)

        # log(lambda_min)
        # log(lambda_min)


        # alpha_selected = 0.25
        alpha_selected = result_best_cv_parameters$best_alpha

      # Ajustar modelo Elastic Net

      set.seed(4444) # Reproducibilidad

      cv_fit <- cv.glmnet(
        x, y_var,
        offset = offset_var,
         penalty.factor = penalty_vector,
        alpha = alpha_selected,            # Combina Lasso y Ridge
        family = "poisson",     # Distribución para datos de conteo
        type.measure = "deviance", # Usar devianza como métrica de evaluación
        nfolds = kfold_n            # 10-fold Cross-Validation
      )

      plot_kfold <- plot(cv_fit)

      coef_plot_path <- here::here(where_to_save, "Elastic_net_lambda_path.png")
      

      plot_n_vars_lambda <- plot_coef_path(cv_fit, save_path = coef_plot_path)


      # Ajustar modelo final con el mejor lambda
      elastic_net_model <- glmnet(
        x, y_var,
        offset = offset_var,
         penalty.factor = penalty_vector,
        alpha = alpha_selected,
        lambda = lambda_selected,
        family = "poisson"
      )

      # Extract coefficients from the model
      coef_values <- as.matrix(coef(elastic_net_model))

      ##########
      ##########
      ## boots to get IC coef
      ##########
      ##########

      require(glmnet)
      require(boot)

      set.seed(4444)

      # Function to fit elastic net on bootstrap sample
      boot_fun <- function(indices, x, y, offset_var, penalty_vector, lambda_selected, alpha_selected) {
        x_boot <- x[indices, , drop = FALSE]  # Resample X
        y_boot <- y[indices]                  # Resample Y
        offset_boot <- offset_var[indices]
        
        # Fit elastic net model
        model <- glmnet( x_boot, 
                         y_boot, 
                         alpha = alpha_selected, 
                         lambda = lambda_selected, 
                         family = "poisson", 
                         offset = offset_boot, 
                         penalty.factor = penalty_vector)
  
        
        # Convert sparse coefficients to dense vector
        as.vector(coef(model))
      }

      # Number of bootstrap samples
      R <- 1000

      # Run bootstrapping
      boot_coefs <- replicate(R, 
                              boot_fun(sample(1:nrow(x), replace = TRUE), 
                                       x = x , 
                                       y = y_var, 
                                       offset_var = offset_var, 
                                       penalty_vector = penalty_vector, 
                                       lambda_selected = lambda_selected, 
                                       alpha_selected = alpha_selected))

      # Compute confidence intervals (95% percentile interval)
      ci_lower <- apply(boot_coefs, 1, quantile, probs = 0.025, na.rm = TRUE)
      ci_upper <- apply(boot_coefs, 1, quantile, probs = 0.975, na.rm = TRUE)


      # Combine results into a data frame
      # coef_ci <- data.frame(
      #   Estimate = coef_values,
      #   CI_Lower = ci_lower,
      #   CI_Upper = ci_upper
      # )

      # print(coef_ci)

    

    variable_name_to_plot <- load_variables_names_to_match()


      ##########
      ##########
      ## end
      ##########
      ##########

      coef_df <- data.frame(
        Variable = rownames(coef_values),
        Coefficient = coef_values[, 1],
        CI_Lower = ci_lower,
        CI_Upper = ci_upper
      )  %>% left_join(
        variable_name_to_plot, 
        by = join_by("Variable" == "elastic_names")
      )

      

      # Add a column to indicate whether the coefficient is zero or not
      coef_df$Selected <- coef_df$Coefficient != 0

      sig_variables <- coef_df %>% filter(Selected == TRUE) %>% pull(., "Variable")
      non_sig_variables <- coef_df %>% filter(Selected == FALSE) %>% pull(., "Variable")

      # Reorder the Variable factor based on the Coefficient values
    #   coef_df$Variable <- reorder(coef_df$Variable, coef_df$Coefficient)
      coef_df$`Long Label` <- reorder(coef_df$`Long Label`, coef_df$Coefficient)


      significative_variables <- stringr::str_extract(sig_variables, paste(all_environment_variables, collapse = "|")) %>% na.omit(.) %>% as.vector(.) %>% unique()
      non_significative <- stringr::str_extract(non_sig_variables, paste(all_environment_variables, collapse = "|")) %>% na.omit(.) %>% as.vector(.) %>% unique()

      # Plot the coefficients
      plot_to_show_elastic <- ggplot(coef_df, aes(x = .data[["Long Label"]], y = Coefficient)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(
          title = "Variable Coefficients",
          x = "Variables",
          y = "Coefficient Value"
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(
            color = ifelse(coef_df$Selected[order(coef_df$Coefficient)], "black", "red") # Match label colors to sorted values
          )
        )

        # Save the plot to a file in high resolution
      ggsave(
        filename = here::here(where_to_save, "Elastic_net_plott.png"),  # Specify file name
        plot = plot_to_show_elastic,               # Save the last plot
        dpi = 600,                        # Set resolution (dots per inch)
        width = 15,                       # Set width of the image (in inches)
        height = 12,                       # Set height of the image (in inches)
        units = "in"                      # Units for width and height
      )

      list(
        elastic_coef_table = coef_df,
        elastic_k_fold_plot = plot_kfold,
        elastic_plot_coef = plot_to_show_elastic,
        elastic_plot_coef_path = here::here(where_to_save, "Elastic_net_plott.png"),
        list_significative_variables = significative_variables,
        list_non_significative_variables = non_significative,
        result_optimization = result_best_cv_parameters,
        plot_n_vars_lambda = coef_plot_path,
        alpha_used = alpha_selected,
        lambda_used = lambda_selected,
        kfold_n = kfold_n
      )



    }

elastic_net_glmmPen <- function(
  data,
  all_environment_variables,
  covariables,                 # SIN incluir offset() aquí
  output_variable,             # p.ej. "h_visits"
  group_var      = "house_number",
  offset_var     = "fu_months",
  where_to_save,
  alpha_grid     = c(0.9, 0.7, 0.5, 0.3),  # mezcla L1/L2 (ENet-like)
  penalty        = "lasso",   # alternativas: "MCP", "SCAD"
  nlambda        = 20,        # nº de lambdas por rejilla
  search_type    = "abbrev",  # búsqueda abreviada λ (rápida)
  bic_option     = "BICq",    # criterio para escoger modelo
  penalty_vector = NULL       # opcional: nombres-> 0/1 (0=no penalizar)
){

  set.seed(4444) # Reproducibilidad

  dir.create(where_to_save, showWarnings = FALSE, recursive = TRUE)
  stopifnot(all(c(output_variable, group_var, offset_var) %in% names(data)))

  # --- construir fórmula de FIXED + RANDOM (sin offset en la fórmula)
  cov_no_offset <- covariables[!grepl("^offset\\(", covariables)]
  rhs <- paste(c(cov_no_offset, all_environment_variables), collapse = " + ")
  form_str <- sprintf("%s ~ %s + (1 | %s)", output_variable, rhs, group_var)
  form <- as.formula(form_str)

  # --- offset (log tiempo de seguimiento) como VECTOR
  off_vec <- log(data[[offset_var]])  # misma longitud que y

  # --- vector de no-penalización (0/1) alineado con X de FIXED (sin intercepto)
  X_fixed <- model.matrix(as.formula(paste("~", rhs)), data = data)[, -1, drop = FALSE]
  fixef_names <- colnames(X_fixed)

  if (is.null(penalty_vector)) {
    # por defecto: NO penalizar 'covariables' (confusores); SÍ penalizar ambientales
    no_penalizar_idx <- Reduce(`|`, lapply(cov_no_offset, function(tt) grepl(paste0("^", gsub("([\\[\\]\\^\\$\\.\\|\\(\\)\\?\\*\\+\\{\\}\\\\])","\\\\\\1", tt),"),"), fixef_names)))
    fixef_noPen <- ifelse(no_penalizar_idx, 0L, 1L)
  } else {
    # si te pasan un penalty.factor (0/1 por columna), lo mapeamos a 0/1 esperado por glmmPen
    pf <-  fixef_names %in% penalty_vector

    fixef_noPen <-as.numeric(!pf) # 0 = no penalizar
  }



  # --- control de la búsqueda de lambdas y de la optimización
  sel_ctrl   <- glmmPen::selectControl(nlambda = nlambda, search = search_type, BIC_option = bic_option)
  optim_ctrl <- glmmPen::optimControl(var_restrictions = "fixef", standardization = TRUE)

  best_fit   <- NULL
  best_bic   <- Inf
  best_alpha <- NA_real_

  for (a in alpha_grid) {
    fit <- glmmPen::glmmPen(
      formula        = form,
      data           = data,
      family         = poisson(),              # Poisson canon. (no NB ni ZI)
      offset         = off_vec,                # offset fuera de la fórmula
      fixef_noPen    = fixef_noPen,            # 0 no penaliza, 1 sí penaliza
      penalty        = penalty,                # "lasso" + alpha -> ENet-like
      alpha          = a,                      # mezcla L1/L2 (Mnet)
      optim_options  = optim_ctrl,
      tuning_options = sel_ctrl,
      progress       = TRUE
    )
    b <- BIC(fit)
    if (is.finite(b) && b < best_bic) {
      best_bic <- b; best_fit <- fit; best_alpha <- a
    }
  }

  # --- extraer coeficientes y selección
  beta  <- fixef(best_fit)     # incluye intercepto
  beta  <- beta[!names(beta) %in% "(Intercept)"]
  sel   <- beta != 0
  coef_df <- data.frame(
    Variable    = names(beta),
    Coefficient = as.numeric(beta),
    Selected    = as.logical(sel),
    row.names   = NULL
  )

  # variables ambientales elegidas
  sel_env <- unique(na.omit(stringr::str_extract(coef_df$Variable[coef_df$Selected], 
                                                 paste(all_environment_variables, collapse = "|"))))

  # estado del RE por casa
  Sigma <- sigma(best_fit)     # matriz de varianza RE; revisa >0 para (1|house)
  re_ok <- any(diag(Sigma) > 0)

  list(
    fit                    = best_fit,
    alpha_used             = best_alpha,
    bic_best               = best_bic,
    fixef_table            = coef_df,
    selected_env_variables = sel_env,
    random_intercept_kept  = re_ok,
    formula_used           = form_str,
    offset_used            = sprintf("log(%s)", offset_var),
    notes = "Usa Poisson + (1|house) con penalización tipo ENet vía alpha. Úsalo para selección; re-ajusta ZINB mixto en glmmTMB con las seleccionadas."
  )
 }
##'add function description
##'
##'
##'@title ADD_YOUR_FUNCTION_TITLE
##'@param data1 ADD_1rt_PARAMETER_DESCRIPTION
##'@param data2 ADD_2on_PARAMETER_DESCRIPTION
##'@param variable ADD_3r_PARAMETER_DESCRIPTION
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
benjamini_hochberg_calculation <- function(pvalues){
# pvalues<-c(0.01,0.001, 0.05, 0.20, 0.15, 0.15)


# ranks<-rank(pvalues, ties.method = "last")

# p_m_over_k<-pvalues*length(pvalues)/ranks


# # for (r in length(pvalues):1) {
# #   print(p_m_over_k[ranks>=r])
# # }


# pvalues_adj<-c()

# for (i in 1:length(pvalues)) {
  
#   # find the rank
#   tmp_rank<-ranks[i]
  
#   # get all the p_m_over_k that are greater or equal to this rank
#   # and get the min value
#   pvalues_adj<-c(pvalues_adj, min(1,min(p_m_over_k[ranks>=tmp_rank])))
# }

 stats::p.adjust(pvalues, method="BH")
 


}


##'add function description
##'
##'
##'@title ADD_YOUR_FUNCTION_TITLE
##'@param data1 ADD_1rt_PARAMETER_DESCRIPTION
##'@param data2 ADD_2on_PARAMETER_DESCRIPTION
##'@param variable ADD_3r_PARAMETER_DESCRIPTION
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
get_base_group_ilumintaion_source <- function(data, base){
  td <- data %>%
  filter(
    ilumination_fuel_compl == "Don't know"
  ) %>%
  pull(
    perm_id
  )

grouped_il <- data %>%
  filter(
    ilumination_fuel_compl == "Don't know"
  ) %>% select(perm_id, ilumination_fuel)

solution <- base %>%
      group_by(perm_id) %>%
  mutate(flag_btw_dates_no = all(check_sensi != "between_dates"),
        min_sensitive_survey = min(check_sensi_numeric)) %>%
  filter(
    perm_id %in% td,
     flag_btw_dates_no |
      check_sensi == "between_dates",
      check_sensi != "no_surveys_on_period",
      check_sensi_numeric == min_sensitive_survey
  ) %>%
  select(
    perm_id, 
    ilumination_fuel_compl,
    start,
    check_sensi,
    start_fu_date,
    end_fu_date
  ) %>%
  group_by(perm_id) %>%
  summarise(
    ilumination_fuel_compl_s1 = paste(ilumination_fuel_compl, collapse = ", "),
    start_s1 = paste(start, collapse = ", "),
    start_fu_s1 = paste(start_fu_date, collapse = ", "),
    end_fu_s1 = paste(end_fu_date, collapse = ", "),
    check_sensitive_s1 = paste(check_sensi, collapse = ", ")
  )


  solution %>% 
  left_join(
    grouped_il,
    by = "perm_id"
  )
}


##'add function description
##'
##'
##'@title subseting_by_limit_geo
##'@param data1 ADD_1rt_PARAMETER_DESCRIPTION
##'@param data2 ADD_2on_PARAMETER_DESCRIPTION
##'@param variable ADD_3r_PARAMETER_DESCRIPTION
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
subseting_by_limit_geo <- function(data, limit, health_principal_vector){
   data %>%
  left_join(
    limit %>%
    filter(`get(group_field)` %in% health_principal_vector),
    by = join_by("healthdist_name_facility" == "get(group_field)") # healthdist_name_facility_all7, healthdist_name_facility
  ) %>%
  mutate(
    health_distance_limit = ifelse(
      is.na(health_distance_limit), 0, health_distance_limit
    )
  ) %>%
  filter(healthdist <= health_distance_limit) %>% as.data.frame()
}

##'add function description
##'
##'
##'@title filter_migration_ilumination_ocupation_paper_1
##'@param data1 ADD_1rt_PARAMETER_DESCRIPTION
##'@param data2 ADD_2on_PARAMETER_DESCRIPTION
##'@param variable ADD_3r_PARAMETER_DESCRIPTION
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
filter_migration_ilumination_ocupation_paper_1 <- function(data){

  data %>%
    filter(
      ilumination_fuel != "Don't know"
    )


    # %>%
    # filter(
    #    !(flag_migration)
       
    # ) 

}

##'add function description
##'
##'
##'@title ADD_YOUR_FUNCTION_TITLE
##'@param data1 ADD_1rt_PARAMETER_DESCRIPTION
##'@param data2 ADD_2on_PARAMETER_DESCRIPTION
##'@param variable ADD_3r_PARAMETER_DESCRIPTION
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
treat_pre_model_data <- function(data){

# Family instability: 1) Death of mother, 
# 2) Death of father, 3) Death of sibling, 
# 4) Living without mother in household, 
# 5) Living without father in household.

# [118] "father_alive_FIMCA"
# [119] "mother_alive_FIMCA"
# [120] "father_lives_here_FIMCA"
# [121] "mother_lives_here_FIMCA"
# [122] "sibling_death_FIMCA"


  dd <- data  %>%
    filter(
        has_tv_mca != "Doesn't know"
        , has_radio_mca != "Doesn't know"
        , has_dvd_mca != "Doesn't know"
        , has_farm_of_commercial_production_mca != "Doesn't know"
        
    ) %>% 
    mutate(
      death_of_mother = ifelse(mother_alive_FIMCA == "yes", "no", "yes"),
      death_of_father = ifelse(father_alive_FIMCA == "yes", "no", "yes"),
      living_without_mother_in_household = ifelse(mother_lives_here_FIMCA == "yes", "no", "yes"),
      living_without_father_in_household = ifelse(father_lives_here_FIMCA == "yes", "no", "yes"),
      death_of_sibling = sibling_death_FIMCA
    )
  
      # death_of_mother 
      # death_of_father 
      # living_without_mother_in_household 
      # living_without_father_in_household
      # death_of_sibling

  dd

}


# [67] "healthdist_all7"
# [68] "healthdist_name_facility_all7"
# [69] "healthdist_name_grp_all7"
# [70] "healthdist_name_Jgrp_all7"

##'add function description
##'
##'
##'@title ADD_YOUR_FUNCTION_TITLE
##'@param data1 ADD_1rt_PARAMETER_DESCRIPTION
##'@param data2 ADD_2on_PARAMETER_DESCRIPTION
##'@param variable ADD_3r_PARAMETER_DESCRIPTION
##'@return data.frame / list / vector
##'@author Fabian Coloma
##'@export
##'@examples
##'
##'
inside_manhica <- function(data, geo){
  
  participants_man <- data %>% 
    select(
      start,
      perm_id,
      start_fu_date,
      end_fu_date,
      house_number,
        gps_longitude,
        gps_latitude
    ) %>%
    filter(
      !is.na(gps_longitude),
      !is.na(gps_latitude)
    ) %>%
      st_as_sf(
        .,
        coords = c("gps_longitude", "gps_latitude"),
        crs = 4326
      ) %>%
      st_transform(
        crs = 32736  # WGS84 UTM 36S
      )


    # Convert to boolean vector
    participants_inside_boolean <- as.logical(st_within(participants_man, geo))

    # Replace NA values with FALSE
    participants_inside_boolean[is.na(participants_inside_boolean)] <- FALSE

    data_in_pre <- participants_man %>% 
        sf::st_drop_geometry() 

    data_in <- data_in_pre[participants_inside_boolean, ]



  data %>%
    inner_join(
      data_in,
      by = dplyr::join_by(
        "house_number" == "house_number",
        "start" == "start",
        "perm_id" == "perm_id",
        "start_fu_date" == "start_fu_date",
        "end_fu_date" == "end_fu_date"
      )
    )


}


##' add function description
##'
##'
##' @title process_final_variables
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
process_final_variables_paper_1 <- function(data){


ddj <- data %>%
  mutate(
    dplyr::across(
      dplyr::starts_with("h_visits"), ~tidyr::replace_na(., 0)
    )
  )  %>%
    mutate(
        gender = sex,
        child_dob = start_fu_date,
        healthdist_km = healthdist / 1000,
        healthdist_final_std = scale(healthdist_km),
        roaddist_km = roaddist / 1000,
        roaddist_std = scale(roaddist_km),
        ilumination_fuel_v2_post_bn = ifelse(ilumination_fuel == "Clean", 1, 0),
        mother_age_at_birth_of_child = floor(as.numeric(
                difftime(
                    child_dob,
                    mother_dob,
                    units = c("days")
                )
            ) / 365.25),
        
        mother_age_at_birth_of_child_std = scale(mother_age_at_birth_of_child),

        year_birth = lubridate::year(child_dob),

        region_grp_by_health =  case_when(
            healthdist_name_facility %in% c("Taninga",
                                            "Palmeira",
                                            "Malavele") ~ "3 de fevereiro influence",
            healthdist_name_facility %in% c("Maragra",
                                            "Manhica sede") ~ "manhica influence",
            healthdist_name_facility %in% c("Xinavane") ~ "xinavane influence",
            healthdist_name_facility %in% c("Ilha Josina Machel") ~ "Ilha Josina Machel influence"
            
        ),
        with_some_hospital_visit = ifelse(h_visits>0, "HAS visits", "NO visits"),
        out_by_visist = paste0(ilumination_fuel, " - ",with_some_hospital_visit)
        

    ) %>% rename(
      "healthdist_all" = "healthdist_all7"
      , "healthdist_name_facility_all" = "healthdist_name_facility_all7"
      , "healthdist_name_opd_sep_all" = "healthdist_name_grp_all7"
      , "healthdist_opd_sep_all" = "healthdist_name_Jgrp_all7"
    )

  ddj

}


##' add function description
##'
##'
##' @title process_final_variables
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
process_final_variables <- function(data, names_to_impute_miss){

names_to_impute <- names_to_impute_miss[!(names_to_impute_miss %in% c("gid", "perm_id", "household"))]

ddj <- data  %>%
  mutate(
    dplyr::across(
      dplyr::starts_with("h_visits"), ~tidyr::replace_na(., 0)
    )
  ) %>%
    mutate(
        gender = sex,
        child_dob = start_fu_date,

        healthdist_km = healthdist / 1000,
        healthdist_final_std = scale(healthdist_km),

        roaddist_km = roaddist / 1000,
        roaddist_std = scale(roaddist_km),

        sugardist_km = sugardist / 1000,
        sugardist_std = scale(sugardist_km),

        ilumination_fuel_v2_post_bn = ifelse(ilumination_fuel == "Clean", 1, 0),
        mother_age_at_birth_of_child = floor(as.numeric(
                difftime(
                    child_dob,
                    mother_dob,
                    units = c("days")
                )
            ) / 365.25),
        
        mother_age_at_birth_of_child_std = scale(mother_age_at_birth_of_child),

        year_birth = lubridate::year(child_dob),

        father_alive_FIMCA = case_when(
          father_alive == "yes" ~ "yes",
          # father_alive == "yes" ~ "yes",
            TRUE ~ "no"
        ),
        mother_alive_FIMCA = case_when(
          mother_alive == "Yes" ~ "yes",
          # father_alive == "yes" ~ "yes",
            TRUE ~ "no"
        ),
        
        father_lives_here_FIMCA = case_when(
          father_lives_here == "Yes" ~ "yes",
          # father_alive == "yes" ~ "yes",
            TRUE ~ "no"
        ),
        mother_lives_here_FIMCA = case_when(
          mother_lives_here == "Yes" ~ "yes",
          # father_alive == "yes" ~ "yes",
            TRUE ~ "no"
        ),
        sibling_death_FIMCA = case_when(
          sibling_death == TRUE ~ "yes",
          # father_alive == "yes" ~ "yes",
            TRUE ~ "no"
        ),
        with_some_hospital_visit = ifelse(h_visits>0, "HAS visits", "NO visits"),
        out_by_visist = paste0(ilumination_fuel, " - ",with_some_hospital_visit)
        

    ) %>%
  mutate(
    # across(
  #   dplyr::starts_with("h_visits"), 
  #   ~tidyr::replace_na(., 0)
  # ),
  ndvi_2015_500_test = ndvi_2015_500 
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "artificial_light_at_night_2015_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "artificial_light_at_night_2020_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "artificial_light_at_night_2015_1000",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "artificial_light_at_night_2020_1000",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "artificial_light_at_night_diff_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "artificial_light_at_night_diff_1000",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "imperviousness_density_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "imperviousness_density_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "tree_cover_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "shrubland_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "grassland_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "cropland_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "build_up_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "bareland_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "permanent_water_bodies_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "tree_cover_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "shrubland_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "grassland_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "cropland_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "build_up_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "bareland_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "permanent_water_bodies_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "ndvi_2015_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "ndvi_2020_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "ndvi_2015_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "ndvi_2020_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "ndvi_diff_300",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "ndvi_diff_500",
    general_imput = TRUE
  ) %>%
  impute_missing_mean(
    .,
    field_to_inputted = "ndvi_2015_500_test"
  ) %>% rename(
      "healthdist_all" = "healthdist_all7"
      , "healthdist_name_facility_all" = "healthdist_name_facility_all7"
      , "healthdist_name_opd_sep_all" = "healthdist_name_grp_all7"
      , "healthdist_opd_sep_all" = "healthdist_name_Jgrp_all7"
    )





  ddj

}



##' add function description
##'
##'
##' @title joining_data_w_visits
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
joining_data_w_visits_paper_1 <- function(
        data_fu_base = data_fu_base,
        mother_w_basic_info_cleaned = mother_w_basic_info_cleaned,
        indv_edu_ocu_choice = indv_edu_ocu_choice,


        house_features_btw_x_choice = house_features_btw_x_choice,
        # house_econ_btw_x_choice = house_econ_btw_x_choice,
        h_visits_total_by_participants = h_visits_total_by_participants, # we will need put 0 in miss values on no visits 
        data_SES_created = data_SES_created,
        distance_data_treatment_GIS = distance_data_treatment_GIS
        ){

dj2 <- data_fu_base %>%
    inner_join(
        mother_w_basic_info_cleaned,
        by = "perm_id"
    ) %>%
    inner_join(
        indv_edu_ocu_choice %>%
            rename(
                mother_has_education = has_education,
                mother_education = education,
                mother_ocupation = ocupation,
                check_sensi_inv = check_sensi
            ),
        by = "mother_perm_id"
    ) %>%
    inner_join(
        house_features_btw_x_choice %>%
            select(
                perm_id,
                ilumination_fuel,
                ilumination_fuel_compl,
                ilumination_fuel_impolute_clean,
                wall_material,
                wall_material_detail,
                kitchen_fuel,
                kitchen_fuel_compl,
                gps_longitude,
                gps_latitude,
                check_sensi,
                house_number
            ),
        by = "perm_id"
    ) %>%
    left_join(
        h_visits_total_by_participants,
        by = "perm_id"
    ) %>%
    inner_join(
        data_SES_created,
        by = "perm_id"
    ) %>%
    inner_join(
      distance_data_treatment_GIS %>% 
        sf::st_drop_geometry() %>% 
        # filter(participant_inside_manhica) %>%
        as.data.frame(),
      by = "perm_id" 
    ) %>%
    as.data.frame()

    # %>% 
    # inner_join(
    #   location_clean %>% 
    #   select(-observations),
    #   by = join_by("region" == "neighborhood")
    # ) %>% 
    # left_join(
    #   data_environment,
    #   by = "perm_id"
    # )%>%
    # as.data.frame()

    dj2



}


##' add function description
##'
##'
##' @title joining_data_w_visits
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
joining_data_w_visits <- function(
        data_fu_base = data_fu_base,
        mother_w_basic_info_cleaned = mother_w_basic_info_cleaned,
        indv_edu_ocu_choice = indv_edu_ocu_choice,
        family_inestability_choice = family_inestability_choice,
        family_siblings_death_extraction = family_siblings_death_extraction,
        parental_marital_status_choice = parental_marital_status_choice,
        house_features_btw_x_choice = house_features_btw_x_choice,
        house_econ_btw_x_choice = house_econ_btw_x_choice,
        mca_part_1_econ = mca_part_1_econ,
        h_visits_total_by_participants = h_visits_total_by_participants, # we will need put 0 in miss values on no visits 
        data_SES_created = data_SES_created,
        distance_data_treatment_GIS = distance_data_treatment_GIS,
        location_clean = location_clean,
        data_environment = data_environment
        ){

dj2 <- data_fu_base %>%
    inner_join(
        mother_w_basic_info_cleaned,
        by = "perm_id"
    ) %>%
    inner_join(
        indv_edu_ocu_choice %>%
            rename(
                mother_has_education = has_education,
                mother_education = education,
                mother_ocupation = ocupation
            ),
        by = "mother_perm_id"
    ) %>%
    inner_join(
        family_inestability_choice %>% 
            select(
                -marital_status
            ),
        by = "perm_id"
    ) %>%
    inner_join(
        family_siblings_death_extraction %>% 
            select(
                perm_id,
                sibling_death
            ),
        by = "perm_id"

    ) %>%
    inner_join(
        parental_marital_status_choice,
        by = join_by("mother_perm_id" == "perm_id")
    ) %>%
    inner_join(
        house_features_btw_x_choice %>%
            select(
                perm_id,
                ilumination_fuel,
                dplyr::ends_with("_HEMCA")
            ),
        by = "perm_id"
    ) %>%
    inner_join(
        house_features_btw_x_choice %>%
            select(
                perm_id,
                gps_longitude,
                gps_latitude,
                house_number
            ),
        by = "perm_id"
    ) %>%
    inner_join(
        mca_part_1_econ,
        by = "perm_id"
    ) %>%
    left_join(
        h_visits_total_by_participants,
        by = "perm_id"
    ) %>%
    inner_join(
        data_SES_created,
        by = "perm_id"
    ) %>%
    inner_join(
      distance_data_treatment_GIS %>% 
        sf::st_drop_geometry() %>% 
        # filter(participant_inside_manhica) %>%
        as.data.frame(),
      by = "perm_id" 
    ) %>% 
    inner_join(
      location_clean %>% 
      select(-observations),
      by = join_by("region" == "neighborhood")
    ) %>% 
    left_join(
      data_environment,
      by = "perm_id"
    )%>%
    as.data.frame()

    dj2



}


##' add function description
##'
##'
##' @title treatment_data_to_get_distance
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##' TODO: realizar los buffer segun personas de st_intersect
##' linea 1586
treatment_data_to_get_distance <- function(
  hfeatures,
  manhica = manhica,
  filter_inside_manhica = TRUE,
  health_f,
  roads_f,
  sugar_f,
  health_facilities_to_distance = c( "Manhica sede",
                                        "Maragra",
                                        "Ilha Josina Machel",
                                        "Taninga",
                                        "Nwamatibjana", # is palmeira - is the name of hospital
                                        "Malavele",
                                        "Xinavane",
                                        "Palmeira" # Maluana - optros que no estan
                                        )){

  # cohort_100 <- st_buffer(hfeatures, 100)
  # cohort_300 <- st_buffer(hfeatures, 300)
  cohort_500 <- st_buffer(hfeatures, 500) 

  health <- health_f %>%
    filter(
      nome %in% health_facilities_to_distance
    )

  health2 <- health_f %>%
    mutate(
      all_facility_name = ifelse(
        nome %in% health_facilities_to_distance,
        paste0("7_", nome),
        paste0("not7_", nome)
        ),
      facility_grouped = ifelse(
        nome %in% health_facilities_to_distance,
        "7_withData",
        "out_7"
      )
    )

  roads <- roads_f

  sugar <- sugar_f

  # 3. Compute GIS variables ---

  # Distance to nearest health centre
  healthdist <- apply(st_distance(hfeatures, health), 1, min)

  # For each participant, find the index of the nearest health facility
  nearest_health_index <- apply(st_distance(hfeatures, health), 1, which.min)

  # Extract the name of the nearest health facility using the index
  nearest_health_name <- health$nome[nearest_health_index]

  ##########
  ##########
  ## health 2
  ##########
  ##########

  # Distance to nearest health centre
  healthdist2 <- apply(st_distance(hfeatures, health2), 1, min)

  # For each participant, find the index of the nearest health facility
  nearest_health_index2 <- apply(st_distance(hfeatures, health2), 1, which.min)

  # Extract the name of the nearest health facility using the index
  nearest_health_name22 <- health2$nome[nearest_health_index2]
  nearest_health_name23 <- health2$all_facility_name[nearest_health_index2]
  nearest_health_name24 <- health2$facility_grouped[nearest_health_index2]


  # Distance to nearest road
  roaddist <- apply(st_distance(hfeatures, roads), 1, min)

  # Distance to sugar cane factory
  sugardist <- apply(st_distance(hfeatures, sugar), 1, min)

  #  NEIGHBOrt
  Nneigh_500 <- sapply(st_intersects(cohort_500, hfeatures), length)
  Nneigh_500 <- ifelse(Nneigh_500==0, 1, Nneigh_500)

  ##########
  ##########
  ## inside_manhica
  ##########
  ##########

    # Convert to boolean vector
    participants_inside_boolean <- as.logical(st_within(hfeatures, manhica))

    # Replace NA values with FALSE
    participants_inside_boolean[is.na(participants_inside_boolean)] <- FALSE

  hfeatures$healthdist <- healthdist
  hfeatures$healthdist_name_facility <- nearest_health_name
  hfeatures$roaddist <- roaddist
  hfeatures$sugardist <- sugardist
  hfeatures$Nneigh_500 <- Nneigh_500
  hfeatures$participant_inside_manhica <- participants_inside_boolean

  hfeatures$healthdist_all7 <- healthdist2
  hfeatures$healthdist_name_facility_all7 <- nearest_health_name22
  hfeatures$healthdist_name_grp_all7 <- nearest_health_name23
  hfeatures$healthdist_name_Jgrp_all7 <- nearest_health_name24


  hfeatures  



}


##' add function description
##'
##'
##' @title transform_base_GIS
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
transform_base_GIS <- function(data){

  hfeatures <- data %>%
    select(
      perm_id,
      # house_number,
      gps_longitude,
      gps_latitude
    ) %>%
    filter(
      complete.cases(.)
    )

  # df to sf, project
  dedo <- sf::st_as_sf(hfeatures, coords = c("gps_longitude", "gps_latitude"), crs = 4326) |>
    sf::st_transform(crs = 32736)  # WGS84 UTM 
  
  dedo

}

##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
Grouping_deo_by_postos <- function(data){
  data %>%
      st_transform(crs = 32736) %>%
      group_by(Admin_Post) %>%
      summarise()
}


##' cleaning and exteracting basic information of Mother
##' we will group al  the steps to extrat the most common 
##' values and the surveys between the FU of every child
##' I thing maybe begfore.. 
##' @title siblings_treatment
##' @param data data expanded
##' @param w_sensitivity which sensitivity we want
##' @param w_common_variables wich variable we want knos has more common
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
siblings_treatment <- function(data_mother, data_base){


  d2 <- data_base %>%
    select(
      perm_id,
      fu_death
    ) %>%
    inner_join(
      data_mother,
      by = "perm_id"
    ) 
  

  d3 <- d2 %>% 
      group_by(mother_perm_id) %>%
      summarise(
      count_child_in_fu = n()
      , any_death = sum(fu_death)
    ) %>%
    filter(count_child_in_fu > 1)

  d4 <- d2 %>% 
      left_join(d3, by = "mother_perm_id") %>%
      mutate(
          sibling_death = ifelse(
              !(is.na(count_child_in_fu)) &
              (
                  count_child_in_fu == any_death |
                  any_death > 1 |
                  (any_death >=1 & any_death == 0)
              ),
              TRUE,
              FALSE
          )
      )

    d4


}

##' cleaning and exteracting basic information of Mother
##' we will group al  the steps to extrat the most common 
##' values and the surveys between the FU of every child
##' I thing maybe begfore.. 
##' @title grouping_visits_at_participants_and_cleaning
##' @param data data expanded
##' @param w_sensitivity which sensitivity we want
##' @param w_common_variables wich variable we want knos has more common
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
grouping_visits_at_participants_and_cleaning <- function(data){
  data %>%
  filter(
    !(same_episode)
  ) %>%
  group_by(
          perm_id
          ) %>% 
      summarise(
          h_visits = sum(num_v),
          h_visits_respiratory = sum(respiratory),
          h_visits_respiratory_byCode = sum(respiratory_all_respiratory),
          h_visits_malaria_visits = sum(malaria),
          h_visits_fever_visits = sum(fever),
          h_visits_bronchitis_visits = sum(bronchitis)
          )
}

##' cleaning and exteracting basic information of Mother
##' we will group al  the steps to extrat the most common 
##' values and the surveys between the FU of every child
##' I thing maybe begfore.. 
##' @title which_common_features_is_needed
##' @param data data expanded
##' @param w_sensitivity which sensitivity we want
##' @param w_common_variables wich variable we want knos has more common
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
clean_get_mother_basic_information <- function(data, interval_data, data_member){
  
  data_sub_selection <- data %>%
    select(
      start = sys_odk_start
      , perm_id
      , mother_perm_id
      , father_perm_id
      , gender
    ) %>% as.data.frame()

  cleaning_btw_fu <- clean_interval_data_and_deploy(
    data_sub_selection,
    interval_data,
    id_1 = "perm_id",
    id_2 = "perm_id"
  )

  most_common_data_pre <- which_common_features_is_needed(
    data = cleaning_btw_fu,
    w_sensitivity = c("between_dates"
                       ,"between_3months_plus"
                       ,"between_6months_plus"
                       ,"between_1yr_plus"
                       ,"between_1_5yr_plus"
                       ,"between_2yr_plus"),
    w_common_variables = c( "mother_perm_id",
                            "father_perm_id",
                            "gender"
    )
  ) %>% 
  filter(mother_perm_id != "UNK") %>% as.data.frame()

  data_mother <- data_member %>%
    filter(
      perm_id %in% most_common_data_pre$mother_perm_id
    ) %>%
    select(
      mother_perm_id = perm_id,
      mother_dob = dob
    ) %>%
    distinct() %>%
    group_by(mother_perm_id) %>%
    mutate(number_by_id = row_number()) 

    print(nrow(data_mother))

    data_mother_no_err <- data_mother %>%
      filter(number_by_id <= 1)

    
    print(nrow(data_mother_no_err))

    # %>%
    # getMostCommonSurveyResponse(
    #       .,
    #       mother_perm_id,
    #       c("mother_dob")
    # )


  final_data <- most_common_data_pre %>% 
    left_join(
      data_mother_no_err,
      by = "mother_perm_id"
    )

  final_data

}

##' In this section is needed ask for pondering the most common .. 
##' other question to ask is see if is bnetter group before or after the variables
##' I thing maybe begfore.. 
##' @title which_common_features_is_needed
##' @param data data expanded
##' @param w_sensitivity which sensitivity we want
##' @param w_common_variables wich variable we want knos has more common
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
which_sensibility_is_need <- function(data, w_sensitivity){
  data %>%
    filter(check_sensi %in% w_sensitivity)
}
##' In this section is needed ask for pondering the most common .. 
##' other question to ask is see if is bnetter group before or after the variables
##' I thing maybe begfore.. 
##' @title which_common_features_is_needed
##' @param data data expanded
##' @param w_sensitivity which sensitivity we want
##' @param w_common_variables wich variable we want knos has more common
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
which_common_features_is_needed <- function(data, w_sensitivity, w_common_variables, impolute = FALSE){

  if(w_sensitivity[1] == TRUE){
    if(impolute == TRUE){
 print(table(data$ilumination_fuel_compl))

   data  %>%
      group_by(perm_id) %>%
      mutate(
        flag_btw_dates_no = all(check_sensi != "between_dates"),
        min_sensitive_survey = min(check_sensi_numeric)
      ) %>%
      filter(
        flag_btw_dates_no |
        check_sensi == "between_dates",
        check_sensi != "no_surveys_on_period",
        check_sensi_numeric == min_sensitive_survey
      )%>%
      mutate(
        ilumination_fuel_impolute_clean = all(ilumination_fuel_compl %in% c("Battery", "Electricity", "Solar panel")) 
      ) %>%
        getMostCommonSurveyResponse(
            .,
            perm_id,
            all_of(c(w_common_variables, "check_sensi", "check_sensi_numeric"))
        )
    } else {

   data  %>%
      group_by(perm_id) %>%
      mutate(
        flag_btw_dates_no = all(check_sensi != "between_dates"),
        min_sensitive_survey = min(check_sensi_numeric)
      ) %>%
      filter(
        flag_btw_dates_no |
        check_sensi == "between_dates",
        check_sensi != "no_surveys_on_period",
        check_sensi_numeric == min_sensitive_survey
      ) %>%
        getMostCommonSurveyResponse(
            .,
            perm_id,
            all_of(c(w_common_variables, "check_sensi", "check_sensi_numeric"))
        )
    }
   

  }else{
    data %>%
      filter(check_sensi %in% w_sensitivity)%>%
        getMostCommonSurveyResponse(
            .,
            perm_id,
            all_of(c(w_common_variables, "check_sensi"))
        )

  }

}

##' in this section we will get the list of houses that the child been with 
##' the respective fu time in the home
##' We think necessary delete the houses the child been cero days.
##' and delete that child have a exit time or fu negative.. but we need here ?
##' @title getting_houseId_partID_w_periods
##' @param data_system data
##' @return data.frame that have different
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##' TODO: que haces con los exit == 1
getting_houseId_partID_w_periods <- function(data, remove_list){

#  %>%
#     filter(
#       !(
#         perm_id %in% remove_list$to_delet_just_on_a_part$cero_ext_and_some_FU &
#         start_diff == 0
#       )
#     )
  dd <- data %>% 
    filter(
      !(perm_id %in% c(
        remove_list$to_delete_sure$negative_fu_ext_ids
        , remove_list$to_delete_sure$death_at_start_ids
        , remove_list$to_delete_sure$cero_fu_cases_ids
        , remove_list$to_delete_sure$no_birth_as_start
      ) )
    ) %>%
    select(
      perm_id
      , household
      , start_fu_date
      , end_fu_date
    ) 

  dd
}

##' In this section we will create a list of id 
##' that we will delete, but not all at the same time
##' Because we will want see the distribution  
##' of this cases to know which type of information we are lossing
##'
##' @title generation_list_del_sysData
##' @param data data summary to extract the miss cases
##' @return list
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
generation_list_del_sysData <- function(data){
  
  ##########
  ##########
  ## NEGASTIVE cases on FU and EXITS
  ##########
  ########## 9 cases 

  negative_fu_or_ext <- data %>% 
    filter(
      flag_negative_EXT == TRUE |
      flag_negative_FU == TRUE
    ) %>% 
    pull(perm_id) %>% unique()

  ##########
  ##########
  ## cases of death in the start
  ##########
  ########## 142 cases

  death_in_start <- data %>%
    filter(
      flag_dth_in_start == TRUE
    ) %>% 
    pull(perm_id) %>% unique()
  
  ##########
  ##########
  ## cases of death in the start
  ##########
  ########## 142 cases

  no_birth_in_start <- data %>%
    filter(
      start_type != "BIR"
    ) %>% 
    pull(perm_id) %>% unique()

  ##########
  ##########
  ## we quit the child we loss at the Start
  ##########
  ##########
  # Cause they are given 0 information.. 
  # we will study if some of they has hospital 
  # visits - 
  # 70 cases

  cero_fu_cases <- data %>%
        filter(
            # fu_days == 0
            flag_fu_all_cero_days
        ) %>% 
        filter(
            !(perm_id %in% c(
                negative_fu_or_ext
                , death_in_start
                ))
            
        ) %>% pull(., "perm_id") %>% unique()

  ##########
  ##########
  ## CERO DAYS on EXT and MOVEMENTS
  ##########
  ##########
  # here we dont make to delete the id, just the row o cero 
  # 60 cases -

  cero_ext_but_FU_some <- data %>%
    filter(
        flag_ext_cero_days, 
        !(
            perm_id %in% c(
                negative_fu_or_ext
                , death_in_start
                , cero_fu_cases
            )
        )
    ) %>% 
    pull(perm_id) %>%
    unique()

  ##########
  ##########
  ## ADD_YOUR_DESCRIPTION
  ##########
  ##########

  fu_migration_to_see <- data %>%
      filter(
          flag_migration, 
          !(
              perm_id %in% c(
                  negative_fu_or_ext
                  , death_in_start
                  , cero_fu_cases
              )
          )
      ) %>% 
      pull(perm_id) %>%
      unique()


    list(
      to_delete_sure = list(
        negative_fu_ext_ids = negative_fu_or_ext
        , death_at_start_ids = death_in_start
        , cero_fu_cases_ids = cero_fu_cases
        , no_birth_as_start = no_birth_in_start
      )
      , to_delet_just_on_a_part = list(
        cero_ext_and_some_FU = cero_ext_but_FU_some
      )
      , to_check = list(
        migration_ids = fu_migration_to_see

      )
    )
}

##' in this secction we will obtain a row per participant
##' we will select the last date and the first date,
##' to get the period that we want look on the caracteristics.. 
##' @title gettingFUperParticipant
##' @param data_system data
##' @param start dste of start FU
##' @param end date end FU
##' @return data.frame that have different
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
gettingFUperParticipant <- function(data){
  
  end_type_desc <- data  %>% 
    arrange(perm_id, start_date) %>%
    group_by(perm_id) %>%
    filter(row_num == max(row_number())) %>% 
    as.data.frame() %>% 
    select(
      perm_id,
      end_fu_type
    )
  
  start_type_desc <- data  %>% 
    arrange(perm_id, start_date) %>%
    group_by(perm_id) %>%
    filter(row_num == min(row_number())) %>% 
    as.data.frame() %>% 
    select(
      perm_id,
      start_type
    )

  # objetive 27.863 participants

  data %>%
    group_by(perm_id) %>%
    summarise(
      start_date = min(start_date)
      , end_date = max(end_fu_date)
      , out_days = sum(start_diff, na.rm = TRUE)
      , flag_migration = any(flag_migration, na.rm = TRUE)
      , flag_any_exit = any(flag_any_exit, na.rm = TRUE)
      , flag_mov_number = any(flag_mov_number, na.rm = TRUE)
      , flag_house_change = any(flag_house_change , na.rm = TRUE)
      , flag_negative_EXT = any(flag_negative_EXT, na.rm = TRUE)
      , flag_ext_cero_days = any(flag_ext_cero_days, na.rm = TRUE)
      , fu_days = sum(fu_days_b, na.rm =TRUE)
      , fu_months = sum(fu_months_b, na.rm =TRUE)
      , fu_years = sum(fu_years_b, na.rm =TRUE)
      , out_perc = (out_days / ifelse(fu_days == 0, 1, fu_days)) * 100 
      , flag_fu_all_cero_days = all(flag_fu_cero_days, na.rm=TRUE)
      , flag_fu_cero_days = any(flag_fu_cero_days, na.rm=TRUE)
      , flag_negative_FU = any(flag_negative_FU, na.rm=TRUE)
      , flag_dth_in_start = any(flag_dth_in_start, na.rm=TRUE)
      , flag_migration_gen = out_days > (30.4375 * 6)
    ) %>%
    left_join(
      start_type_desc,
      by = "perm_id"
    ) %>% 
    left_join(
      end_type_desc,
      by = "perm_id"
    ) %>%
    rename(end_fu_date = end_date, start_fu_date = start_date) %>%
    mutate(
      fu_death = case_when(
        end_fu_type == "DTH" ~ 1,
        TRUE ~ 0
      )
    )
    # %>% 
    # mutate(across(starts_with('flag'), ~replace_na(.,FALSE)))
}



##' Subselecting history System information
##'
##'
##' @title process_res_history
##' @param data_system data 
##' @param start dste of start FU
##' @param end date end FU
##' @return data.frame that have different 
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
process_res_history <- function(data_system,
                                   start,
                                   end){

  start <- as.Date(start)
  end <- as.Date(end)
  
  vect_birth <- data_system %>%
    dplyr::filter(start_date >= start & start_type == "BIR") %>%
    pull(., perm_id) %>%
    unique(.)
  
  # don't ask how we advice this case...(1 case)
  vect_birth_before_as_FU <- data_system %>%
    dplyr::filter(
        perm_id %in% vect_birth,
        start_date < start
    ) %>%
    pull(., perm_id) %>% 
    unique(.)
  
  dd <- data_system %>% 

    dplyr::filter(
        perm_id %in% vect_birth,
        !(perm_id %in% vect_birth_before_as_FU),
        
        start_date < end


        # (end_date < vEnd_date |
        # is.na(end_date))
    ) %>%
    dplyr::mutate(
          end_fu_date = case_when(
            is.na(end_date) ~ as.Date(end) - 1,
            end_date >= as.Date(end) ~ as.Date(end) -1,
            TRUE ~ end_date)
    )%>%
    arrange(perm_id, start_date) %>%
    group_by(perm_id) %>%
    dplyr::mutate(

        start_diff = as.numeric(start_date) - as.numeric(lag(end_fu_date)),
        end_type_before = lag(end_type),
        end_date_before = lag(end_fu_date),
        house_number_befor = lag(household),
        row_num = row_number(),
        flag_migration = start_diff > (30.4375 * 6) # six month is 
        # , flag_birth_as_start = ifelse(perm_id %in% vect_birth, TRUE, FALSE)
        , flag_any_exit = start_diff > 0
        , flag_mov_number = (row_number() - 1) > 0
        , flag_house_change =  household == house_number_befor
        , flag_negative_EXT = start_diff < 0
        # , flag_ext_cero_days = start_diff == 0
        , flag_ext_cero_days = start_diff == 0
        
        # declared as migration
    )   # include a basis flag to 0 FU and the sum of FU is not 0
    # %>%
    # dplyr::filter(
    #     # end_type_before != "EXT" &
    #     (flag_migration != TRUE |
    #     is.na(flag_migration))
    # )


  dd %>%
    dplyr::mutate(
      end_fu_type = if_else( (end_date > end) | is.na(end_type) | is.na(end_date),
                             "noEnd", 
                             end_type ),
      start_fu_date = if_else(start_date < start, as.Date(start), start_date),  # sure?
      # end_fu_date = case_when(
      #   is.na(end_date) ~ as.Date(end),
      #   end_date > as.Date(end) ~ as.Date(end),
      #   TRUE ~ end_date
      # ),
      fu_days_b = as.integer(end_fu_date - start_fu_date),
      fu_years_b = fu_days_b / 365.25,
      fu_months_b = fu_days_b / 30.4375
      , flag_negative_FU = fu_days_b < 0
      # fu_perc_migration = (start_diff / fu_days) * 100
      , flag_fu_cero_days = fu_days_b == 0
      , flag_dth_in_start = flag_fu_cero_days == TRUE & end_fu_type == "DTH"

      
    ) %>%
    filter(
      fu_days_b >= 0 # solo es un caso 
    )

}

##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
# NAME_OF_YOUR_FUNKTION <- function(){

# }

##'
##' Subselecting the FU dates for member information.
##'
##' @title subsetBirthCohort
##' @param survey_data data.
##' @param res_history data.
##' @param start char.
##' @param end char.
##' @return data.frame / list / vector
##' @author Sergio Olmo
##' @export
##' @examples
##' 
##'
subsetBirthCohort <- function(survey_data, res_history, start, end) {
  multiple_dobs <- survey_data %>%
    select(perm_id, dob) %>%
    distinct() %>%
    count(perm_id) %>%
    filter(n > 1) %>%
    arrange(desc(n))

  multiple_sex_records <- survey_data %>%
    select(perm_id, gender) %>%
    distinct() %>%
    count(perm_id) %>%
    filter(n > 1) %>%
    arrange(desc(n))

  ## FIXME Perhaps we should get the most common date of birth instead of dropping those with several dates
  children_clean <- survey_data %>%
    filter(
      !perm_id %in% multiple_dobs$perm_id,
      !perm_id %in% multiple_sex_records$perm_id,
    ) %>%
    filter(dob >= start, dob < end)

  birth_cohort <- children_clean %>%
    select(perm_id, dob, gender) %>%
    distinct() %>%
    left_join(res_history, by = "perm_id") %>%
    filter(start_type == "BIR") %>%
    select(perm_id, dob, gender)

  birth_cohort
}

##' 
##' Creating FU variables
##'
##' @title getCohortFollowUpPeriods
##' @param cohort data.
##' @param follow_up data.
##' @param start char.
##' @param end char.
##' @return data.frame
##' @author Sergio Olmo
##' @export
##' @examples
##' 
##'
getCohortFollowUpPeriods <- function(cohort, follow_up, start, end) {
  cohort_res_history_within_fu <- cohort %>%
    left_join(follow_up, by = "perm_id") %>%
    ## Some cleaning is needed
    filter(dob <= start_date, start_date < end) %>%
    mutate(
      start_fu_date = if_else(start_date < start, as.Date(start), start_date),
      end_fu_date = case_when(
        is.na(end_date) ~ as.Date(end),
        end_date > as.Date(end) ~ as.Date(end),
        TRUE ~ end_date
      ),
      fu_days = as.integer(end_fu_date - start_fu_date)
    )

  cohort_res_history_within_fu
}

##' 
##' Same episode cleaning
##'
##' @title getSameEpisodes
##' @param events data.
##' @param window data.
##' @return data.frame
##' @author Sergio Olmo
##' @export
##' @examples
##' 
##'
getSameEpisodes <- function(events, window) {
  events_diff <- events %>%
    arrange(perm_id, date_event) %>%
    group_by(perm_id) %>%
    summarise(
      date_event = date_event,
      date_event_lag1 = lag(date_event),
      d_diff = as.integer(date_event - date_event_lag1),
      across(matches("^diag[1-4]$"), ~.x, .names = "{.col}_current"),
      across(matches("^diag[1-4]$"), lag, .names = "{.col}_lag")
    ) %>%
    ungroup()

  ## Checks if there is more than one occurrance of
  ## the same diagnostic code in each row.
  events_diff$diag_matches <- events_diff %>%
    select(starts_with("diag")) %>%
    apply(MARGIN = 1, function(x) ifelse(sum(table(x) > 1) > 0, TRUE, FALSE))

  events_diff %>%
    mutate(same_episode = ifelse(d_diff < window & diag_matches, TRUE, FALSE))
}

##'
##' FU events
##'
##' @title getEventsInFu
##' @param follow_up ADD_1rt_PARAMETER_DESCRIPTION
##' @param events ADD_2on_PARAMETER_DESCRIPTION
##' @return data.frame
##' @author Sergio Olmo
##' @export
##' @examples
##' 
##'
getEventsInFu <- function(follow_up, events) {
  events_within_fu <- follow_up %>%
    left_join(events, by = "perm_id") %>%
    mutate(
      event_within_fu = if_else(
        date_event >= start_date & date_event <= end_fu_date,
        TRUE, FALSE
      )
    ) %>%
    filter(event_within_fu | is.na(event_within_fu)) %>%
    mutate(event_id = seq_len(nrow(.)))

  events_within_fu
}

##'
##' number of episodes
##'
##' @title countEpisodes
##' @param follow_up ADD_1rt_PARAMETER_DESCRIPTION
##' @param events ADD_2on_PARAMETER_DESCRIPTION
##' @param window ADD_2on_PARAMETER_DESCRIPTION
##' @param event_name ADD_2on_PARAMETER_DESCRIPTION
##' @param cause ADD_2on_PARAMETER_DESCRIPTION
##' @return data.frame
##' @author Sergio Olmo
##' @export
##' @examples
##' 
##'
countEpisodes <- function(follow_up,
                          events,
                          window,
                          event_name,
                          cause = "all") {
  ## Filter observations pertaining to a specific cause
  stopifnot(cause %in% colnames(events) || cause == "all")
  if (cause != "all") {
    events <- events[events[[cause]], ]
  }

  events <- events %>%
    filter(perm_id %in% unique(follow_up$perm_id))

  same_episodes <- getSameEpisodes(events = events, window = window)
  episodes_within_fu <- getEventsInFu(follow_up, same_episodes)

  episodes_count <- episodes_within_fu %>%
    mutate(
      # Visits belongnig to the same episodes should be included
      # if they appear in the first day of follow up.
      same_episode_valid = ifelse(
        date_event == start_date & same_episode,
        TRUE, FALSE
      )
    ) %>%
    group_by(perm_id, period_ordinal) %>%
    summarise(
      same_episodes = sum(same_episode & !same_episode_valid, na.rm = TRUE),
      n_events_raw = sum(!is.na(date_event)),
      n_episodes = n_events_raw - same_episodes
    ) %>%
    ungroup()

  stopifnot(ncol(episodes_count) == 5)
  colnames(episodes_count) <- c(
    "perm_id",
    "period_ordinal",
    paste0("n_repeated_", event_name),
    paste0("n_", event_name, "_raw"),
    paste0("n_", event_name)
  )

  episodes_count
}

##'
##' count events on hospital visits
##'
##' @title countValidEvents
##' @param events_within_fu ADD_1rt_PARAMETER_DESCRIPTION
##' @return data.frame
##' @author Sergio Olmo
##' @export
##' @examples
##' 
##'
countValidEvents <- function(events_within_fu) {
  events_count <- events_within_fu %>%
    mutate(
      # Visits belongnig to the same episodes should be included
      # if they appear in the first day of follow up.
      same_episode_valid = ifelse(
        date_event == start_date & same_episode,
        TRUE, FALSE
      )
    ) %>%
    group_by(perm_id, period_ordinal) %>%
    summarise(
      same_episodes = sum(same_episode & !same_episode_valid, na.rm = TRUE),
      n_events_raw = sum(!is.na(date_event)),
      n_events_clean = n_events_raw - same_episodes
    ) %>%
    ungroup()

  events_count
}

##'
##' Birth cohor joining
##'
##' @title buildCohortFinal
##' @param follow_up ADD_1rt_PARAMETER_DESCRIPTION
##' @param episode_count ADD_1rt_PARAMETER_DESCRIPTION
##' @param house_features_common ADD_1rt_PARAMETER_DESCRIPTION
##' @param econ_assets_common ADD_1rt_PARAMETER_DESCRIPTION
##' @param survey_visits_common ADD_1rt_PARAMETER_DESCRIPTION
##' @param deaths_clean ADD_1rt_PARAMETER_DESCRIPTION
##' @return data.frame
##' @author Sergio Olmo
##' @export
##' @examples
##' 
##'
buildCohortFinal <- function(follow_up,
                             episode_count,
                             house_features_common,
                             econ_assets_common,
                             survey_visits_common,
                             deaths_clean) {
  cohort <- follow_up %>%
    ## NOTE We keep only the first uniterrupted follow up periods
    filter(start_type == "BIR", fu_days > 0) %>%
    mutate(
      age_end_days = as.integer(end_fu_date - dob),
      age_end_years = round(age_end_days / 365.25)
    ) %>%
    left_join(episode_count, by = c("perm_id", "period_ordinal")) %>%
    ## Need to fill merged variables with no match
    mutate(
      ## FIXME Why this?
      ## fu_days_clean = ifelse(
      ##   is.na(same_episodes),
      ##   yes = fu_days,
      ##   no = fu_days - same_episodes
      ## ),
      across(matches("n_"), replace_na, replace = 0)
      ## n_visits = ifelse(is.na(n_visits), 0, n_visits),
      ## n_admi = ifelse(is.na(n_admi), 0, n_admi)
    ) %>%
    left_join(house_features_common, by = c(start_household = "house_number")) %>%
    left_join(econ_assets_common, by = c(start_household = "house_number")) %>%
    left_join(
      survey_visits_common %>% select(-father_perm_id, -mother_perm_id),
      by = c(head_perm_id = "perm_id")
    ) %>%
    left_join(deaths_clean, by = c(head_perm_id = "perm_id")) %>%
    mutate(
      across(floor_material:relation_with_head, factor),
      head_died_within_fu = ifelse(
        date_of_death >= start_fu_date & date_of_death <= start_fu_date + 365,
        TRUE, FALSE
      ),
      head_household_dead = case_when(
        is.na(date_of_death) & is.na(head_perm_id) ~ NA,
        is.na(date_of_death) & !is.na(head_perm_id) ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    select(perm_id, fu_days, starts_with("n_"), everything())

  cohort
}


##' add function description
##'
##'
##' @title countBirthCohortDays
##' @param data_followup data of FU
##' @param min_date parameter of Min date
##' @param max_date parameter of max date
##' @return data.frame
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
countBirthCohortDays <- function(data_followup, min_date, max_date) {
  ## NOTE If entry would be other than birth, something should be done about
  ## the entry date when computing the difference.
  days_in_periods <- data_followup %>%
    filter(start_date >= min_date, start_date <= max_date) %>%
    mutate(
      end_fu_date = case_when(
        is.na(end_date) ~ as.Date(max_date),
        end_date > as.Date(max_date) ~ as.Date(max_date),
        TRUE ~ end_date
      ),
      days_fu = as.integer(end_fu_date - start_date)
    )

  days_in_periods %>%
    group_by(perm_id) %>%
    summarise(days_fu = sum(days_fu))
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Get the most frequent
##' @param x A vector.
##' @return A string.
##' @author Sergio Olmos
getMostCommonValue <- function(x) {
  freq <- table(x)
  if (class(x) == "factor") {
    obj_class <- "character"
  } else {
    obj_class <- class(x)
  }

  if (sum(freq) == 0) {
    no_records <- NA
    class(no_records) <- obj_class
    return(no_records)
  } else {
    most_freq_value <- names(freq)[which.max(freq)][1]
    class(most_freq_value) <- obj_class
    return(most_freq_value)
  }
}

getMostCommonSurveyResponse <- function(data, id, vars = everything()) {
  dat <- data %>%
    group_by({{ id }}) %>%
    summarise(across({{ vars }}, getMostCommonValue))

  dat
}


getValidHouseVisits <- function(res_history_clean,
                                houses_clean,
                                min_date,
                                max_date,
                                valid_only) {
  validity_of_visits <- res_history_clean %>%
    filter(start_date >= min_date, start_date <= max_date) %>%
    left_join(houses_clean, by = c(start_household = "house_number")) %>%
    mutate(
      end_fu_date = case_when(
        is.na(end_date) ~ as.Date(max_date),
        end_date > as.Date(max_date) ~ as.Date(max_date),
        TRUE ~ end_date
      ),
      visit_valid = if_else(
        date >= start_date & date <= end_fu_date, TRUE, FALSE)
    ) %>%
    select(
      perm_id,
      visit,
      visit_valid,
      start_type,
      start_date,
      end_type,
      end_date,
      date,
      start_household,
      everything()
    ) %>%
    arrange(perm_id, date)

  if (valid_only) {
    filter(validity_of_visits, visit_valid == TRUE)
  } else validity_of_visits
}

getMostCommonHouseFeatures <- function(houses_clean) {

  getMostCommon <- function(x) {
    freq <- table(x)
    ## table will check how many times each level of a factor occurs,
    ## even if all values are NA, in which case all counts will be 0.
    if (sum(freq) == 0) {
      return(NA)
    } else {
      return(names(freq)[which.max(freq)][1])
    }
  }

  house_features_common <- houses_clean %>%
    group_by(house_number) %>%
    summarise(
      across(where(is.factor), getMostCommon, .names = "{.col}_common"),
      across(where(is.factor), ~length(.x), .names = "{.col}_n"),
      across(
        where(is.factor),
        ~sum(is.na(.x)) / length(.x),
        .names = "{.col}_missing"
      )
    ) %>%
    mutate(across(-house_number, as.factor))

  house_features_common
}

getMostCommonHouseFeaturesTimeVarying <- function(res_history_clean,
                                       houses_clean,
                                       min_date,
                                       max_date,
                                       valid_only = TRUE) {
  valid_house_visits <- getValidHouseVisits(
    res_history_clean,
    houses_clean,
    min_date,
    max_date,
    valid_only
  )

  getMostCommon <- function(x) {
    freq <- table(x)
    if (sum(freq) == 0) {
      return(NA)
    } else {
      return(names(freq)[which.max(freq)][1])
    }
  }

  house_features_common <- valid_house_visits %>%
    group_by(perm_id) %>%
    summarise(
      wall_material = getMostCommon(wall_material),
      floor_material = getMostCommon(floor_material),
      has_kitchen = getMostCommon(has_kitchen),
      kitchen_fuel = getMostCommon(kitchen_fuel),
      ilumination_fuel = getMostCommon(ilumination_fuel)
    )

  house_features_common
}

detectCause <- function(data, pattern) {
  labdiag <- data %>%
    select(starts_with("labdiag")) %>%
    mutate(across(everything(), ~replace_na(., "")))

  is_cause_diag <- apply(
    labdiag,
    MARGIN = 1,
    function(x) sum(str_detect(tolower(x), pattern)) > 0
  )

  is_cause_diag
}

getCauseCategory <- function(data) {
  # Modify this list for adding or removing categories and/or labels
  causes <- list(
    respiratory = c(
      "respiratory",
      "pneumonia",
      "emphysema",
      "influenza",
      "lung",
      "pleura",
      "asthma",
      "bronchitis",
      "bronchiolitis"
    ) %>%
      paste(collapse = "|"),
    malaria = c(
      "falciparum",
      "malariae",
      "malaria"
    ) %>%
      paste(collapse = "|"),
    fever = c("fever") %>%
      paste(collapse = "|"),
    bronchitis = c("bronchitis") %>%
      paste(collapse = "|"),
    uri = c("upper_respiratory") %>%
      paste(collapse = "|"),
    control = c(
      "injur",
      "fall",
      "toxic",
      "poisoning",
      "accident",
      "fracture",
      "contusion",
      "bitten",
      "burn",
      "crash",
      "trauma",
      "foreign"
    ) %>%
      paste(collapse = "|")
  )

  causes_df <- map_dfc(causes, ~detectCause(data, .))

  bind_cols(data, causes_df)
}


get_summary_missing <- function(
    data
){

    all_vars <- names(data)

    number_miss_data <- data %>%
        select(all_of(all_vars)) %>%
        mutate(
          across(
            all_of(all_vars),
            ~ is.na(.)
          )
        ) %>%
        colSums() %>%
        as.data.frame()

    names(number_miss_data) <- "num_row_miss"

    miss_data_compl <- number_miss_data %>%
        mutate(
             percentage_miss_data = round(
                    (num_row_miss / nrow(data)
                    ) * 100, 2
                )
        )

    miss_data_compl 
}


get_cause_visits_v2 <- function(data, diag_group, name_new_var = "respiratory_v2"){
  data %>%
    mutate(
      to_consult = case_when(
        !is.na(diag4) ~ 4,
        !is.na(diag3) ~ 3,
        !is.na(diag2) ~ 2,
        !is.na(diag1) ~ 1,
        TRUE ~ 0
      ),

      !!name_new_var := case_when(
        to_consult == 4 ~ ifelse(toupper(diag4) %in% diag_group, TRUE, FALSE),
        to_consult == 3 ~ ifelse(toupper(diag3) %in% diag_group, TRUE, FALSE),
        to_consult == 2 ~ ifelse(toupper(diag2) %in% diag_group, TRUE, FALSE),
        to_consult == 1 ~ ifelse(toupper(diag1) %in% diag_group, TRUE, FALSE),
        to_consult == 0 ~ respiratory
      )

    )
}

get_cause_visits_v3 <- function(data, diag_group, name_new_var = "respiratory_v3"){
  data %>%
    mutate(
      to_consult = case_when(
        !is.na(diag1) ~ 1,
        !is.na(diag2) ~ 2,
        !is.na(diag3) ~ 3,
        !is.na(diag4) ~ 4,
        TRUE ~ 0
      ),

      !!name_new_var := case_when(
        to_consult == 1 ~ ifelse(toupper(diag1) %in% diag_group, TRUE, FALSE),
        to_consult == 2 ~ ifelse(toupper(diag2) %in% diag_group, TRUE, FALSE),
        to_consult == 3 ~ ifelse(toupper(diag3) %in% diag_group, TRUE, FALSE),
        to_consult == 4 ~ ifelse(toupper(diag4) %in% diag_group, TRUE, FALSE),
        to_consult == 0 ~ respiratory
      )

    )
}

codes_by_id_extraction <- function(h_visits_clean_pre, diag, list_perm_id){

  # data_path <- "codes_by_id_extraction/outputs/procced_data/data_fu_final_w_ipw.csv"

  # data_vf_1 <- read_csv(data_path)

  perm_id_1 <- list_perm_id

  # looking the diference beetwen bth date on final data and visist data  

  no_j_includes <- c(
      "J06J0",
      "J1810",
      "J218",
      "J223",
      "JA09",
      "JI1",
      "JI1.1",
      "JI8.9",
      "JJ18.",
      "JO0",
      "JO6",
      "JOO",
      "J191",
      "J180",
      "J189",
      "J210",
      "J218",
      "J219",
      "J208",
      "J209",
      "J111",
      "J159",
      "J180."
  )


  data_with_perm_id <- h_visits_clean_pre 
  # %>%
  #     filter(perm_id %in% perm_id_1)

  a <- data_with_perm_id %>%
      filter(
          str_detect(toupper(diag4), "^J") 
      ) %>% 
      pull(., diag4) %>% 
      unique() 

  b <- data_with_perm_id %>%
      filter(
          str_detect(toupper(diag3), "^J") 
      ) %>% 
      pull(., diag3) %>% 
      unique()

  c <- data_with_perm_id %>%
      filter(
          str_detect(toupper(diag2), "^J") 
      ) %>% 
      pull(., diag2) %>% 
      unique() 

  d <- data_with_perm_id %>%
      filter(
          str_detect(toupper(diag1), "^J") 
      ) %>% 
      pull(., diag1) %>% 
      unique() 
  
  diag_a <- diag %>%
      filter(str_detect(diag_ct, "^J")) %>%
      pull(., diag) %>%
      unique()

  every <- toupper(unique(c(a, b, c, d, diag_a)))

  every[!(every %in% no_j_includes)] %>% as.vector()

}


codes_by_all_respiratory <- function(h_visits_clean_pre, diag, list_perm_id){

  # data_path <- "codes_by_id_extraction/outputs/procced_data/data_fu_final_w_ipw.csv"

  # data_vf_1 <- read_csv(data_path)

  # perm_id_1 <- list_perm_id

  # looking the diference beetwen bth date on final data and visist data  

  no_j_includes <- c(
      "J06J0",
      "J1810",
      "J218",
      "J223",
      "JA09",
      "JI1",
      "JI1.1",
      "JI8.9",
      "JJ18.",
      "JO0",
      "JO6",
      "JOO",
      "J191",
      "J180",
      "J189",
      "J210",
      "J218",
      "J219",
      "J208",
      "J209",
      "J111",
      "J159",
      "J180.",
      "J06-9",
      "J0Y",
      "J0B",
      "J145", 
      "JA3", 
      "JA6", 
      "JA68", 
      "JA9", 
      "JH03",
      "JI1",
      "JI1.1",
      "JI5",
      "JI8",
      "JI8.9",
      "JJ18.",
      "JJ99",
      "JK06",
      "JR0",
      "JR69",
      "J1810",
      "J181", 
      "J180.",
      "J180",
      "J111",
      "J191",
      "J218"
  )


  data_with_perm_id <- h_visits_clean_pre %>%
      mutate(
        diag1 = gsub("O", "0", gsub("`", "", toupper(diag1))),
        diag2 = gsub("O", "0", gsub("`", "", toupper(diag2))),
        diag3 = gsub("O", "0", gsub("`", "", toupper(diag3))),
        diag4 = gsub("O", "0", gsub("`", "", toupper(diag4)))
      )
  ### CREATE A COMPOSITE VARIABLE FOR RESPIRATORY CAUSES ###
  ##########################################################

  ## Generate the variable using ICD10 coding (and not names from diagnostic labels):
  # ICD10 for Diseases of the respiratory system it's J00-J99
  # Also include A15 (respiratory tuberculosis) and P22 (Respiratory distress of newborn) and R04 (Haemorrhage from respiratory passages) 

  # Create a variable that says how many diagnostics starting by J + a15 + p22 + r04 there are:

  a <- data_with_perm_id %>%
      filter(
          str_detect(toupper(diag4), "^J|A15|P22|R04") 
      ) %>% 
      pull(., diag4) %>% 
      unique() 

  b <- data_with_perm_id %>%
      filter(
          str_detect(toupper(diag3), "^J|A15|P22|R04") 
      ) %>% 
      pull(., diag3) %>% 
      unique()

  c <- data_with_perm_id %>%
      filter(
          str_detect(toupper(diag2), "^J|A15|P22|R04") 
      ) %>% 
      pull(., diag2) %>% 
      unique() 

  d <- data_with_perm_id %>%
      filter(
          str_detect(toupper(diag1), "^J|A15|P22|R04") 
      ) %>% 
      pull(., diag1) %>% 
      unique() 
  
  diag_a <- diag %>%
      filter(str_detect(toupper(diag_ct), "^J|A15|P22|R04")) %>%
      pull(., diag) %>%
      unique()

  every <- toupper(unique(c(a, b, c, d, diag_a)))

  every[!(every %in% no_j_includes)] %>% as.vector()

}


# Get most common household asset and gps coordinates with max accuracy
# getMostCommon <- function(x) {
#   freq <- table(x)
#   if (sum(freq) == 0) {
#     return(NA)
#   } else {
#     return(names(freq)[which.max(freq)][1])
#   }
# }

getLessError <- function(x, acc) {
  if(all(is.na(x))){
    return(NA)
  }else{
    return(x[which.min(acc)])
  }
}

##########################################################################
##########################################################################
## Main: support functions to made the models 
##  AND analisys of distribution 
##
## date: 23/05/2023 #nolint
##
## Autor: Fabian Francisco Coloma Velez
##########################################################################
##########################################################################

##' this function will join the data base for paper 2,
##' and the Family inestability data created on the before target
##'
##' @title joining_data_and_fi
##' @param data_treat data. that contains all the information of childrens
##' @param data_fi family adversity data prepared
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame
##' @author Fabian Coloma
##' @export
##' @examples
##'
joining_data_and_fi <- function(
    data_treated,
    data_fi
  ){
    data_treated %>%
        left_join(
            data_fi,
            by = join_by("individual_id" == "perm_id")
        ) 

  }


##' From a table add a variable o list of varibles mading just a line of code
##' that you can add a list of variable by a key join
##'
##' @title join new atributte data
##' @param new_data Data base that remain all fields
##' @param old_data Data that have the new attributes.
##' @param join_field Key field that is used to join the data.
##' @param fields_to_add Name of field o list of names that we want join.
##' @return A data frame.
##' @author Fabian Coloma
join_new_attribute_data <- function(new_data, old_data, join_field = "perm_id", fields_to_add = "healthdist"){ # nolint

    data_final <- new_data %>%
        left_join(
            old_data %>%
                select(
                   all_of(join_field),
                    all_of(fields_to_add)
                ) %>%
                distinct(),
            by = join_field
        )

    data_final
}

# TODO: how change the function of mean....

##' This function add a new variable with missing values imputed by 
##' a field, normally it will be a region or place variable.
##'
##' @title Impute missing mean
##' @param data data. Data base that remain all fields
##' @param field_to_group character. Region or group fields, that is calculated the mean.
##' @param field_to_imputted character. Name of the field that we want impute.
##' @return A data frame.
##' @author Fabian Coloma
impute_missing_mean  <- function(
    data,
    field_to_group = "region",
    field_to_inputted,
    general_imput = FALSE
){ # nolint

    general_imput_mean <- mean(pull(data, field_to_inputted), na.rm = TRUE)

    var_input <- paste0(
        field_to_inputted,
        "_mean_var"
    )

    dat_mean <- data %>%
        select(
            all_of(field_to_group),
            all_of(field_to_inputted)
        ) %>%
        group_by(
           get(field_to_group)
            
        ) %>%
        summarise(
            !!var_input := mean(
                get(field_to_inputted),
                na.rm = TRUE
            )
        )

   names(dat_mean) <- c(field_to_group, var_input)

    var_imputted <- paste0(
        field_to_inputted,
        "_final"
    )

    # data_to <- data %>% 
    #     left_join(
    #         dat_mean,
    #         by = field_to_group
    #     ) %>%
    #     mutate(
    #         !!var_imputted := case_when(
    #             is.na(get(field_to_inputted)) ~ get(var_input),
    #             TRUE ~ get(field_to_inputted)
    #         )
    #     ) %>% as.data.frame()
        
        # after 3 hours looking for the error -- 
        # we have learn the diference of data_fu_complet and data_fu_complete xd
    if(!(general_imput)){

      data_to <- data %>% 
          left_join(
              dat_mean,
              by = field_to_group
          ) %>%
          mutate(
              !!var_imputted := case_when(
                  is.na(get(field_to_inputted)) ~ get(var_input),
                  TRUE ~ get(field_to_inputted)
              )
          ) %>% as.data.frame()

    } else {
      data_to <- data %>% 
          left_join(
              dat_mean,
              by = field_to_group
          ) %>%
          mutate(
              !!var_imputted := case_when(
                  (is.na(get(field_to_inputted)) & is.na(get(var_input))) ~ general_imput_mean,
                  is.na(get(field_to_inputted)) ~ get(var_input),
                  TRUE ~ get(field_to_inputted)
              )
          ) %>% as.data.frame()
    }



    data_to %>%
      select(-all_of(c(var_input, field_to_inputted)))
        # %>%
        # dplyr::select(
        #     dplyr::all_of(
        #         base::names(data_to)[!(base::names(data_to) == var_input)]
        #         )
        #     ) 


}

##' Formating the principal variables used on the cohort
##' the main of configuration are the date fields, 
##' calculating the days of follow up period
##' factoring education and scaling numeric variable as health distance
##'
##' @title process model variables
##' @param data Data base that we wants transform
##' @return A data frame.
##' @author Fabian Coloma
process_model_variables <- function(data){ # nolint
    data %>%
        mutate(
            end_date_fu = as.Date(end_date_fu),
            child_dob = as.Date(child_dob),
            fu_days = as.numeric(
                difftime(
                    end_date_fu,
                    child_dob, 
                    units = c("days")
                )
            ),
            mother_age_at_birth_of_child = floor(as.numeric(
                difftime(
                    child_dob,
                    mother_dob,
                    units = c("days")
                )
            ) / 365.25),
            year_birth = as.factor(lubridate::year(child_dob)),
            healthdist_final_std = scale(healthdist_final),
            mother_education_cat = case_when(
                mother_education=="" ~ "No education",
                mother_education=="999" ~ "No education",
                mother_education=="88" ~ "Primary",
                mother_education=="98" ~ "Primary",
                mother_education=="1" ~ "Primary",
                mother_education=="2" ~ "Primary",
                mother_education=="3" ~ "Primary",
                mother_education=="4" ~ "Primary",
                mother_education=="5" ~ "Primary",
                mother_education=="6" ~ "Primary",
                mother_education=="7" ~ "Primary",
                mother_education=="8" ~ "Primary",
                mother_education=="9" ~ "Primary",
                mother_education=="10" ~ "Primary",
                mother_education=="11" ~ "Primary",
                mother_education=="12" ~ "Secondary",
                mother_education=="13" ~ "Secondary",
                mother_education=="14" ~ "Secondary",
                mother_education=="15" ~ "Secondary",
                mother_education=="16" ~ "Secondary",
                mother_education=="17" ~ "Secondary",
                mother_education=="18" ~ "Secondary",
                mother_education=="19" ~ "Secondary",
                mother_education=="20" ~ "Secondary",
                mother_education=="21" ~ "Secondary",
                mother_education=="22" ~ "Secondary",
                mother_education=="23" ~ "Secondary",
                mother_education=="24" ~ "Secondary",
            )
        ) %>%
        filter(
            end_date_fu >= child_dob
        )
}

##' Formating the principal variables used on the cohort
##' the main of configuration are the date fields, 
##' calculating the days of follow up period
##' factoring education and scaling numeric variable as health distance
##'
##' @title process model variables
##' @param data Data base that we wants transform
##' @return A data frame.
##' @author Fabian Coloma
process_model_variables2 <- function(data){ # nolint
    data %>%
        mutate(
            end_date_fu = as.Date(end_date_fu),
            child_dob = as.Date(dob),
            fu_days = as.numeric(
                difftime(
                    end_date_fu,
                    child_dob, 
                    units = c("days")
                )
            ),
            mother_age_at_birth_of_child = floor(as.numeric(
                difftime(
                    child_dob,
                    mother_dob,
                    units = c("days")
                )
            ) / 365.25),
            year_birth = as.factor(lubridate::year(child_dob)),
            healthdist_final_std = scale(healthdist_final)
        ) %>%
        filter(
            end_date_fu >= child_dob
        )
}


##' Formating the principal variables used on the attributes data
##' Just confirming that all the character have the same format 
##'
##' @title confirm yes o not
##' @param data data. data frame base
##' @param field character. name of the field that we want unify the format
##' @return A data frame.
##' @author Fabian Coloma
confirm_yes_no <- function(data, field){
    with(
        data,
        case_when(
            toupper(get(field)) %in% c("Yes", "yes", "YES") ~ "Yes",
            TRUE ~ "No"
        )
    )
}

##########################################################################
##########################################################################
# ECONOMICS ASSETS PREPARATION AND VARIABLES CREATION
##########################################################################
##########################################################################

##' That function process the data of economy assets, the aim is selecting the principal fields of study
##'
##' @title econ assets preparation
##' @param hh_id_and_perm_id_fu_v vector. is a vector with a list of all perm id and houses hold 
##' that we will include on the final cohort
##' @param econ_assets_clean data. Data frame with economic information
##' @return A data frame.
##' @author Fabian Coloma
econ_assets_preparation <- function(hh_id_and_perm_id_fu_v, econ_assets_clean, list_variables){
    
    data_pre_selected <- hh_id_and_perm_id_fu_v %>%
        inner_join(
            econ_assets_clean %>%
                select(
                    all_of(
                         list_variables
                    )
                ),
            by = c("household_all" = "house_number"),
            multiple = "all"
        ) 

    list_variables_with_has <- list_variables[str_detect(list_variables, "has")]
    
    for(i in 1:length(list_variables_with_has)){
        data_pre_selected2 <- data_pre_selected %>%
        mutate(
            !!list_variables_with_has[i] :=  confirm_yes_no(., list_variables_with_has[i])
        )
    }
     

    data_pre_selected2

}

econ_assets_create_mca_variables_paper1 <- function(pre_mca_econ_assets){

    pre_mca_econ_assets %>%
        mutate(
            has_tv_mca = has_tv,
            has_radio_mca = has_radio,
            has_computer_mca = has_computer,
            has_bike_mca = has_bike,
            has_farm_of_commercial_production_mca = has_farm_of_commercial_production, #nolint
            has_glacier_freezer_mca = case_when(
                    has_glacier == "Yes" | has_freezer == "Yes" ~ "Yes",
                    TRUE ~ "No"
                ),

            # has_celular_telephone
            has_celular_telephone_mca = case_when(
                    has_celular == "Yes" | has_telephone == "Yes" ~ "Yes",
                    TRUE ~ "No"
                ),
            # has_livestock
            has_livestock_mca = case_when(
                    has_pigs == "Yes" |
                    has_goat == "Yes" |
                    has_cattle == "Yes" ~ "Yes",
                    TRUE ~ "No"
                ),
            # hasMotoCar
            has_motoCar_mca = case_when(
                    has_moto == "Yes" |
                    has_car == "Yes" ~ "Yes",
                    TRUE ~ "No"
                )
        )

    
}


econ_assets_create_mca_variables_paper2 <- function(pre_mca_econ_assets){

    pre_mca_econ_assets %>%
        mutate(
            has_tv_mca = has_tv,
            has_radio_mca = has_radio,
            has_dvd_mca = has_dvd,
            has_computer_mca = has_computer,
            has_bike_mca = has_bike,
            has_farm_of_commercial_production_mca = has_farm_of_commercial_production, #nolint
            has_glacier_freezer_mca = case_when(
                    has_glacier == "Yes" | has_freezer == "Yes" ~ "Yes",
                    TRUE ~ "No"
                ),

            # has_celular_telephone
            has_celular_telephone_mca = case_when(
                    has_celular == "Yes" | has_telephone == "Yes" ~ "Yes",
                    TRUE ~ "No"
                ),
            # has_livestock
            has_livestock_mca = case_when(
                    has_pigs == "Yes" |
                    has_goat == "Yes" |
                    has_cattle == "Yes" ~ "Yes",
                    TRUE ~ "No"
                ),
            # hasMotoCar
            has_motoCar_mca = case_when(
                    has_moto == "Yes" |
                    has_car == "Yes" ~ "Yes",
                    TRUE ~ "No"
                ),
            has_tractor_mca = has_tractor
            
        )

    
}

##########################################################################
##########################################################################
# HOUSE FEATURES PREPARATION AND VARIABLES CREATION
##########################################################################
##########################################################################

##' That function process the data of house features, the aim is selecting the principal fields of study
##'
##' @title houses preparation to mca
##' @param hh_id_and_perm_id_fu_v vector. is a vector with a list of all perm id and houses hold 
##' that we will include on the final cohort
##' @param houses_clean data. Data frame with economic information
##' @return A data frame.
##' @author Fabian Coloma
houses_preparation_to_mca <- function(hh_id_and_perm_id_fu_v, houses_clean){
    hh_id_and_perm_id_fu_v %>%
        inner_join(
            houses_clean %>%
                select(
                house_number,
                floor_material, #
                wall_material, #
                kitchen_fuel, #
                coverage_material,
                has_kitchen, #
                is_kitchen_inside, #
                wat_treatment_proc,
                poo_treatment,
                trash_treatment,
                water_source #
                ),
            by = c("household_all" = "house_number"),
            multiple = "all"
        )
}

houses_mca_variables_creation_paper2 <- function(data){

    # floor_material
    data$floor_mat_mca <- data$floor_material

    # wall_material

    data$wall_mat_mca <- data$wall_material

    # kitchen_type

    data$kitchen_type_mca <- data$kitchen_type

    # water_treatment

    data$wat_treatment_proc_mca <- data$wat_treatment_proc

    # #sanitation

    data$poo_treatment_mca <- data$poo_treatment

    # #trash_treatment

    data$trash_treatment_mca <- data$trash_treatment

    # #water_source

    data$water_source_mca <- data$water_source

    # # coerage_material

    data$coverage_mat_mca <- data$coverage_material

    # # kitchen_fuel
    # table(pre_mca_houses_clean$kitchen_fuel)

    data$kitchen_fuel_cat_mca <- data$kitchen_fuel
    
    data

}


# houses_mca_variables_creation <- function(pre_mca_houses_clean){

#     # floor_material
#     pre_mca_houses_clean$floor_mat_mca <- with(
#         pre_mca_houses_clean, 
#         case_when(
#             floor_material %in% 
#             c("Cement", 
#             "Concrete", 
#             "Marmor", 
#             "Mosaic tiles") ~ "Improved",
#             floor_material %in% 
#             c("Wood", 
#             "Adobe", 
#             "Nothing", 
#             "Other") ~ "Unimproved",
#             TRUE ~ "Unimproved"
#         )
#     )

#     # table(pre_mca_houses_clean$floor_mat_mca)

#     # wall_material

#     # table(pre_mca_houses_clean$wall_material)

#     pre_mca_houses_clean$wall_mat_mca <- with(
#         pre_mca_houses_clean, 
#         case_when(
#             wall_material %in% 
#             c(
#                 "Cement blocks",
#                 "Bricks") ~ "Improved",
#             wall_material %in% 
#             c(
#                 "Wood/Zinc",
#                 "Adobe",
#                 "Bamboo/palm leafs", 
#                 "Maticado sticks", 
#                 "Paper bags",
#                 "Other") ~ "Unimproved",
#             TRUE ~ "Unimproved"
#         )
#     )

#     # table(pre_mca_houses_clean$wall_mat_cat)

#     # kitchen_type

#     # table(pre_mca_houses_clean$has_kitchen)
#     # table(pre_mca_houses_clean$is_kitchen_inside)

#     pre_mca_houses_clean$kitchen_type_mca <- with(
#         pre_mca_houses_clean, 
#         case_when(
#             has_kitchen == "No" ~ "no kitchen",
#             has_kitchen == "Yes" & 
#             is_kitchen_inside ==  "Yes" ~ "inside",
#             has_kitchen == "Yes" & 
#             is_kitchen_inside == "No" ~ "outside",
#             TRUE ~ "no kitchen"
#         )
#     )

#     # sum(table(pre_mca_houses_clean$has_kitchen))

#     # sum(table(pre_mca_houses_clean$kitchen_type_mca))

#     # water_treatment

#     # table(pre_mca_houses_clean$wat_treatment_proc)

#     pre_mca_houses_clean$wat_treatment_proc_mca <- with(
#         pre_mca_houses_clean, 
#         case_when(
#             wat_treatment_proc %in% 
#             c(
#                 "Boil water",
#                 "Cholorination") ~ "Improved",
#             wat_treatment_proc %in% 
#             c(
#                 "Don't know", 
#                 "Filtration", 
#                 "No treatment",
#                 "Traditional methods",
#                 "Don't know") ~ "Unimproved",
#                 TRUE ~ "Unimproved"
#         )
#     )

#     # table(pre_mca_houses_clean$wat_treatment_proc)
#     # sum(table(pre_mca_houses_clean$wat_treatment_proc))

#     # #sanitation
#     # table(pre_mca_houses_clean$poo_treatment)
#     # sum(table(pre_mca_houses_clean$poo_treatment))

#     pre_mca_houses_clean$poo_treatment_mca <- with(
#         pre_mca_houses_clean,
#         case_when(
#             poo_treatment %in% 
#             c(
#                 "Hire septic tank cleaning services") ~ "Improved",
#             poo_treatment %in% 
#             c(
#                 "Close latrine hole and open another one",
#                 "Household member does cleaning",
#                 "Don't know",
#                 "Refused to answer") ~ "Unimproved",
#                 TRUE ~ "Unimproved"
#         )
#     )

#     # table(pre_mca_houses_clean$poo_treatment_mca)
#     # sum(table(pre_mca_houses_clean$poo_treatment_mca))

#     # #trash_treatment

#     # table(pre_mca_houses_clean$trash_treatment)
#     # sum(table(pre_mca_houses_clean$trash_treatment))

#     pre_mca_houses_clean$trash_treatment_mca <- with(
#         pre_mca_houses_clean, 
#         case_when(
#             trash_treatment %in% 
#             c(
#                 "Collected by municipality",
#                 "Partly recycled") ~ "Improved",
#             trash_treatment %in% 
#             c(
#                 "Burned",
#                 "Burried",
#                 "Deposited in an improved trash deposit site in HH yard",
#                 "Left in the open",
#                 "Used as fertilizer") ~ "Unimproved",
#             TRUE ~ "Unimproved"
#         )
#     )

#     # table(pre_mca_houses_clean$trash_treatment_mca)
#     # sum(table(pre_mca_houses_clean$trash_treatment_mca))

#     # #water_source
#     # table(pre_mca_houses_clean$water_source)
#     # sum(table(pre_mca_houses_clean$water_source))

#     pre_mca_houses_clean$water_source_mca <- with(
#         pre_mca_houses_clean,
#         case_when(
#             water_source %in% 
#             c(
#                 "Piped water outside the house",
#                 "Piped water inside the house",
#                 "Fountain",
#                 "Water well manual pump", 
#                 "Well/hole dependent on a pump") ~ "Improved",
#             water_source %in% 
#             c(
#                 "Don't know",
#                 "River/lake/pond") ~ "Unimproved",
#             TRUE ~ "Unimproved"
#         )
#     )

#     # table(pre_mca_houses_clean$water_source_mca)
#     # sum(table(pre_mca_houses_clean$water_source_mca))

#     # # coerage_material

#     # table(pre_mca_houses_clean$coverage_material)

#     pre_mca_houses_clean$coverage_mat_mca <- with(
#         pre_mca_houses_clean, 
#         case_when(
#             coverage_material %in% 
#             c("Concrete", 
#             "Tiles") ~ "Improved",
#             coverage_material %in% 
#             c("Grass", 
#             "Lusalite sheet", 
#             "Zinc sheet", 
#             "Other") ~ "Unimproved",
#             TRUE ~ "Unimproved"
#         )
#     )

#     # # kitchen_fuel
#     # table(pre_mca_houses_clean$kitchen_fuel)

#     pre_mca_houses_clean$kitchen_fuel_cat_mca <- with(
#         pre_mca_houses_clean, 
#         case_when(
#             kitchen_fuel %in% 
#             c("Electricity") ~ "Clean",
#             kitchen_fuel %in% 
#             c("Coal", 
#             "Firewood/timber", 
#             "No fuel", 
#             "Gas",
#             "Paraffin",
#             "Don't know") ~ "Polluting"
#         )
#     )

#     return(pre_mca_houses_clean)

# }

treat_multi_dob <- function(data){

    perm_id_duplicate <- data %>% 
        select(
            perm_id,
            dob
        ) %>%
        unique() %>%
        group_by(
            perm_id
        ) %>%
        count() %>%
        filter(
            n >  1
        ) %>%
        pull(., perm_id)
    
    data_multiple_date_solv <- data  %>%
        filter(
            perm_id %in% perm_id_duplicate
        ) %>%
        getMostCommonSurveyResponse(
            .,
            perm_id,
            c("dob")
        )

    data_fin <- cbind()

    # CONTINUAR ---- 
}


status_data_mange_row_miss <- function(data, pos_prev = TRUE, desc_status = ""){

    flow_chart <- data.frame(
        id = desc_status,
        dimension = nrow(data)
    )

    miss_summary <- get_summary_missing(data)

    miss_summary$var_names <- rownames(miss_summary)

    if(pos_prev == TRUE){
        pos <- as.character(as.numeric(pos)+1)
    } else{
        pos <- as.character(1)
    }

    name_file_miss <- paste(
        pos,
        str_replace_all(desc_status, " ", "_"),
        sep = "_"
    )


    write_csv(miss_summary, here(
        # "FC-manhica_child_adversity_data",
        "outputs",
        "missing_status_by_steps",
        paste0(
            name_file_miss,
            ".csv"
            )
        )
    )

    pos

}


##########################################################################
##########################################################################
# estandarizando pesos de los individuos
##########################################################################
##########################################################################


standardise_weight <- function(
   fu_data,
   data_weight
){
    calcul_prev <- data_weight %>% 
    mutate(
        age_on_event = as.numeric(difftime(
                                date_event,
                                birth_date_v,
                                units = "days"
                            ))
    ) %>%
        select(
            perm_id,
            age_on_event,
            weigth
        ) %>%
        left_join(
            fu_data %>% 
                select(perm_id, gender) %>%
                distinct(),
            by = "perm_id"
        ) %>%
        mutate(
            gender = as.character(gender)
        ) %>%
        filter(
            weigth >=0,
            weigth <= quantile(data_weight$weigth, 0.99),
            age_on_event >= 0,
            !is.na(gender)
        )
    
    estandar <- with(
        calcul_prev,
            anthro_zscores(
                sex = gender,
                age = age_on_event,
                weight = weigth 
            )) %>% 
        select(zwei)

    calcul_prev %>%
    cbind(., estandar) 
}


##########################################################################
##########################################################################
# tratando - environment data
##########################################################################
##########################################################################


treatment_data_environment <- function(data, min_alan_lecture = 7){

    data %>%
      mutate(
        
        artificial_light_at_night_2015_500 = ifelse(alan_2015_500 <= min_alan_lecture | is.na(alan_2015_500), min_alan_lecture / 2, alan_2015_500), # are normal .. for this we use half if not was normal it should be  divided by sqrt(2)
        artificial_light_at_night_2020_500 = ifelse(alan_2020_500 <= min_alan_lecture | is.na(alan_2020_500), min_alan_lecture / 2, alan_2020_500),
        artificial_light_at_night_2015_1000 =  ifelse(alan_2015_1000 <= min_alan_lecture| is.na(alan_2015_1000), min_alan_lecture / 2, alan_2015_1000),
        artificial_light_at_night_2020_1000 =  ifelse(alan_2020_1000 <= min_alan_lecture| is.na(alan_2020_1000), min_alan_lecture / 2, alan_2020_1000),

        artificial_light_at_night_diff_500 = artificial_light_at_night_2020_500 - artificial_light_at_night_2015_500,
        artificial_light_at_night_diff_1000 = artificial_light_at_night_2020_1000 - artificial_light_at_night_2015_1000,

        imperviousness_density_300 = imp_300 * 100,
        imperviousness_density_500 = imp_500 * 100,

        tree_cover_300 = round((buff300m_10 / (300 ^ 2)) * 100, 1),
        shrubland_300 = round((buff300m_20 / (300 ^ 2)) * 100, 1),
        grassland_300 = round((buff300m_30 / (300 ^ 2)) * 100, 1),
        cropland_300 = round((buff300m_40 / (300 ^ 2)) * 100, 1),
        build_up_300 = round((buff300m_50 / (300 ^ 2)) * 100, 1),
        bareland_300 = round((buff300m_60 / (300 ^ 2)) * 100, 1),
        permanent_water_bodies_300 = round((buff300m_80 / (300 ^ 2)) * 100, 1),
        
        tree_cover_500 = round((buff500m_10 / (500 ^ 2)) * 100, 1),
        shrubland_500 = round((buff500m_20 / (500 ^ 2)) * 100, 1),
        grassland_500 = round((buff500m_30 / (500 ^ 2)) * 100, 1),
        cropland_500 = round((buff500m_40 / (500 ^ 2)) * 100, 1),
        build_up_500 = round((buff500m_50 / (500 ^ 2)) * 100, 1),
        bareland_500 = round((buff500m_60 / (500 ^ 2)) * 100, 1),
        permanent_water_bodies_500 = round((buff500m_80 / (300 ^ 2)) * 100, 1),

        ndvi_2015_300 = ndvi_2015_300 * 100,
        ndvi_2020_300 = ndvi_2020_300 * 100,
        ndvi_2015_500 = ndvi_2015_500 * 100,
        ndvi_2020_500 = ndvi_2020_500 * 100,

        ndvi_diff_300 = ndvi_2020_300 - ndvi_2015_300,
        ndvi_diff_500 = ndvi_2020_500 - ndvi_2015_500


    )

}


###
# Variables clasification
###

# alan_vars <- c(
#   "alan_2015_500",
#   "alan_2020_500",
#   "alan_2015_1000",
#   "alan_2020_1000"
#   )

# imp_vars <- c(
#   "imp_300",
#   "imp_500"
#   )

# buff_vars <- c(
#   "buff300m_10",
#   "buff300m_20",
#   "buff300m_30",
#   "buff300m_40",
#   "buff300m_50",
#   "buff300m_60",
#   "buff300m_80",
#   "buff500m_10",
#   "buff500m_20",
#   "buff500m_30",
#   "buff500m_40",
#   "buff500m_50",
#   "buff500m_60",
#   "buff500m_80"
#   )

# ndvi_vars <- c(
#   "ndvi_2015_300",
#   "ndvi_2020_300",
#   "ndvi_2015_500",
#   "ndvi_2020_500"
#   )

# alan_vars <- c(
#   "artificial_light_at_night_2015_500",
#   "artificial_light_at_night_2020_500",
#   "artificial_light_at_night_2020_1000",
#   "artificial_light_at_night_2020_1000"
#   )

# imp_vars <- c(
#   "imperviousness_density_300",
#   "imperviousness_density_500"
#   )

# buff_vars <- c(
#   "tree_cover_300",
#   "shrubland_300",
#   "grassland_300",
#   "cropland_300",
#   "build_up_300",
#   "bareland_300",
#   "permanent_water_bodies_300",
#   "tree_cover_500",
#   "shrubland_500",
#   "grassland_500",
#   "cropland_500",
#   "build_up_500",
#   "bareland_500",
#   "permanent_water_bodies_500"
#   )

# ndvi_vars <- c(
#   "ndvi_2015_300",
#   "ndvi_2020_300",
#   "ndvi_2015_500",
#   "ndvi_2020_500"
#   )




# -- -- -- -- ---------------------------------------
#  list <-  treat_head_ocupation_All(
#     heads_id = data_final_to_mod_v2$head_perm_id,
#     member_history = member_details_history_raw_v2
#   )
# -- -- -- -- ---------------------------------------

treat_head_ocupation_All <- function(heads_id, member_history){


# Filter data to keep only "Unemployed" occupations
unemployed_data <-  member_history %>%
    filter(
      perm_id %in% heads_id
    ) %>%
    select(
      creation_date = sys_odk_creation_date,
      perm_id,
      ordinal,
      ocupation

    ) %>%
  filter(ocupation == "Unemployed")

# Group by perm_id and count occurrences of "Unemployed" occupations
unemployed_counts <- unemployed_data %>%
  group_by(perm_id) %>%
  summarise(
    count_unemployed = n(),
    consecutive_years = any(diff(ordinal) == 1)  # Check if the ordinals are consecutive
  )

perm_ids_two_consecutive_unemployed <- unemployed_counts %>%
  filter(count_unemployed >= 2 & consecutive_years) %>%
  pull(perm_id)

perm_ids_two_consecutive_unemployed

}

# -- -- -- -- ---------------------------------------
  # data <- process_data_to_mca_socio_econ_paper_2(
  #   data_with_all_perms_id = data_fu_final_v2,
  #   mca_paper_2_socio = mca_1_v2,
  #   id_with_head_perm_id_long_Unemployed  = head_perm_id_long_Unemployed

  # )
# -- -- -- -- ---------------------------------------

##' add function description
##'
##'
##' @title process_data_to_mca_socio_econ_paper_2
##' @param data_with_all_perms_id data all data
##' @param mca_paper_2_socio datmca data
##' @param id_with_head_perm_id_long_Unemployed id wiot long unemploymente
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
process_data_to_mca_socio_econ_paper_2 <- function(
    data_with_all_perms_id,
    mca_paper_2_socio,
    id_with_head_perm_id_long_Unemployed 
  ){

    data_info_general <- data_with_all_perms_id %>%
      select(
        perm_id,
        mother_education_mca = mother_education,
        long_head_unemployed_mca = long_head_unemployed
      )
    
    mca_paper_2_socio %>%
      left_join(
        data_info_general,
        by = "perm_id"
      )
  }



#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    PCA - and environment description
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#


##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
want_exe_version <- function(version_name){

  fileConn <- file(here::here("setting_files", "paths_ref.R"))

  writeLines(paste0("where_to_read <- '", version_name, "'"), fileConn)

  close(fileConn)

}


##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
basic_transfor_to_sf_mozambique <- function(data){

  data %>%
    st_as_sf(
      .,
      coords = c("gps_longitude", "gps_latitude"),
      crs = 4326
    ) %>%
    st_transform(
      crs = 32736  # WGS84 UTM 36S
    )

}


##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
add_main_pca <- function(data){
    
    alan_vars <- c(
    #   "artificial_light_at_night_2015_500",
    #   "artificial_light_at_night_2020_500",
    "artificial_light_at_night_2015_1000",
    # "artificial_light_at_night_2020_1000",
    "artificial_light_at_night_diff_1000"
    )

    imp_vars <- c(
    #   "imperviousness_density_300",
    "imperviousness_density_500"
    )

    buff_vars <- c(
    # "tree_cover_300",
    #   "shrubland_300",
    #   "grassland_300",
    #   "cropland_300",
    # "build_up_300",
    #   "bareland_300",
    #   "permanent_water_bodies_300",
    # # "tree_cover_500",
    # "shrubland_500",
    # "grassland_500",
    "cropland_500"
    
    # # "build_up_500",
    # "bareland_500",
    # # "permanent_water_bodies_500"
    )

    ndvi_vars <- c(
    #   "ndvi_2015_300",
    #   "ndvi_2020_300",
    "ndvi_2015_500",
    "ndvi_diff_500"
    # "ndvi_2020_500"
    )

    distances <-  c(
    # "healthdist_final",
    "roaddist_km"
    # ,
    # "sugardist_final"  # no en ###
    )

    buffe_neihg <- c(
    "Nneigh_500_final"
    # # # ,
    # # "ilumin_500_final"
    # "kitchen_500_final"
    )

    all_names <- c("perm_id", alan_vars, imp_vars, buff_vars, ndvi_vars, distances, buffe_neihg)

    all_names_env <- c(
    alan_vars,
    imp_vars,
    buff_vars,
    ndvi_vars,
    distances,
    buffe_neihg
    )

    pc <- prcomp(
    data[all_names_env], 
    center = TRUE,
    scale. = TRUE
    )


    data$PC1 <- pc$x[,1]
    data$PC2 <- pc$x[,2]
    data$PC3 <- pc$x[,3]
    data$PC4 <- pc$x[,4]
    data$PC5 <- pc$x[,5]
    data$PC6 <- pc$x[,6]
    data$PC7 <- pc$x[,7]


    data

}

##' adding and saving pca result at the moment we hace 7 pc included
##' in here que are saving the biplot too
##'
##' @title add_pca_and_save
##' @param data data.frame were is included all the variables we need
##' @param where_to_save identificable name of the iteration
##' @param alan_vars list of variables from litgh at nigth
##' if is FALSE the list are default
##' @param imp_vars list of variables fron the block ... 
##' if is FALSE the list are default
##' @param buff_vars list of variables fron the block ... 
##' if is FALSE the list are default
##' @param ndvi_vars list of variables fron the block ... 
##' if is FALSE the list are default
##' @param distances list of variables fron the block ... 
##' if is FALSE the list are default
##' @param buffe_neihg list of variables fron the block ... 
##' if is FALSE the list are default
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
add_pca_and_save <- function(
    data,
    where_to_save,
    alan_vars = FALSE,
    imp_vars = FALSE,
    buff_vars = FALSE,
    ndvi_vars = FALSE,
    distances = FALSE,
    buffe_neihg = FALSE
    ){
    
    if(alan_vars == FALSE){
        alan_vars <- c(
            #   "artificial_light_at_night_2015_500",
            #   "artificial_light_at_night_2020_500",
            "artificial_light_at_night_2015_1000",
            # "artificial_light_at_night_2020_1000",
            "artificial_light_at_night_diff_1000"
            )
    }

    if(imp_vars == FALSE){imp_vars <- c(
    #   "imperviousness_density_300",
    "imperviousness_density_500"
    )}

    if(buff_vars == FALSE){buff_vars <- c(
    # "tree_cover_300",
    #   "shrubland_300",
    #   "grassland_300",
    #   "cropland_300",
    # "build_up_300",
    #   "bareland_300",
    #   "permanent_water_bodies_300",
    # # "tree_cover_500",
    # "shrubland_500",
    # "grassland_500",
    "cropland_500"
    
    # # "build_up_500",
    # "bareland_500",
    # # "permanent_water_bodies_500"
    )}

    if(ndvi_vars == FALSE){ndvi_vars <- c(
    #   "ndvi_2015_300",
    #   "ndvi_2020_300",
    "ndvi_2015_500",
    "ndvi_diff_500"
    # "ndvi_2020_500"
    )}

    if(distances == FALSE){distances <-  c(
    # "healthdist_final",
    "roaddist_final"
    # ,
    # "sugardist_final"  # no en ###
    )}

    if(buffe_neihg == FALSE){buffe_neihg <- c(
    "Nneigh_500_final"
    # # # ,
    # # "ilumin_500_final"
    # "kitchen_500_final"
    )}

    all_names <- c("perm_id", alan_vars, imp_vars, buff_vars, ndvi_vars, distances, buffe_neihg)

    all_names_env <- c(
    alan_vars,
    imp_vars,
    buff_vars,
    ndvi_vars,
    distances,
    buffe_neihg
    )

    # Perform PCA
    pc <- prcomp(
    data[all_names_env], 
    center = TRUE,
    scale. = TRUE
    )

    # Perform PCA
  saveRDS(
    pc,
    here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  "pca_res.rds" 
  ))
    # Save PCA biplot
    ggplot2::ggsave(
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  "pca_biplot.jpeg" ),
      plot = ggbiplot::ggbiplot(pc)
    )

    # Save PCA summary
    

    # Contribution save
    write.csv(
      summary(pc)$importance,
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  "pca_contribution.csv")
    )

    # Contribution save
    write.csv(
      summary(pc)$rotation,
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  "pca_loadings.csv")
    )


    data$PC1 <- pc$x[,1]
    data$PC2 <- pc$x[,2]
    data$PC3 <- pc$x[,3]
    data$PC4 <- pc$x[,4]
    data$PC5 <- pc$x[,5]
    data$PC6 <- pc$x[,6]
    data$PC7 <- pc$x[,7]


    data

}

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    ADD_YOUR_DESCRIPTION_BLOCK
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##' TODO:: CAMBIAR TODOS LOS SAVE para que tengan que ver con el Environment
add_pca_and_save_2 <- function(data, where_to_save, list_variables, environment_end, many_dim = FALSE){

    all_names_env <- list_variables

    if(many_dim == FALSE){
      many_dim = length(list_variables)
    }

    # Perform PCA
    pc <- prcomp(
    data[all_names_env],
    center = TRUE,
    scale. = TRUE
    )

    # Perform PCA
  saveRDS(
    pc,
    here::here( 
      # "outputs",
      #             "paper_2_R_model_results",
                  where_to_save,
                  environment_end, 
                  "pca_res.rds" 
  ))
    # Save PCA biplot
    ggplot2::ggsave(
      here::here( 
        # "outputs",
        #           "paper_2_R_model_results",
                  where_to_save,
                  environment_end,
                  "pca_biplot.jpeg" ),
      plot = ggbiplot::ggbiplot(pc)
    )

    # Save PCA summary
    

    # Contribution save
    write.csv(
      summary(pc)$importance,
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  environment_end,
                  "pca_contribution.csv")
    )

    # Contribution save
    write.csv(
      summary(pc)$rotation,
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  environment_end,
                  "pca_loadings.csv")
    )


# TODO: automatic comprobation of 80% of explanation i <- 2
  for(i in 1:many_dim){
    if(i<10){
      var_name_new <- paste0(environment_end, "_DIM_0", as.character(i))
    }else{
      var_name_new <- paste0(environment_end, "_DIM_", as.character(i))
    }
    
    # print(var_name_new)
    data <- data %>%
      dplyr::mutate(
        !!var_name_new := pc$x[, i]
      )
  }



    data
}

add_pca_and_save_v3 <- function( data,
                                 all_scaled = TRUE, 
                                 where_to_save, 
                                 list_variables, 
                                 environment_end, 
                                 many_dim = FALSE,
                                 clusters_var = NULL){


    dir.create(here::here(where_to_save,
                          environment_end), 
                showWarnings = FALSE, 
                recursive = TRUE)

    all_names_env <- list_variables

    if(many_dim == FALSE){
      many_dim = length(list_variables)
    }

    # Perform PCA
    if(all_scaled == TRUE){
            pc <- prcomp(
    data[all_names_env],
    center = TRUE,
    scale. = TRUE
    )

    } else {
          pc <- prcomp(
    data[all_names_env],
    center = FALSE,
    scale. = FALSE
    )

    }


# Get PCA scores (observations)
pca_scores <- as.data.frame(pc$x[, 1:2])  # Only PC1 & PC2
pca_scores$labels <- rownames(data)  # Add row names if needed

# Extract variable loadings (arrows)
loadings <- as.data.frame(pc$rotation[, 1:2])  # Extract PC1 & PC2 loadings
loadings$var <- rownames(loadings)  # Add variable names

# Scale the loadings to match the PCA plot scale
scaling_factor <- max(abs(pca_scores$PC1), abs(pca_scores$PC2)) / max(abs(loadings$PC1), abs(loadings$PC2))
loadings[, 1:2] <- loadings[, 1:2] * scaling_factor * 0.35

if(!is.null(clusters_var)){

  # Create the improved biplot with color by a grouping variable
  p <- ggbiplot::ggbiplot(pc, var.scale = 2, var.axes = FALSE, 
                          groups = clusters_var) +  # Add color by variable
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 12), 
          legend.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 16),
          aspect.ratio = 1) +  # Make it wider
    labs(title = "PCA Biplot",
        x = "PC1",
        y = "PC2") +
    # Add arrows for variables
    ggplot2::geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                arrow = arrow(length = unit(0.3, "cm")), color = "red", alpha = 0.7) +
    # Repel variable names
    ggrepel::geom_text_repel(data = loadings, aes(x = PC1, y = PC2, label = var), 
                    color = "#008cff", size = 4, max.overlaps = 90, 
                    box.padding = 0.9)

} else {

  # Create the improved biplot
  p <- ggbiplot::ggbiplot(pc, var.scale = 2, var.axes = FALSE) +  # Disable default arrows
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 12), 
          legend.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 16),
          aspect.ratio = 1) +  # Make it wider
    labs(title = "PCA Biplot",
        x = "PC1",
        y = "PC2") +
    # Add arrows for variables
    ggplot2::geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                arrow = arrow(length = unit(0.3, "cm")), color = "red", alpha = 0.7) +
    # Repel variable names
    ggrepel::geom_text_repel(data = loadings, aes(x = PC1, y = PC2, label = var), 
                    color = "#008cff", size = 4, max.overlaps = 90, 
                    box.padding = 0.9)

}


# theme_minimal() +

# Save the plot with a wider format
ggsave(here::here( where_to_save,
                  environment_end,
                  "pca_biplot.png" ), p, width = 10, height = 10, dpi = 600)


    # Perform PCA
  saveRDS(
    pc,
    here::here(   where_to_save,
                  environment_end, 
                  "pca_res.rds" 
  ))
    # Save PCA biplot
    # ggplot2::ggsave(
    #   here::here( where_to_save,
    #               environment_end,
    #               "pca_biplot.jpeg" ),
    #   plot = ggbiplot::ggbiplot(pc)
    # )


# ggplot2::ggsave(
#   filename = here::here(where_to_save, "volcano_plot_hd.jpeg"),  # Specify file name
#   plot = ggbiplot::ggbiplot(pc),               # Save the last plot
#   dpi = 600,                        # Set resolution (dots per inch)
#   width = 15,                       # Set width of the image (in inches)
#   height = 12,                       # Set height of the image (in inches)
#   units = "in"                      # Units for width and height
# )

    # Save PCA summary
    

    # Contribution save
    write.csv(
      summary(pc)$importance,
      here::here( where_to_save,
                  environment_end,
                  "pca_contribution.csv")
    )

    # Contribution save
    write.csv(
      summary(pc)$rotation,
      here::here( where_to_save,
                  environment_end,
                  "pca_loadings.csv")
    )


# TODO: automatic comprobation of 80% of explanation i <- 2
  for(i in 1:many_dim){
    if(i<10){
      var_name_new <- paste0(environment_end, "_DIM_0", as.character(i))
    }else{
      var_name_new <- paste0(environment_end, "_DIM_", as.character(i))
    }
    
    # print(var_name_new)
    data <- data %>%
      dplyr::mutate(
        !!var_name_new := pc$x[, i]
      )
  }


list(
  data_all_pc = data,
  biplot_path = here::here( where_to_save,
                  environment_end,
                  "pca_biplot.png" ),

  contribution_info = summary(pc)$importance,
  loadings_info = summary(pc)$rotation,
  general_path = here::here( where_to_save,
                  environment_end )
)
    
}

##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##' TODO: add the posibility of calculate everything without 
##' TODO: made automatic slection of many_dim
add_mca_and_save <- function(
  data,
  # ends_var = "_mca",
  where_to_save, 
  # many_dim = 7,
  # restrict_variable = FALSE # max is the variable name
  list_variables,
  environment_end,
  many_dim = FALSE # max is the number of variables
){

  require(FactoMineR)
  require(factoextra)

  names_data_to_reduce <- list_variables

if(many_dim == FALSE){
  many_dim <- length(list_variables)
}

  data_to_mca <- data %>%
    dplyr::select(
      dplyr::all_of(names_data_to_reduce)
    )
  
  # ca::mjca()
  res.mca <- FactoMineR::MCA(data_to_mca, ncp = many_dim, graph = FALSE)
# names(res.mca)
  # summary(res.mca)

  name_save <- paste0("res_", environment_end, ".rds")

  saveRDS(
  res.mca,
  here::here(  "outputs",
               "paper_2_R_model_results",
                where_to_save,
                environment_end,
                name_save )
  )

  # res.mca$var$coord
  
  # eig.val <- get_eigenvalue(res.mca)

  # dim(res.mca$ind$coord)

# TODO: automatic comprobation of 80% of explanation i <- 2
  for(i in 1:many_dim){
    if(i < 10){
      var_name_new <- paste0(environment_end, "_DIM_0", as.character(i))
    } else {
      var_name_new <- paste0(environment_end, "_DIM_", as.character(i))
    }
    
    # print(var_name_new)
    data <- data %>%
      dplyr::mutate(
        !!var_name_new := res.mca$ind$coord[, i]
      )
  }
  # data$MCA1 rr <- res.mca$ind$coord[, 1]
  # data$MCA2 <- res.mca$ind$coord[, 2]
  # data$MCA3 <- res.mca$ind$coord[, 3]
  # data$MCA4 <- res.mca$ind$coord[, 4]
  # data$MCA5 <- res.mca$ind$coord[, 5]
  # data$MCA6 <- res.mca$ind$coord[, 6]
  # data$MCA7 <- res.mca$ind$coord[, 7]


    # Save PCA importance
    name_save <- paste0(environment_end, "_variance_importance.jpeg")

    ggplot2::ggsave(
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  environment_end,
                  name_save),
      plot = fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
    )

    # Save PCA variable location 
    name_save <- paste0(environment_end, "_variance_location_biplot.jpeg")

    ggplot2::ggsave(
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  environment_end,
                  name_save ),
      plot = fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
             )
    )

    # Save PCA biplot
    name_save <- paste0(environment_end, "_biplot.jpeg")

    ggplot2::ggsave(
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  environment_end,
                  name_save ),
      plot = factoextra::fviz_mca_biplot(res.mca, 
                repel = FALSE, # Avoid text overlapping (slow if many point)
                ggtheme = ggplot2::theme_minimal())
    )


  


  


  # The red dashed line on the graph above indicates the expected average value, 
  # If the contributions were uniform. The calculation of the expected contribution 
  # value, under null hypothesis, has been detailed in the principal component analysis 
  # chapter.

# TODO:: make a bucle to see n dimensions nos just 2
  # Contributions of rows to dimension 1
name_save <- paste0(environment_end, "_contribution_var_1.jpeg")
    ggplot2::ggsave(
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  environment_end,
                  name_save ),
      plot = fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
    )

  # Contributions of rows to dimension 2
name_save <- paste0(environment_end, "_contribution_var_2.jpeg")
    ggplot2::ggsave(
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  environment_end,
                  name_save  ),
      plot = fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
    )

  

# write.infile(res.mca, "mca.txt", sep = "\t")

  data

}

##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##' TODO: add the posibility of calculate everything without 
##' TODO: made automatic slection of many_dim
add_mca_and_save2 <- function(
  data,
  # ends_var = "_mca",
  where_to_save, 
  # many_dim = 7,
  # restrict_variable = FALSE # max is the variable name
  list_variables,
  environment_end,
  many_dim = FALSE # max is the number of variables
){

  #--------------------------------------------------------------------------#
  #--------------------------------------------------------------------------#
  #    MCA by library CA
  #--------------------------------------------------------------------------#
  #--------------------------------------------------------------------------#

  names_data_to_reduce <- list_variables

  if(many_dim == FALSE){
    many_dim <- length(list_variables)
  }

  data_to_mca <- data %>%
    dplyr::select(
      dplyr::all_of(names_data_to_reduce)
    )
  

  ##########
  ##########
  ## calculation MCA 
  ##########
  ##########
if(any(environment_end %in% c("HE"))){
   res.mca <- ca::mjca( data_to_mca, 
                       lambda = "indicator", 
                       nd = many_dim, 
                       graph = FALSE, 
                       maxit = 150)
}else{
   res.mca <- ca::mjca( data_to_mca, 
                       lambda = "indicator", 
                       nd = many_dim, 
                       graph = FALSE, 
                       maxit = 150)
}


  name_save <- paste0("results_", environment_end, ".rds")


  saveRDS(
  res.mca,
  here::here(  "outputs",
               "paper_2_R_model_results",
                where_to_save,
                environment_end,
                name_save )
  )

  ##########
  ##########
  ## creation name of dimension
  ##########
  ##########

  names_dimension <- paste0("DIM ", 1:res.mca$nd.max)

  ##########
  ##########
  ## Variance contribution
  ##########
  ##########

  variance_contribution <- data.frame(
    eigenvalues = res.mca$inertia.e,
    `percentage of variance` = res.mca$inertia.e * 100,
    `cumulative percentage of variance` = cumsum(res.mca$inertia.e)
  ) %>% t() %>% as.data.frame()

  names(variance_contribution) <- names_dimension


  write.csv(
    variance_contribution,
     here::here(  "outputs",
               "paper_2_R_model_results",
                where_to_save,
                environment_end,
                "var_contribution.csv" ) )

  
  ##########
  ##########
  ## ADD_YOUR_DESCRIPTION
  ##########
  ##########

  loadings_coordinates <- res.mca$colpcoord %>% as.data.frame() 
  
  names(loadings_coordinates) <- names_dimension

  rownames(loadings_coordinates) <- res.mca$levelnames

  write.csv(
    loadings_coordinates,
     here::here(  "outputs",
               "paper_2_R_model_results",
                where_to_save,
                environment_end,
                "column_cordinates.csv" ) )

  # CORR
  
  column_squared_correlations <-  res.mca$colcor %>% as.data.frame()

  names(column_squared_correlations) <- names_dimension

  rownames(column_squared_correlations) <- res.mca$levelnames

  write.csv(
    column_squared_correlations,
     here::here(  "outputs",
               "paper_2_R_model_results",
                where_to_save,
                environment_end,
                "column_squared_correlations.csv" ) )

  # CONTRIBUTIONS

  column_contribution <-  res.mca$colctr %>% as.data.frame()
  
  names(column_contribution) <- names_dimension

  rownames(column_contribution) <- res.mca$levelnames

  write.csv(
    column_contribution,
     here::here(  "outputs",
               "paper_2_R_model_results",
                where_to_save,
                environment_end,
                "column_contribution.csv" ) )


# It seems like you're referring to the output vectors from Multiple Correspondence Analysis (MCA). Let's break down what each of these vectors represents:

# Row Coordinates (rowcoord): These are the standard coordinates of the rows (categories or observations) in the original space. Each row will have coordinates in the original dimensions of the data. These coordinates represent the position of each row in the original space before dimension reduction.

# Row Principal Coordinates (rowpcoord): These are the principal coordinates of the rows. After applying MCA, the rows are projected onto the principal dimensions (components) of the reduced space. These coordinates represent the position of each row in the reduced-dimensional space.


# TODO: automatic comprobation of 80% of explanation i <- 2
  for(i in 1:dim(res.mca$rowpcoord)[2]){
    if(i < 10){
      var_name_new <- paste0(environment_end, "_DIM_0", as.character(i))
    } else {
      var_name_new <- paste0(environment_end, "_DIM_", as.character(i))
    }
    
    # print(var_name_new)
    data <- data %>%
      dplyr::mutate(
        !!var_name_new := res.mca$rowpcoord[, i]
      )
  }


  ##########
  ##########
  ## variance importance
  ##########
  ##########

    # Save PCA importance
    name_save <- paste0("Variance_importance.jpeg")

    ggplot2::ggsave(
      here::here( "outputs",
                  "paper_2_R_model_results",
                  where_to_save,
                  environment_end,
                  name_save),
      plot = fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 100))
    )


  ##########
  ##########
  ## diferents bi plots full creation
  ##########
  ##########

  # for(i in 1:dim(res.mca$rowpcoord)[2]){ i <- 1, j <- 2
  #   for(j in (i + 1):dim(res.mca$rowpcoord)[2]){
  if(dim(res.mca$rowpcoord)[2]<10){
    numero_to_iterate <- dim(res.mca$rowpcoord)[2]
  } else { 
    numero_to_iterate <- 6
  }
  for(i in 1:(numero_to_iterate-1)){
    for(j in (i + 1):numero_to_iterate){
      # Save PCA biplot
      name_save <- as.character(paste0("Biplot_", i, "_", j, ".jpeg"))
      
      print(paste0(i, " - ", j))

      
      jpeg( filename = here::here( "outputs",
                    "paper_2_R_model_results",
                    where_to_save,
                    environment_end,
                    name_save ), width = 650, height = 650 )
        plot(res.mca, dim = c(as.numeric(i),as.numeric(j)), contrib = "relative")
      dev.off()

    }
  }
   



  data

}

##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
geo_mapa_creation <- function(
  which_metric = "healthdist_final"
  ,filter_distribution_of_variable = 1 # from 0 to 1, left tail 
  ,n_cuts_to_color = 10
  ,participants_geo_data = participants_geo_data
  ,districts = districts
  ,manhica = manhica
  ,postos = postos
  ,highway = highway
  ,roads = roads
  ,main_hosp = main_hosp
  ,other_hosp = other_hosp
  ,apstation = apstation
  ,sugar = sugar
  ,gradient_color_def = "-RdYlBu"
){

  # print(which_metric)

  metrict_vec <- dplyr::pull(participants_geo_data, which_metric)

# TODO:: no se que cojones pasa pero hay que ver - el resultado es un vecto .. 
# DONE
  quantile_to_filter <- as.numeric(quantile(metrict_vec, filter_distribution_of_variable))

  # print(quantile_to_filter)

    participants_geo_data <- participants_geo_data %>%
        filter(
            get(which_metric) <=  quantile_to_filter
            )

#  print(dim(participants_geo_data))

  metrict_vec <- dplyr::pull(participants_geo_data, which_metric)

  min_metric <- floor(min(metrict_vec))
  # print(min_metric)
  max_metric <-  1 + ceiling(max(metrict_vec))
  # print(max_metric)

  breaks <- (max_metric - min_metric) / n_cuts_to_color

# print(breaks)
  
  seq_to_plot <- seq(min_metric, max_metric, breaks)

# print(seq_to_plot)


plot <- tm_shape(districts, bbox = manhica) +
  tm_polygons(col = "#ffffff", lwd = 0) +
tm_shape(manhica) +
  tm_polygons(col = "#ebebeb", border.col = "#3A3A3A", lwd = 1.5) +
tm_shape(postos) +
  tm_polygons(col = "#ebebeb", border.col = "#3A3A3A", lwd = 0.25) +
tm_shape(highway) +
  tm_lines(col = "gold", lwd = 2, lty = "longdash")  +
tm_shape(roads) +
  tm_lines(col = "#33ff00", lwd = 2, lty = "dotted")  +
tm_shape(participants_geo_data) +
  # tm_dots(col = which_metric, breaks = seq_to_plot,  palette = "YlOrRd", legend.show = T) +
  tm_dots(col = which_metric, breaks = seq_to_plot, palette = gradient_color_def, legend.show = T) + # Using a diverging palette
tm_shape(main_hosp) +
  tm_symbols(size = 1.1, col = "#00d4d0", border.col = "#440154FF", shape = 23) +
tm_shape(postos) +
  tm_text("Admin_Post", size = 0.6, col = "#3b3939") +
tm_shape(other_hosp) +
  tm_symbols(size = 0.5, col = "#440154FF", border.col = "#440154FF", shape = 25, alpha = 0.5) +
tm_shape(apstation) +
  tm_symbols(size = 0.4, col = "#c85d5d", border.col = "#000000", shape = 21, alpha = 0.9) +
tm_shape(sugar) +
tm_symbols(size = 0.5, col = "#00f2ff", border.col = "#000000", shape = 24, alpha = 0.9) +
tm_add_legend(type = "fill", col = "#ebebeb", border.col = "#3A3A3A", lwd = 1.5, label = "Manhiça district") +
tm_add_legend(type = "symbol", size = 0.7, col = "#00d4d0", border.col = "#440154FF", shape = 23, label = "Main hospitals") +
tm_add_legend(type = "symbol", size = 0.3, col = "#440154FF", border.col = "#440154FF", shape = 25, alpha = 0.5, label = "Other health facilities") +
tm_add_legend(type = "line", lty = "longdash", col = "gold", lwd = 2, label = "Main road") +
tm_add_legend(type = "line", lty = "dotted", col = "#33ff00", lwd = 2, label = "highway") +
tm_add_legend(type = "symbol", size = 0.4, col = "#c85d5d", border.col = "#000000", shape = 21, alpha = 0.9, label = "Air pollution station") +
tm_add_legend(type = "symbol", size = 0.4, col = "#00f2ff", border.col = "#000000", shape = 24, alpha = 0.9, label = "Sugar cane factory") +
tm_scale_bar(width = 0.1, text.size = 0.75) +
tm_layout(bg.color = "#AFDCF0", legend.outside = T, legend.text.size = 0.65, 
          legend.outside.size = 0.15) 


# print(class(plot))

plot

}


# png("reports/mapping_variables_with_geodata_v2/map_healtdistance.png", res = 300*4, width = 2100*4, height = 1800*4)
# m2
# dev.off()



##' modelling pca for create the repor as easly possible
##'
##'
##' @title mod_pca_variable
##' @param data ADD_1rt_PARAMETER_DESCRIPTION
##' @param output_variables ADD_2on_PARAMETER_DESCRIPTION
##' @param fml_base_vars ADD_3r_PARAMETER_DESCRIPTION
##' @param pca_corrds_vars ADD_3r_PARAMETER_DESCRIPTION
##' @param where_to_save ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame 
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
mod_pca_variable <- function(
  data,
  output_variables = FALSE,
  fml_base_vars = FALSE,
  pca_corrds_vars = FALSE,
  where_to_save
){

  if(output_variables == FALSE){
    output_variables <- "h_visits"
  }

  if(fml_base_vars == FALSE){
    fml_base_vars <- c(
      output_variables,
      "fu_days",
      "gender",
      "year_birth",
      "mother_age_at_birth_of_child",
      "posto_admnistrativo",
      "healthdist_final_std",
      "mother_education",
      "SES_var"
    )
  }

  if(pca_corrds_vars == FALSE){
    pca_corrds_vars <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
  }

  fml_base <- paste(
    c(paste0(output_variables, " ~ offset(log(fu_days + 1))"),
      fml_base_vars[3:length(fml_base_vars)]),
    collapse = " + ")

  fml_pca_1 <- paste0(fml_base, " + ",pca_corrds_vars)

# dir.create(
#    here::here("outputs",
#                     "paper_2_R_model_results",
#                     where_to_save,
#                     environment_end,
#                     names_to_save_new),

#   showWarnings = FALSE
# )

  # Perform Generalized Linear Models (GLM) with PCA variables
   mapGeneralModelsCount(
    formulas_to_exe = fml_pca_1,      # fml_vars_imp, fml_vars_buff, fml_vars_ndv
    data_mod        = data,
    family_def      = "negative binomial", # poisson, zero inflated, logistic, quasipoisson, negative binomial
    ipw             = FALSE,
    ipw_select      = "ipw_ml_4",
    path_save       = here::here("outputs",
                    "paper_2_R_model_results",
                    where_to_save,
                    "negative_pca_no_ipw.rds"))




}






##' modelling mca for create the repor as easly possible
##'
##'
##' @title mod_mca_variable
##' @param data ADD_1rt_PARAMETER_DESCRIPTION
##' @param output_variables ADD_2on_PARAMETER_DESCRIPTION
##' @param fml_base_vars ADD_3r_PARAMETER_DESCRIPTION
##' @param pca_corrds_vars ADD_3r_PARAMETER_DESCRIPTION
##' @param where_to_save ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame 
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
mod_mca_variable <- function(
  data,
  output_variables = FALSE,
  fml_base_vars = FALSE,
  mca_corrds_vars = FALSE,
  where_to_save
){

  if(output_variables == FALSE){
    output_variables <- "h_visits"
  }

  if(fml_base_vars == FALSE){
    fml_base_vars <- c(
      output_variables,
      "fu_days",
      "gender",
      "year_birth",
      "mother_age_at_birth_of_child",
      "posto_admnistrativo",
      "healthdist_final_std",
      "mother_education",
      "SES_var"
    )
  }

  if(mca_corrds_vars == FALSE){
    mca_corrds_vars <- c("MCA1", "MCA2", "MCA3", "MCA4", "MCA5", "MCA6", "MCA7")
  }

  fml_base <- paste(
    c(paste0(output_variables, " ~ offset(log(fu_days + 1))"),
      fml_base_vars[3:length(fml_base_vars)]),
    collapse = " + ")

  fml_mca_1 <- paste0(fml_base, " + ",mca_corrds_vars)

# dir.create(
#    here::here("outputs",
#                     "paper_2_R_model_results",
#                     where_to_save,
#                     environment_end,
#                     names_to_save_new),

#   showWarnings = FALSE
# )
  # Perform Generalized Linear Models (GLM) with PCA variables
  mapGeneralModelsCount(
    formulas_to_exe = fml_mca_1,      # fml_vars_imp, fml_vars_buff, fml_vars_ndv
    data_mod        = data,
    family_def      = "negative binomial", # poisson, zero inflated, logistic, quasipoisson, negative binomial
    ipw             = FALSE,
    ipw_select      = "ipw_ml_4",
    path_save       = here::here("outputs",
                    "paper_2_R_model_results",
                    where_to_save,
                    "negative_mca_no_ipw.rds"))




}


##' modelling mca for create the repor as easly possible
##'
##'
##' @title mod_any_set_of_variable
##' @param data ADD_1rt_PARAMETER_DESCRIPTION
##' @param output_variables ADD_2on_PARAMETER_DESCRIPTION
##' @param fml_base_vars ADD_3r_PARAMETER_DESCRIPTION
##' @param pca_corrds_vars ADD_3r_PARAMETER_DESCRIPTION
##' @param where_to_save ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame 
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
mod_any_set_of_variable <- function(
  data,
  output_variables = FALSE,
  fml_base_vars = FALSE,
  fml_zi_1 = NULL,
  variables_to_mod_or_end = FALSE,
  where_to_save,
  environment_end,
  STD_variable = FALSE,
  forest_t = FALSE,
  family_model = "negative binomial", # poisson, zero inflated, logistic, quasipoisson, negative binomial
  name_facility_in_order = FALSE
){

  if(output_variables == FALSE){
    output_variables <- "h_visits"
  }

  if(fml_base_vars[1] == FALSE){
    fml_base_vars <- c(
      output_variables,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # ,
      # "mother_education"
    )
  }

  if(variables_to_mod_or_end[1] == FALSE){
    stop("pleas check your list, or your structure of variables in mca or HEMCA")
  }

  if(any(variables_to_mod_or_end %in% c("mca", "HEMCA", "FIMCA"))){
    variables_to_mod_or_end_2 <- names(data)[stringr::str_detect(names(data), paste0("^", variables_to_mod_or_end))]
  } else {
    variables_to_mod_or_end_2 <- variables_to_mod_or_end
  }

  fml_base <- paste(
    c(paste0(output_variables, " ~ offset(log(fu_months))"),
      fml_base_vars[3:length(fml_base_vars)]),
    collapse = " + ")

  if(STD_variable == TRUE){
    # scale if the variabel is numeric--- if not .. pass to fator 
    # fml_vec <- paste0(fml_base, " + scale(", variables_to_mod_or_end_2, ")")
    # Use lapply to transform column names based on class
    transformed_names <- unlist(lapply(variables_to_mod_or_end_2, function(col_name) {
        col_class <- class(data[[col_name]])
        if (col_class == "numeric" | col_class == "integer") {
          return(paste0("scale(", col_name, ")"))
        } else if (col_class == "character") {
          return(paste0("as.factor(", col_name, ")"))
        } else {
          return(col_name) # Keep unchanged for other classes
        }
      }))

  fml_vec <- paste0(fml_base, " + ", transformed_names)
  
  }else{
    transformed_names <- c()
    fml_vec <- paste0(fml_base, " + ", variables_to_mod_or_end_2)
  }
  
  if(forest_t == TRUE ){
    names_to_save_new <- paste0("negative_forest_no_ipw.rds")
  } else {
    names_to_save_new <- paste0("negative_", variables_to_mod_or_end[1], "_no_ipw.rds")
  }



dir.create(
   here::here("outputs",
                    "paper_2_R_model_results",
                    where_to_save,
                    environment_end
                    ),
  recursive = TRUE

  ,showWarnings = FALSE
)

  if(name_facility_in_order){

    data$healthdist_name_facility <- relevel(as.factor(data$healthdist_name_facility), ref = "Manhica sede")

  }

  # Perform Generalized Linear Models (GLM) with PCA variables
  mapGeneralModelsCount(
    formulas_to_exe = fml_vec,      # fml_vars_imp, fml_vars_buff, fml_vars_ndv
    data_mod        = data,
    family_def      = family_model, # poisson, zero inflated, logistic, quasipoisson, negative binomial
    ipw             = FALSE,
    ipw_select      = "ipw_ml_4",
    transformed_names = transformed_names,
    path_save       = here::here("outputs",
                    "paper_2_R_model_results",
                    where_to_save,
                    environment_end,
                    names_to_save_new))




}


##' modelling mca for create the repor as easly possible
##'
##'
##' @title mod_any_set_of_variable
##' @param data ADD_1rt_PARAMETER_DESCRIPTION
##' @param output_variables ADD_2on_PARAMETER_DESCRIPTION
##' @param fml_base_vars ADD_3r_PARAMETER_DESCRIPTION
##' @param pca_corrds_vars ADD_3r_PARAMETER_DESCRIPTION
##' @param where_to_save ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame 
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
mod_any_set_of_variable_RI <- function(
  data,
  output_variables = FALSE,
  fml_base_vars = FALSE,
  in_ZI_part = FALSE,
  fml_zi_1 = NULL,
  variables_to_mod_or_end = FALSE,
  where_to_save,
  environment_end,
  STD_variable = FALSE,
  forest_t = FALSE,
  family_model = "negative binomial", # poisson, zero inflated, logistic, quasipoisson, negative binomial
  name_facility_in_order = FALSE
){

  if(output_variables == FALSE){
    output_variables <- "h_visits"
  }

  if(fml_base_vars[1] == FALSE){
    fml_base_vars <- c(
      output_variables,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # ,
      # "mother_education"
    )
  }

  if(variables_to_mod_or_end[1] == FALSE){
    stop("pleas check your list, or your structure of variables in mca or HEMCA")
  }

  if(any(variables_to_mod_or_end %in% c("mca", "HEMCA", "FIMCA"))){
    variables_to_mod_or_end_2 <- names(data)[stringr::str_detect(names(data), paste0("^", variables_to_mod_or_end))]
  } else {
    variables_to_mod_or_end_2 <- variables_to_mod_or_end
  }

  fml_base <- paste(
    c(paste0(output_variables, " ~ offset(log(fu_months))"),
      fml_base_vars),
    collapse = " + ")

if(!in_ZI_part){
    fml_base_vars_tozi <- fml_base_vars[fml_base_vars != "(1 | house_number)"]
} else {
    fml_base_vars_tozi <- fml_base_vars
}
    fml_zi_RI <- paste(  " ~ ", paste(
      fml_base_vars_tozi,
    collapse = " + "))

  if(STD_variable == TRUE){
    # scale if the variabel is numeric--- if not .. pass to factor 
    # fml_vec <- paste0(fml_base, " + scale(", variables_to_mod_or_end_2, ")")
    # Use lapply to transform column names based on class
    transformed_names <- unlist(lapply(variables_to_mod_or_end_2, function(col_name) {
        col_class <- class(data[[col_name]])
        if (col_class == "numeric" | col_class == "integer") {
          return(paste0("scale(", col_name, ")"))
        } else if (col_class == "character") {
          return(paste0("as.factor(", col_name, ")"))
        } else {
          return(col_name) # Keep unchanged for other classes
        }
      }))

  fml_vec <- paste0(fml_base, " + ", transformed_names)
  fml_vec_RI <- paste0(fml_zi_RI, " + ", transformed_names)
  
  }else{
    transformed_names <- c()
    fml_vec <- paste0(fml_base, " + ", variables_to_mod_or_end_2)
    fml_vec_RI <- paste0(fml_zi_RI, " + ", variables_to_mod_or_end_2)
  }
  
  if(forest_t == TRUE ){
    names_to_save_new <- paste0("negative_forest_no_ipw.rds")
  } else {
    names_to_save_new <- paste0("negative_", variables_to_mod_or_end[1], "_no_ipw.rds")
  }



dir.create(
   here::here("outputs",
                    "paper_2_R_model_results",
                    where_to_save,
                    environment_end
                    ),
  recursive = TRUE

  ,showWarnings = FALSE
)

  if(name_facility_in_order){

    data$healthdist_name_facility <- relevel(as.factor(data$healthdist_name_facility), ref = "Manhica sede")

  }

  # Perform Generalized Linear Models (GLM) with PCA variables
  mapGeneralModelsCount(
    formulas_to_exe = fml_vec,      # fml_vars_imp, fml_vars_buff, fml_vars_ndv
    data_mod        = data,
    family_def      = family_model, # poisson, zero inflated, logistic, quasipoisson, negative binomial
    fml_ri_hurd     = fml_vec_RI,
    ipw             = FALSE,
    ipw_select      = "ipw_ml_4",
    transformed_names = transformed_names,
    path_save       = here::here("outputs",
                    "paper_2_R_model_results",
                    where_to_save,
                    environment_end,
                    names_to_save_new))




}


##' with the result of iterative models we will plot and saving these results
##' and calculate IC and more information 
##'
##' @title plot_mod_and_result_saving_mod_res
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
plot_mod_and_result_saving_mod_res <- function(
    mod_res,
    coords_vars = FALSE,
    if_no_recursive_model_base_model = FALSE,
    where_to_save,
    environment_end,
    has_ipw = TRUE,
    vPlot_categorical = FALSE,
    forest_t = FALSE){

      if(coords_vars[1] == FALSE){
        stop("define a list of coeficient do you wanna plot, pleas. plus")
      }

      if(if_no_recursive_model_base_model == FALSE){
        if_no_recursive_model_base_model_t <- "M6"
      }

    desc_model <- data_model_desc_function(
      variables_to_create = coords_vars,
        recursive_model = FALSE,
        if_no_recursive_model_base_model = if_no_recursive_model_base_model_t,
        forest_t = forest_t  
    )

    
    # Combine model information with descriptions
    model_information <- mod_res$mod_info %>%
      left_join(
        desc_model,
        by = "model_id"
      ) %>%  
      select(
        model_desc,
        model_id,
        outcome,
        covariables = cov
      )



    # Create a data frame for plotting
    pre_plot <- mod_res$coef_table %>%
      mutate(ipw_t = "Without IPW") %>%
      left_join(
        desc_model,
        by = "model_id"
      ) %>%
      dplyr::rename(vif_grt10 = "vif_normal",
                    vif_std_grt10 = "vif_std")




if(vPlot_categorical == TRUE){

      to_plot_pre <- pre_plot %>%
        filter(str_detect(term, paste(coords_vars, collapse = "|"))) %>%
        select(
          Estimate,
          `Std. Error`,
          # `Pr(>|t|)`, for quasi-poisson is t distribution
          `Pr(>|z|)`,
          Pr_FDR,
          term,
          # model_id,
          ipw_t,
          aic,
          bic,
          # model_desc,
          vif_grt10,
          vif_std_grt10,
          term
        ) %>% hr.perincr(., 1) 


      to_plot <- to_plot_pre %>%
        left_join(
          data_model_desc_function(
            variables_to_create = to_plot_pre$term,
              recursive_model = FALSE,
              if_no_recursive_model_base_model = if_no_recursive_model_base_model_t,
              forest_t = forest_t
          )
          ,
          by = join_by("term" == "term_id")
        )

    if(forest_t == TRUE){
      to_plot <- to_plot %>%
        dplyr::mutate(
          ipw_t = case_when(
            str_detect(term, "^SRE_") ~ "SRE",
            str_detect(term, "^SEA_") ~ "SEA",
            str_detect(term, "^HE_") ~ "HE",
            str_detect(term, "^FI_") ~ "FI",
            TRUE ~ "something out of environ"
          )
        )
    }

}else{
      # Select relevant columns and filter terms related to PCA
      to_plot <- pre_plot %>%
        filter(term %in% coords_vars) %>%
        select(
          Estimate,
          `Std. Error`,
          # `Pr(>|t|)`, for quasi-poisson is t distribution
          `Pr(>|z|)`,
          Pr_FDR,
          term,
          model_id,
          ipw_t,
          aic,
          bic,
          model_desc,
          vif_grt10,
          vif_std_grt10
        ) %>% hr.perincr(., 1)
}


    # Write results to CSV
    las_tuna <- paste0("model_rr_", coords_vars[1], ".csv")

    write.csv(
      to_plot %>%
        select(
          term,
          `Pr(>|z|)`,
          Pr_FDR,
          beta_exp,
          Lower_exp,
          Upper_exp
        ),
      here::here(   "outputs",
                    "paper_2_R_model_results",
                    where_to_save,
                    environment_end,
                    las_tuna)
        )





    # Plot HR and IC for environmental variables
    min_xlim <- min(to_plot$Lower_exp, na.rm = TRUE) - 0.2
    max_xlim <- max(to_plot$Upper_exp, na.rm = TRUE) + 0.2

  if(forest_t == TRUE){
    plot <- plot_hr_ic(
      to_plot %>% mutate(Covariable = term), 
      source = to_plot$term, 
      xlim_inf = min_xlim, 
      xlim_upper = max_xlim,
      vColor = "ipw_t",
      has_ipw = forest_t,
      y_title = "RR (95% CI) per dimension"
    )
  }else{
    plot <- plot_hr_ic_env_vars(
      to_plot, 
      source = to_plot$term, 
      xlim_inf = min_xlim, 
      xlim_upper = max_xlim
    )
  }




      las_tuna <- paste0("model_rr_plot_", coords_vars[1], ".jpeg")

      # Save the plot
      ggsave(
        here::here(   "outputs",
                    "paper_2_R_model_results",
                    where_to_save,
                    environment_end,
                    las_tuna
        ),
        plot = plot
      )

}

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    PLOTING GEO MAPAS - WITH REDUCTION COORDINATES MCA AND PCA
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

##' add function description
##'
##'
##' @title plot_mod_and_result_saving_geo_map
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
plot_mod_and_result_saving_geo_map <- function(
  coords_vars = FALSE
  ,filter_distribution_of_variable = 1
  ,n_cuts_to_color = 10
  ,where_to_save = where_to_save 
  ,participants_geo_data 
  ,districts = districts
  ,manhica = manhica
  ,postos = postos
  ,highway = highway
  ,roads = roads
  ,main_hosp = main_hosp
  ,other_hosp = other_hosp
  ,apstation = apstation
  ,sugar = sugar
  ,gradient_color_def = "-RdYlBu"
){

      if(coords_vars[1] == FALSE){
        stop("define a list of coeficient do you wanna plot, pleas. plus")
      }

      # if(length(vfilter_distribution_of_variable) == 1){
      #   vfilter_distribution_of_variable <- rep(vfilter_distribution_of_variable, length(coords_vars))
      # }

      # if(length(vn_cuts_to_color) == 1){
      #   vn_cuts_to_color <- rep(vn_cuts_to_color, length(coords_vars))
      # }

      # if(!(length(vfilter_distribution_of_variable) == length(coords_vars))){
      #   stop("please define the same number of paramater as dimensions you wanna plot or define just one, values from 0 to 1")
      # }

      # if(!(length(vn_cuts_to_color) == length(coords_vars))){
      #    stop("please define the same number of paramater as dimensions you wanna plot or define just one, number of cuts")
      # }

      dir.create(file.path(here::here("outputs", "paper_2_R_model_results",  where_to_save, "geo_maps")), showWarnings = FALSE)
      # i <- 5

      for(i in 1:length(coords_vars)){

       plot <-  geo_mapa_creation(
            which_metric = coords_vars[i]
            ,filter_distribution_of_variable = filter_distribution_of_variable # from 0 to 1, left tail 
            ,n_cuts_to_color = n_cuts_to_color
            ,participants_geo_data = participants_geo_data
            ,districts = districts
            ,manhica = manhica
            ,postos = postos
            ,highway = highway
            ,roads = roads
            ,main_hosp = main_hosp
            ,other_hosp = other_hosp
            ,apstation = apstation
            ,sugar = sugar
            ,gradient_color_def = gradient_color_def
          )

        las_tuna <- paste0("geo_map_", coords_vars[i], ".jpeg")

        # jpeg(
        #   here::here(  "outputs",
        #               "PCA",
        #               "PCA_v2",
        #               where_to_save,
        #               "geo_maps",
        #               las_tuna)
        #   )

        #   plot

        # dev.off()

        tmap::tmap_save(tm = plot, filename =  here::here(  "outputs",
                      "paper_2_R_model_results",
                      where_to_save,
                      "geo_maps",
                      las_tuna)
          )

      }




}


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    ADD_YOUR_DESCRIPTION_BLOCK
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
##' add function description
##'
##'
##' @title ADD_YOUR_FUNCTION_TITLE
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
which_names <- function(data, ends){
  names(data)[str_detect(names(data), paste0("^", ends))]
}

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    get_sre_variables
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
##' add function description
##'
##'
##' @title get_sre_variables
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
get_sre_variables <- function(names, type){

  alan_vars <- c(   "artificial_light_at_night_2015_500_final",
                    "artificial_light_at_night_2020_500_final",
                  "artificial_light_at_night_2015_1000_final",
                   "artificial_light_at_night_2020_1000_final"
                  #  ,
                  # "artificial_light_at_night_diff_1000_final" 
                  )


  imp_vars <- c("imperviousness_density_300_final",
                "imperviousness_density_500_final" )

  buff_vars <- c( "tree_cover_300_final",
                  "shrubland_300_final",
                  "grassland_300_final",
                  "cropland_300_final",
                  "build_up_300_final",
                  "bareland_300_final",
                  "permanent_water_bodies_300_final",
                  "tree_cover_500_final",
                  "shrubland_500_final",
                  "grassland_500_final",
                  "cropland_500_final",
                  "build_up_500_final",
                  "bareland_500_final",
                  "permanent_water_bodies_500_final" 
                  )

  ndvi_vars <- c( "ndvi_2015_300_final",
                  "ndvi_2020_300_final",
                  "ndvi_2015_500_final",
                  # "ndvi_diff_500_final",
                  "ndvi_2020_500_final"
                  )

  distances <-  c(   # "healthdist_final",
                    "roaddist_km"
                     ,
                     "sugardist_km"  # no en ###
                    )

  buffe_neihg <- c( "Nneigh_500"
                    #  ,
                    #  "ilumin_500_final",
                    #  "kitchen_500_final"
                  )


  c(  alan_vars,
      imp_vars,
      buff_vars,
      ndvi_vars,
      distances,
      buffe_neihg )


}

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    get_generic_variables list_or_end_of_variables 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
##' add function description
##'
##'
##' @title get_generic_variables
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
get_generic_variables <- function(data_names, type, restrict_variable = c("")){

  stopifnot(type %in% c("SRE", "SEA", "HE", "FI"))

  if(type == "SEA")(
    ends_variables <- "_mca"
  )

  if(type == "HE")(
    ends_variables <- "_HEMCA"
  )
  if(type == "FI")(
    ends_variables <- "_FIMCA"
  )


  data_names[(stringr::str_detect(data_names, ends_variables)) & !(data_names %in% restrict_variable)]

}

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    CATEGORICAL VARIABLE DESCRIPTIVE FUNCTION
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
##' In this function we will made a list with the principal descriptions of 
##' any set of categorical variables 
##'
##' @title generic_cat_descriptive_variables
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
generic_cat_descriptive_variables <- function(data_cat, def_set, var_set, where_to_save){

  # stopifnot( all(
  #   any(apply(data_cat, 2, class) == "character" |
  #   apply(data_cat, 2, class) == "factor")) )

##########
##########
## porp table
##########
##########

  table_proportion <- data_cat %>%
    select(all_of(var_set)) %>%
    lapply(function(x) {
      if(is.factor(x) | is.character(x) | is.logical(x)) {
        prop.table(table(x))
      } else {
        NULL
      }
    }) %>%
    bind_rows(.) %>% t()

  colnames(table_proportion) <- var_set


##########
##########
## normal table
##########
##########

  table_1 <- data_cat %>%
    select(all_of(var_set)) %>%
    lapply(function(x) {
      if(is.factor(x) | is.character(x) | is.logical(x)) {
        table(x)
      } else {
        NULL
      }
    }) %>%
    bind_rows(.) %>% t()

  colnames(table_1) <- var_set

##########
##########
## no idem 
##########
##########

data_list <- list(table = table_1, table_prop = table_proportion)

saveRDS(
  data_list,
  here::here("outputs", "paper_2_R_model_results", where_to_save, def_set, "table_description.rds")
)



}

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    REPORT GENERATION 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

##' add function description
##'
##'
##' @title report_genartion_dim_reduction
##' @param data1 ADD_1rt_PARAMETER_DESCRIPTION
##' @param data2 ADD_2on_PARAMETER_DESCRIPTION
##' @param variable ADD_3r_PARAMETER_DESCRIPTION
##' @return data.frame / list / vector
##' @author Fabian Coloma
##' @export
##' @examples
##' 
##'
report_generation_dim_reduction <- function(
  data,
  where_to_save = FALSE,
  geo_map_info_loaded = FALSE,
  environments_list = c("SRE", "SEA", "HE", "FI", "ALL"),
  outcome_to_model = "h_visits"

){


  if(where_to_save == FALSE){
    where_to_save <- "PCA-base_1"
  }

  dir.create(
    file.path(
      here::here( "outputs", 
                  "paper_2_R_model_results", 
                  where_to_save )
    ), 
    showWarnings = FALSE
  )

  for(i in 1:length(environments_list)){
    dir.create( file.path( 
      here::here(
        "outputs", 
        "paper_2_R_model_results",  
        where_to_save,
        environments_list[i])), 
      showWarnings = FALSE)

  }

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    EXTRACTING ENVIRONMENTS - AND VARIABLES NAMES
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

complete_set_names <- names(data)

SRE_variables <- get_sre_variables( names = complete_set_names, 
                                    type = environments_list[1] ) # here the parameters are ilusory xd

SEA_variables <- get_generic_variables( data_names = complete_set_names,
                                        type = environments_list[2],
                                        restrict_variable = c("has_tractor_mca", "long_head_unemployed_mca") )
# names(data)

SEA_variables <- c(SEA_variables, "mother_education")

HE_variables <- get_generic_variables(  data_names = complete_set_names,
                                        type = environments_list[3],
                                        restrict_variable = c("wat_treatment_proc_cat_HEMCA") )

FI_variables <- get_generic_variables(  data_names = complete_set_names,
                                        type = environments_list[4],
                                        restrict_variable = c("parental_marital_status_FIMCA") )

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    Descripive block
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

##########
##########
## SRE numeric Descriptive
##########
##########

SRE_desc <- skimr::skim(data[SRE_variables])

saveRDS(
  SRE_desc, 
  here::here(
    "outputs", 
    "paper_2_R_model_results",   
    where_to_save, 
    environments_list[1],
    "table_description.rds" ) )


##########
##########
## SEA categorical descriptive
##########
##########

SEA_list_desc <- generic_cat_descriptive_variables( data_cat = data, 
  def_set = environments_list[2],
  var_set = SEA_variables,
  where_to_save = where_to_save )


##########
##########
## SEA categorical descriptive
##########
##########

HE_list_desc <- generic_cat_descriptive_variables( data_cat = data, 
  def_set = environments_list[3],
  var_set = HE_variables,
  where_to_save = where_to_save )

##########
##########
## SEA categorical descriptive
##########
##########

FI_list_desc <- generic_cat_descriptive_variables( data_cat = data, 
  def_set = environments_list[4],
  var_set = FI_variables,
  where_to_save = where_to_save )


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    dimension reduction information
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

  data <- add_pca_and_save_2(
    data,
    where_to_save = where_to_save,
    list_variables = SRE_variables,
    environment_end = environments_list[1],
    many_dim = FALSE # max is the number of variables
  )

  data <- add_mca_and_save2(
    data,
    where_to_save = where_to_save,
    list_variables = SEA_variables,
    environment_end = environments_list[2],
    many_dim = FALSE # max is the number of variables
  )

  data <- add_mca_and_save2(
      data,
    where_to_save = where_to_save,
    list_variables = HE_variables,
    environment_end = environments_list[3],
    many_dim = FALSE # max is the number of variables
    )

  data <- add_mca_and_save2(
      data,
    where_to_save = where_to_save,
    list_variables = FI_variables,
    environment_end = environments_list[4],
    many_dim = FALSE # max is the number of variables
    )

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#   modeeling information
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

  # Output variables to choose from:
  # - "h_visits"
  # - "h_respiratory_all_visits_v3"
  # - "h_malaria_visits"
  # - "h_fever_visits"
  # - "h_bronchitis_visits"

##########
##########
## SRE models
##########
##########

# individuals variables

  res_mod_SRE_indv <- mod_any_set_of_variable(
    data,
    output_variables = outcome_to_model, # "h_visits"
    variables_to_mod_or_end = SRE_variables,
    fml_base_vars = c(
      outcome_to_model,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # ,
      # "mother_education"
       ),
    where_to_save = where_to_save,
    environment_end = environments_list[1],
    STD_variable = TRUE
  )


# dimensions reduction

  dimension_names_SRE <- which_names(data, environments_list[1])
  
  res_mod_SRE <- mod_any_set_of_variable(
    data,
    output_variables = outcome_to_model,
    variables_to_mod_or_end = dimension_names_SRE,
    fml_base_vars = c(
      outcome_to_model,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # "mother_education"
       ),
    where_to_save = where_to_save,
    environment_end = environments_list[1]
  )

##########
##########
## SEA models
##########
##########

# individuals

  res_mod_SEA_indv <- mod_any_set_of_variable(
    data,
    output_variables = outcome_to_model,
    variables_to_mod_or_end = SEA_variables,
    fml_base_vars = c(
      outcome_to_model,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # "mother_education"
       ),
    where_to_save = where_to_save,
    environment_end = environments_list[2]
  )


# dimensions

  dimension_names_SEA <- which_names(data, environments_list[2])

  res_mod_SEA <- mod_any_set_of_variable(
    data,
    output_variables = outcome_to_model,
    variables_to_mod_or_end = dimension_names_SEA,
    fml_base_vars = c(
      outcome_to_model,       # the same variable as outcome !
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # "mother_education"
      ),
    where_to_save = where_to_save,
    environment_end = environments_list[2]
  )

##########
##########
## HE models
##########
##########

# individuals

  res_mod_HE_indv <- mod_any_set_of_variable(
    data,
    output_variables = outcome_to_model,
    variables_to_mod_or_end = HE_variables,
    fml_base_vars = c(
      outcome_to_model,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # "mother_education"
       ),
    where_to_save = where_to_save,
    environment_end = environments_list[3]
  )


# dimensions\

 dimension_names_HE <- which_names(data, environments_list[3])

  res_mod_HE <- mod_any_set_of_variable(
      data,
      output_variables = outcome_to_model,
      variables_to_mod_or_end = dimension_names_HE,
      fml_base_vars = c(
      outcome_to_model,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # "mother_education"
      ),
      where_to_save = where_to_save,
    environment_end = environments_list[3]
    )

##########
##########
## FI models
##########
##########

# individuals

  res_mod_FI_indv <- mod_any_set_of_variable(
    data,
    output_variables = outcome_to_model,
    variables_to_mod_or_end = FI_variables,
    fml_base_vars = c(
      outcome_to_model,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # "mother_education"
       ),
    where_to_save = where_to_save,
    environment_end = environments_list[4]
  )


# dimensions\

 dimension_names_FI <- which_names(data, environments_list[4])

  res_mod_FI <- mod_any_set_of_variable(
      data,
      output_variables = outcome_to_model,
      variables_to_mod_or_end = dimension_names_FI,
      fml_base_vars = c(
      outcome_to_model,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # "mother_education"
      ),
      where_to_save = where_to_save,
    environment_end = environments_list[4]
    )

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    MODEL FOREST PLOT 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#


 dimension_names_FORESt <- c("SRE_DIM_01", "SRE_DIM_02", "SRE_DIM_03", "SRE_DIM_04", "SEA_DIM_01", "SEA_DIM_02", "SEA_DIM_03", "SEA_DIM_04", "SEA_DIM_05", "SEA_DIM_06", "SEA_DIM_07", "SEA_DIM_08", "HE_DIM_01", "HE_DIM_02", "HE_DIM_03", "FI_DIM_01", "FI_DIM_02", "FI_DIM_03", "FI_DIM_04")
#  dimension_names_FORESt <- c( "SRE_DIM_01", 
#                                  "SRE_DIM_02", 
#                                  "SRE_DIM_03", 
#                                  "SRE_DIM_04", 
#                                  "SEA_DIM_01", 
#                                  "SEA_DIM_02", 
#                                  "SEA_DIM_03", 
#                                  "SEA_DIM_04", 
#                                 #  "SEA_DIM_05", 
#                                 #  "SEA_DIM_06", 
#                                 #  "SEA_DIM_07", 
#                                 #  "SEA_DIM_08", 
#                                  "HE_DIM_01", 
#                                  "HE_DIM_02", 
#                                  "HE_DIM_03", 
#                                  "HE_DIM_04", 
#                                  "FI_DIM_01", 
#                                  "FI_DIM_02" #, 
#                                 #  "FI_DIM_03", 
#                                 #  "FI_DIM_04" 
#                                  )

  res_mod_FORESt <- mod_any_set_of_variable(
      data,
      output_variables = outcome_to_model,
      variables_to_mod_or_end = paste(dimension_names_FORESt, collapse = " + " ),
      fml_base_vars = c(
      outcome_to_model,
      "fu_months",
      "gender",
      # "SES_var",
      # "roaddist_std",
      "healthdist_final_std",
      "healthdist_name_facility",
      "as.factor(year_birth)",
      "mother_age_at_birth_of_child_std"
      # "mother_education" 
      ),
      where_to_save = where_to_save,
    environment_end = environments_list[5],
    forest_t = TRUE
    )



  plot_mod_and_result_saving_mod_res(
    mod_res = res_mod_FORESt,
    coords_vars = dimension_names_FORESt,
    if_no_recursive_model_base_model = FALSE,
    where_to_save = where_to_save,
    environment_end = environments_list[5],
    has_ipw = FALSE,
    vPlot_categorical = TRUE,
    forest_t = TRUE
  )


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#    bloc_plot models
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

##########
##########
## SRE modelling plot
##########
##########

# INDIVIDUAL

  plot_mod_and_result_saving_mod_res(
    mod_res = res_mod_SRE_indv,
    coords_vars = paste0("scale(", SRE_variables, ")"),
    if_no_recursive_model_base_model = FALSE,
    where_to_save = where_to_save,
    environment_end = environments_list[1],
    forest_t = FALSE
  )

# COMPONENTS

  plot_mod_and_result_saving_mod_res(
    mod_res = res_mod_SRE,
    coords_vars = dimension_names_SRE,
    if_no_recursive_model_base_model = FALSE,
    where_to_save = where_to_save,
    environment_end = environments_list[1]
  )

##########
##########
## SEA
##########
##########

# INDIVIDUAL

  plot_mod_and_result_saving_mod_res(
    mod_res = res_mod_SEA_indv,
    coords_vars = SEA_variables,
    if_no_recursive_model_base_model = FALSE,
    where_to_save = where_to_save,
    environment_end = environments_list[2],
    vPlot_categorical = TRUE
  )

# COMPONENTS

  plot_mod_and_result_saving_mod_res(
    mod_res = res_mod_SEA,
    coords_vars = dimension_names_SEA,
    if_no_recursive_model_base_model = FALSE,
    where_to_save = where_to_save,
    environment_end = environments_list[2]
  )

##########
##########
## HE modelling plot 
##########
##########

# INDIVIDUAL

  plot_mod_and_result_saving_mod_res(
    mod_res = res_mod_HE_indv,
    coords_vars = HE_variables,
    if_no_recursive_model_base_model = FALSE,
    where_to_save = where_to_save,
    environment_end = environments_list[3],
    vPlot_categorical = TRUE
  )

# COMPONENTS

  plot_mod_and_result_saving_mod_res(
    mod_res = res_mod_HE,
    coords_vars = dimension_names_HE,
    if_no_recursive_model_base_model = FALSE,
    where_to_save = where_to_save,
    environment_end = environments_list[3]
  )

##########
##########
## FI modelling plot
##########
##########

# INDIVIDUAL

  plot_mod_and_result_saving_mod_res(
    mod_res = res_mod_FI_indv,
    coords_vars = FI_variables,
    if_no_recursive_model_base_model = FALSE,
    where_to_save = where_to_save,
    environment_end = environments_list[4],
    vPlot_categorical = TRUE
  )

# COMPONENTS

  plot_mod_and_result_saving_mod_res(
    mod_res = res_mod_FI,
    coords_vars = dimension_names_FI,
    if_no_recursive_model_base_model = FALSE,
    where_to_save = where_to_save,
    environment_end = environments_list[4]
  )

  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  #    geo mapas 
  #-----------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------#
  print("the data is downloading")
  if(geo_map_info_loaded == FALSE){
    tar_load(districts)
    tar_load(manhica)
    tar_load(postos)
    tar_load(highway)
    tar_load(roads)
    # tar_load(participants_geo_data)
    tar_load(main_hosp)
    tar_load(other_hosp)
    # tar_load(postos)
    # tar_load(other_hosp)
    tar_load(apstation)
    tar_load(sugar)
    # tar_load(participants_geo_data)
  }

  data <- data %>% basic_transfor_to_sf_mozambique(.)

  plot_mod_and_result_saving_geo_map(
    coords_vars =  dimension_names_SRE,
    filter_distribution_of_variable = 1,
    n_cuts_to_color = 10,
    where_to_save = where_to_save
  ,participants_geo_data  = data
  ,districts = districts
  ,manhica = manhica
  ,postos = postos
  ,highway = highway
  ,roads = roads
  ,main_hosp = main_hosp
  ,other_hosp = other_hosp
  ,apstation = apstation
  ,sugar = sugar
  )

  plot_mod_and_result_saving_geo_map(
    coords_vars = dimension_names_SEA,
    filter_distribution_of_variable = 1,
    n_cuts_to_color = 10,
    where_to_save = where_to_save
  ,participants_geo_data  = data
  ,districts = districts
  ,manhica = manhica
  ,postos = postos
  ,highway = highway
  ,roads = roads
  ,main_hosp = main_hosp
  ,other_hosp = other_hosp
  ,apstation = apstation
  ,sugar = sugar
  )

  plot_mod_and_result_saving_geo_map(
    coords_vars =  dimension_names_HE,
    filter_distribution_of_variable = 1,
    n_cuts_to_color = 10,
    where_to_save = where_to_save
  ,participants_geo_data  = data
  ,districts = districts
  ,manhica = manhica
  ,postos = postos
  ,highway = highway
  ,roads = roads
  ,main_hosp = main_hosp
  ,other_hosp = other_hosp
  ,apstation = apstation
  ,sugar = sugar
  )

  plot_mod_and_result_saving_geo_map(
    coords_vars =  dimension_names_FI,
    filter_distribution_of_variable = 1,
    n_cuts_to_color = 10,
    where_to_save = where_to_save
  ,participants_geo_data  = data
  ,districts = districts
  ,manhica = manhica
  ,postos = postos
  ,highway = highway
  ,roads = roads
  ,main_hosp = main_hosp
  ,other_hosp = other_hosp
  ,apstation = apstation
  ,sugar = sugar
  )

  plot_mod_and_result_saving_geo_map(
    coords_vars =  c("h_visits"),
    filter_distribution_of_variable = 0.99,
    n_cuts_to_color = 5,
    where_to_save = where_to_save
  ,participants_geo_data  = data
  ,districts = districts
  ,manhica = manhica
  ,postos = postos
  ,highway = highway
  ,roads = roads
  ,main_hosp = main_hosp
  ,other_hosp = other_hosp
  ,apstation = apstation
  ,sugar = sugar
  ,gradient_color_def = "YlOrRd"
  )



}


# functionTEST <- function(){
#   tar_load(data_to_mod_w_environment_v2)
#   print(dim(data_to_mod_w_environment_v2))
# }


load_variables_by_300 <- function(){
  c("artificial_light_at_night_2015_500_final" 
,"artificial_light_at_night_2020_500_final"
# ,"artificial_light_at_night_2015_1000_final"
# ,"artificial_light_at_night_2020_1000_final"
,"imperviousness_density_300_final"
# ,"imperviousness_density_500_final"
,"tree_cover_300_final"
,"shrubland_300_final"
,"grassland_300_final"
,"cropland_300_final"
,"build_up_300_final"
,"bareland_300_final"
,"permanent_water_bodies_300_final"
# ,"tree_cover_500_final"
# ,"shrubland_500_final"
# ,"grassland_500_final"
# ,"cropland_500_final"
# ,"build_up_500_final"
# ,"bareland_500_final"
# ,"permanent_water_bodies_500_final"
,"ndvi_2015_300_final"
,"ndvi_2020_300_final"
# ,"ndvi_2015_500_final"
# ,"ndvi_2020_500_final"
,"roaddist_km"
,"sugardist_km"
,"Nneigh_500"
,"has_tv_mca"
,"has_radio_mca"
,"has_dvd_mca"
,"has_computer_mca"
,"has_bike_mca"
,"has_farm_of_commercial_production_mca"
,"has_glacier_freezer_mca"
,"has_celular_telephone_mca"
,"has_livestock_mca"
,"has_motoCar_mca"
,"mother_education"
,"unimproved_hh_material_HEMCA"
,"sanitation_cat_HEMCA"
,"no_acces_to_electricity_HEMCA"
,"kitchen_fuel_post_HEMCA"
,"death_of_mother"
,"death_of_father"
,"living_without_mother_in_household"
,"living_without_father_in_household"
,"death_of_sibling")
}

load_variables_by_500 <- function(){
  c(

# "artificial_light_at_night_2015_500_final" 
# ,"artificial_light_at_night_2020_500_final"
"artificial_light_at_night_2015_1000_final"
,"artificial_light_at_night_2020_1000_final"
# ,"imperviousness_density_300_final"
,"imperviousness_density_500_final"
# ,"tree_cover_300_final"
# ,"shrubland_300_final"
# ,"grassland_300_final"
# ,"cropland_300_final"
# ,"build_up_300_final"
# ,"bareland_300_final"
# ,"permanent_water_bodies_300_final"
,"tree_cover_500_final"
,"shrubland_500_final"
,"grassland_500_final"
,"cropland_500_final"
,"build_up_500_final"
,"bareland_500_final"
,"permanent_water_bodies_500_final"
# ,"ndvi_2015_300_final"
# ,"ndvi_2020_300_final"
,"ndvi_2015_500_final"
,"ndvi_2020_500_final"
,"roaddist_km"
,"sugardist_km"
,"Nneigh_500"
,"has_tv_mca"
,"has_radio_mca"
,"has_dvd_mca"
,"has_computer_mca"
,"has_bike_mca"
,"has_farm_of_commercial_production_mca"
,"has_glacier_freezer_mca"
,"has_celular_telephone_mca"
,"has_livestock_mca"
,"has_motoCar_mca"
,"mother_education"
,"unimproved_hh_material_HEMCA"
,"sanitation_cat_HEMCA"
,"no_acces_to_electricity_HEMCA"
,"kitchen_fuel_post_HEMCA"
,"death_of_mother"
,"death_of_father"
,"living_without_mother_in_household"
,"living_without_father_in_household"
,"death_of_sibling")
}
