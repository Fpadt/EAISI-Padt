#' Get Config Value from YAML File with Hierarchy Support
#'
#' Retrieves a value from a YAML configuration file based on a hierarchical
#' key using dot notation (e.g., "SALES.pipeline").
#'
#' @param .key Character. The key to retrieve from the YAML file. For nested keys, use dot notation (e.g., "SALES.source_path").
#' @param config_file Character. The path to the YAML configuration file. Defaults to ".config.yaml".
#' @return The value associated with the given key, or NULL if the key does not exist.
#' @export
pa_config_get_value <- function(
    .key,
    config_file = ".config.yaml") {

  # Load required package
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The 'yaml' package is required but not installed. Please install it using install.packages('yaml').")
  }

  # Check if the config file exists
  if (!file.exists(config_file)) {
    stop("Configuration file not found: ", config_file)
  }

  # Read the YAML file
  config <- yaml::read_yaml(config_file)

  # Split the hierarchical .key into parts
  key_parts <- unlist(strsplit(.key, "\\."))

  # Navigate the hierarchy to retrieve the value
  current <- config
  for (part in key_parts) {
    if (!is.list(current) || is.null(current[[part]])) {
      warning("Key '", .key, "' not found in configuration file: ", config_file)
      return(NULL)
    }
    current <- current[[part]]
  }

  return(current)
}


#' Set Config Value in YAML File with Hierarchy Support
#'
#' Adds or updates a key-value pair in a YAML configuration file. Supports hierarchical
#' keys using dot notation (e.g., "SALES.pipeline").
#'
#' @param .key Character. The key to add or update in the YAML file. For nested keys, use dot notation (e.g., "SALES.source_path").
#' @param .value Any. The value to associate with the given key.
#' @param config_file Character. The path to the YAML configuration file. Defaults to ".config.yaml".
#' @return NULL. The function updates the YAML file in place.
#' @export
pa_config_set_value <- function(
    .key,
    .value,
    config_file = ".config.yaml") {

  # Load required package
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The 'yaml' package is required but not installed. Please install it using install.packages('yaml').")
  }

  # Initialize an empty config if the file does not exist
  if (!file.exists(config_file)) {
    config <- list()
  } else {
    # Read the existing YAML file
    config <- yaml::read_yaml(config_file)
  }

  # Split the hierarchical .key into parts
  key_parts <- unlist(strsplit(.key, "\\."))

  # Navigate the hierarchy to set the value
  current <- config
  for (i in seq_along(key_parts)) {
    part <- key_parts[i]
    if (i == length(key_parts)) {
      # Set the value at the final key
      current[[part]] <- .value
    } else {
      # Create a nested list if the part does not exist or is not a list
      if (!is.list(current[[part]])) {
        current[[part]] <- list()
      }
      current <- current[[part]]
    }
  }

  # Write the updated configuration back to the YAML file
  yaml::write_yaml(config, config_file)

  message("Key '", .key, "' has been updated/added with value: ", .value)
}
