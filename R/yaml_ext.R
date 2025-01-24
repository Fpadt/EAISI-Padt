#' get config value from YAML file
#'
#' Reads a value from a YAML configuration file based on the provided key.
#'
#' @param key Character. The key whose value needs to be retrieved.
#' @param config_file Character. The path to the YAML configuration file. Defaults to ".config.yaml".
#' @return The value associated with the provided key, or NULL if the key does not exist.
#' @export
pa_get_config_value <- function(key, config_file = ".config.yaml") {

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

  # Retrieve the value associated with the key
  value <- config[[key]]

  # Return the value or a message if the key does not exist
  if (is.null(value)) {
    warning("Key '", key, "' not found in configuration file.")
    return(NULL)
  }

  return(value)
}

#' Set Config Value in YAML File
#'
#' Adds or updates a key-value pair in a YAML configuration file.
#'
#' @param key Character. The key to add or update in the YAML file.
#' @param value Any. The value to associate with the given key.
#' @param config_file Character. The path to the YAML configuration file. Defaults to ".config.yaml".
#' @return NULL. The function updates the YAML file in place.
#' @export
pa_set_config_value <- function(key, value, config_file = ".config.yaml") {
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

  # Update or add the key-value pair
  config[[key]] <- value

  # Write the updated configuration back to the YAML file
  yaml::write_yaml(config, config_file)

  message("Key '", key, "' has been updated/added with value: ", value)
}
