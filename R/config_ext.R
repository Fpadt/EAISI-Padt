#' Get config value from YAML file with hierarchy support
#'
#' Retrieves a value from a YAML configuration file based on a hierarchical
#' key using dot notation (e.g., "sales.transformation").
#'
#' @param .key Character. The key to retrieve from the YAML file. For nested keys, use dot notation (e.g., "sales.source_path").\cr
#' @param .config_path Character. The full file name to the config file
#' @return The value associated with the given key, or NULL if the key does not exist.
#' @examples
#' # Get the value of a key from the configuration file
#' pa_config_get_value("environment")
#' @export
pa_config_get_value <- function(
    .key,
    .config_path = .cn_config_file_path_get()
    ) {

  # Normalize and construct the full path to the configuration file
  config_path <- fs::path_abs(.config_path)

  # Check if the config file exists
  if (!file.exists(config_path)) {
    stop("Configuration file not found: ", config_path)
  }

  # Read the YAML file
  config <- yaml::read_yaml(config_path)

  # Split the hierarchical .key into parts
  key_parts <- unlist(strsplit(.key, "\\."))

  # Navigate the hierarchy to retrieve the value
  current <- config
  for (part in key_parts) {
    if (!is.list(current) || is.null(current[[part]])) {
      warning("Key '", .key, "' not found in configuration file: ", config_path)
      return(NULL)
    }
    current <- current[[part]]
  }

  return(current)
}


#' Set config value in YAML file with hierarchy support
#'
#' Adds or updates a key-value pair in a YAML configuration file. Supports hierarchical
#' keys using dot notation (e.g., "sales.pipeline"). If the section or key does not exist,
#' it will be created. If it already exists, it will be updated.
#'
#' @param .key Character. The key to add or update in the YAML file. For nested keys, use dot notation (e.g., "SALES.pipeline").\cr
#' @param .value Any. The value to associate with the given key.\cr
#' @param .config_path Character. The full file name to the config file\cr
#' @return Character. The normalized path of the updated configuration file.
#' @export
pa_config_set_value <- function(
    .key,
    .value,
    .config_path = .cn_config_file_path_get()
    ) {

  # Normalize and construct the full path to the configuration file
  config_path <- fs::path_abs(.config_path)

  # Initialize an empty config if the file does not exist
  if (!file.exists(config_path)) {
    config <- list()
  } else {
    # Read the existing YAML file
    config <- yaml::read_yaml(config_path)
  }

  # Split the hierarchical .key into parts
  key_parts <- unlist(strsplit(.key, "\\."))

  # Helper function to recursively navigate and set values
  set_nested_value <- function(config, key_parts, value) {
    if (length(key_parts) == 1) {
      # Set the final key with the value
      config[[key_parts[1]]] <- value
    } else {
      # If the current key does not exist or is not a list, initialize it as a list
      if (!is.list(config[[key_parts[1]]])) {
        config[[key_parts[1]]] <- list()
      }
      # Recursively navigate deeper
      config[[key_parts[1]]] <- set_nested_value(
        config[[key_parts[1]]],
        key_parts[-1],
        value
      )
    }
    return(config)
  }

  # Update the configuration using the helper function
  config <- set_nested_value(config, key_parts, .value)

  # Write the updated configuration back to the YAML file
  yaml::write_yaml(config, config_path, indent = 2)

  # Add blank lines before top-level sections for better readability
  yaml_content <- readLines(config_path)
  yaml_content <- paste0(
    unlist(lapply(seq_along(yaml_content), function(i) {
      if (i > 1 && grepl("^\\S+:\\s*$", yaml_content[i]) && yaml_content[i - 1] != "") {
        return(c("", yaml_content[i])) # Add a blank line before the top-level key
      }
      return(yaml_content[i])
    })),
    collapse = "\n"
  )
  writeLines(yaml_content, config_path)

  # Return the normalized path and print a message
  message(
    green(
      paste("Key '", .key, "' has been updated/added with value:",
            .value)))

  return(config_path)
}
