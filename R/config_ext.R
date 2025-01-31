#' Get config value from YAML file with hierarchy support
#'
#' Retrieves a value from a YAML configuration file based on a hierarchical
#' key using dot notation (e.g., "sales.transformation").
#'
#' @param .key Character. The key to retrieve from the YAML file. For nested keys, use dot notation (e.g., "sales.source_path").\cr
#' @return The value associated with the given key, or NULL if the key does not exist.
#' @examples
#' # Get the value of a key from the configuration file
#' pa_config_value_get("environment")
#' @export
pa_config_value_get <- function(
    .key
    ) {

  # reload config into cache if needed
  .hl_config_get()

  # Split the hierarchical .key into parts
  key_parts <- unlist(strsplit(.key, "\\."))

  # Navigate the hierarchy to retrieve the value
  config <- .hl_config_get()
  for (part in key_parts) {
    if (!is.list(config) || is.null(config[[part]])) {
      warning("Key '", .key, "' not found in configuration file: ",
              .padt_env$cfg_path)
      return(NULL)
    }
    config <- config[[part]]
  }

  return(config)
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
pa_config_value_set <- function(
    .key,
    .value,
    .config_path = .padt_env$cfg_path
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

  # reload config into cache
  .hl_config_get(.reload = TRUE)

  # Return the normalized path and print a message
  message(
    green(
      paste("Key '", .key, "' has been updated/added with value:",
            .value)))

  return(config_path)
}


#' Retrieve all values with a specific key from the same hierarchy level
#'
#' This internal function searches the YAML configuration hierarchy for entries
#' matching a specific key. It ensures that all matching entries come from the
#' same level in the hierarchy.
#'
#' @param .key Character. The key to search for within the YAML configuration.
#' @param .config List. The parsed YAML configuration (default is `.padt_env$cfg`).
#' @param .level Integer. The hierarchy level to search for keys (default is 0 for root).
#' @return A list or vector of all matching values for the specified key from the same level.
#'         Returns an empty list if no matches are found.
#' @examples
#' # Example YAML structure:
#' # sales:
#' #   system1:
#' #     folder: "folder1"
#' #   system2:
#' #     folder: "folder2"
#' # inventory:
#' #   items:
#' #     folder: "folder3"
#' #   warehouse:
#' #     details:
#' #       folder: "folder4"
#' #
#' # .yaml_entries_same_level_get("folder") will return:
#' # list("folder1", "folder2") or list("folder3") depending on where the search starts.
#'
#' .yaml_entries_same_level_get("folder")
#'
#' @export
pa_config_level_values_get <- function(
    .key,
    .level = 0
    ) {

  # reload config into cache if needed
  config <- .hl_config_get()

  # Recursive helper function to search at the specified level
  find_entries_at_level <- function(config, key, current_level, target_level) {
    results <- list()

    # If we are at the target level, search for the key
    if (current_level == target_level) {
      for (name in names(config)) {
        if (name == key) {
          results <- c(results, config[[name]])
        }
      }
      return(results)
    }

    # If not at the target level, traverse deeper
    for (name in names(config)) {
      if (is.list(config[[name]])) {
        results <- c(results, find_entries_at_level(config[[name]], key, current_level + 1, target_level))
      }
    }

    results
  }

  # Determine the level of the key in the configuration
  target_level <- .level

  # Call the recursive function to find all entries at the same level
  find_entries_at_level(.config, .key, current_level = 0, target_level = target_level)
}
