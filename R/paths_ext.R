#' Get the Path to the Currently Set Environment
#'
#' Reads the `.config.yaml` configuration and resolves placeholders such as
#' `"OneDriveConsumer"` or `"OneDriveBusiness"` back to real file paths,
#' then appends the currently set environment to the path.
#'
#' @param project_dir A character string specifying the project directory
#'   where the `.config.yaml` file is located. Defaults to `"."`.
#'
#' @return A character string containing the absolute path to the current
#'   environment.
#'
#' @details
#' If `.config.yaml` does not exist or the required keys (`root_dir` and
#' `environment`) are missing, the function will stop with an error message.
#'
#' @export
pa_getwd <- function(
    project_dir = "."
) {
  # Normalize project directory
  project_dir <- path_abs(project_dir)

  # Define the path for the .config file
  config_file <- path(project_dir, ".config.yaml")

  # Check if the config file exists
  if (!file_exists(config_file)) {
    stop("Configuration file not found: ", config_file)
  }

  # Read the configuration data
  config_data <- read_yaml(config_file)

  # Validate keys in the config
  if (!"root_dir" %in% names(config_data)) {
    stop("Key 'root_dir' is missing in the configuration file.")
  }
  if (!"environment" %in% names(config_data)) {
    stop("Key 'environment' is missing in the configuration file.")
  }

  # Resolve placeholders in root_dir
  root_dir <- path_abs(config_data$root_dir)
  onedrive_consumer   <- path_abs(Sys.getenv("OneDriveConsumer", ""))
  onedrive_commercial <- path_abs(Sys.getenv("OneDriveCommercial", ""))

  if (path_has_parent(root_dir, "OneDriveConsumer")) {
    relative_path <- path_rel(root_dir, start = "OneDriveConsumer")
    root_dir <- path(onedrive_consumer, relative_path)
  } else if (path_has_parent(root_dir, "OneDriveBusiness")) {
    relative_path <- path_rel(root_dir, start = "OneDriveBusiness")
    root_dir <- path(onedrive_commercial, relative_path)
  }

  # Construct and return the normalized environment path
  environment_path <- path(root_dir, config_data$environment)
  return(path_abs(environment_path))
}
