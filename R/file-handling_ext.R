#' get working directory
#'
#' Determines the working directory based on the project directory and the configuration file.
#' resolves placeholders such as `"OneDriveConsumer"` or `"OneDriveBusiness"`
#' back to real file paths, then appends the currently set environment to the path.
#'
#' @param project_dir Character. The path to the project directory containing the `.config.yaml` file.
#' @return Character. The absolute path to the working directory.
#' @export
pa_wd_get <- function(
    project_dir = ".") {

  # Check if the project directory exists
  if (!dir.exists(project_dir)) {
    stop("The provided project directory does not exist: ", project_dir)
  }

  # Normalize project directory
  project_dir <- path_abs(project_dir)

  # Define the path for the .config file
  config_file <- path(project_dir, CONFIG_FLDR, ".config.yaml")

  # Retrieve root_dir and environment from the YAML file
  root_dir    <- pa_config_get_value("root_dir"   , config_file)
  environment <- pa_config_get_value("environment", config_file)

  # Check if root_dir and environment are valid
  if (is.null(root_dir) || root_dir == "") {
    stop("The 'root_dir' is not set or invalid in the .config.yaml file.")
  }
  if (is.null(environment) || environment == "") {
    stop("The 'environment' is not set or invalid in the .config.yaml file.")
  }

  # Resolve placeholders in root_dir
  root_dir            <- path_abs(root_dir)
  onedrive_consumer   <- path_abs(Sys.getenv("OneDriveConsumer"  , ""))
  onedrive_commercial <- path_abs(Sys.getenv("OneDriveCommercial", ""))

  # Check environment variables
  if (onedrive_consumer == "" || onedrive_commercial == "") {
    stop("Environment variables 'OneDriveConsumer' or 'OneDriveCommercial' are not set.")
  }

  if (path_has_parent(root_dir, "OneDriveConsumer")) {
    relative_path <- path_rel(root_dir, start = "OneDriveConsumer")
    root_dir <- path(onedrive_consumer, relative_path)
  } else if (path_has_parent(root_dir, "OneDriveBusiness")) {
    relative_path <- path_rel(root_dir, start = "OneDriveBusiness")
    root_dir <- path(onedrive_commercial, relative_path)
  }

  # Construct and return the normalized environment path
  environment_path <- path(root_dir, environment)

  return(path_abs(environment_path))
}

