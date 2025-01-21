library(fs)
library(yaml)
library(crayon)

# R/constants.R
#' Vector of DTAP Environments
#'
#' These are the default DTAP environments used in the package.
#' @export
DTAP <- c("Development", "Test", "Acceptance", "Production")

#' Vector of BSGP Stages
#'
#' The four data stages commonly used in data processing.
#' @export
BSGP <- c("Bronze", "Silver", "Gold", "Platinum")

#' Vector of Functional Areas
#'
#' Common functional areas for demonstration purposes.
#' @export
AREA <- c("sales", "stock", "promotions", "master_data")


#' Save a Configuration to YAML
#'
#' Internal function to save configuration settings to a `.config.yaml` file.
#'
#' @param config_list A named list of configuration items to write to the YAML file.
#' @param project_dir A character string specifying the project directory where
#'   the `.config.yaml` file is (or will be) located. Defaults to `"."`.
#'
#' @details
#' This function is used internally by the package to store key-value pairs
#' into a YAML file for persistent configuration management.
#'
#' @keywords internal
.save_config_to_yaml <- function(
    config_list,
    project_dir = ".") {

  # Normalize project directory
  project_dir <- normalizePath(project_dir, mustWork = TRUE)

  # Define the path for the .config file
  config_file <- file.path(project_dir, ".config.yaml")

  # Write the configuration data to the YAML file
  write_yaml(config_list, config_file)

  message(
    green(paste0("Configuration saved to YAML file: ", config_file)))
}

#' Upsert (Update/Insert) a Key-Value Pair in Configuration
#'
#' Internal function to add or update a single key-value pair in `.config.yaml`.
#'
#' @param .key A character string specifying the key to be updated or inserted.
#' @param .value The value to assign to the key.
#' @param .project_dir A character string specifying the project directory where
#'   the `.config.yaml` file is (or will be) located. Defaults to `"."`.
#'
#' @details
#' If the `.config.yaml` file already exists, this function updates or inserts
#' the key-value pair in that file. If the file does not exist, it is created
#' along with a minimal configuration list.
#'
#' @keywords internal
.upsert_config_in_yaml <- function(
    .key,
    .value,
    .project_dir = ".") {

  # Normalize project directory
  project_dir <- normalizePath(.project_dir, mustWork = TRUE)

  # Define the path for the .config file
  config_file <- file.path(project_dir, ".config.yaml")

  # Check if the config file exists
  if (file.exists(config_file)) {
    # Read the existing configuration
    config_data <- read_yaml(config_file)
  } else {
    # Initialize an empty configuration if the file does not exist
    config_data <- list()
  }

  # Check if the key exists and update or insert
  if (!.key %in% names(config_data)) {
    message(
      silver(paste0("Key '", .key, "' does not exist. ",
      "Adding it to the configuration.")))
  }
  config_data[[.key]] <- .value

  # Write the updated configuration back to the YAML file
  write_yaml(config_data, config_file)

  message(
    green(paste0("Key '", .key,
                 "' updated/added in the configuration file: ", config_file)))
}

#' Copy the Config Folder from Package to Destination
#'
#' Internal function to copy the `config` folder located in the package's `extdata`
#' directory to a specified destination. If the destination already contains a
#' `config` folder, the function does nothing.
#'
#' @param destination_dir A character string specifying the directory where the
#'   `config` folder should be copied. If the folder already exists at the
#'   destination, no action is taken.
#'
#' @details
#' This function locates the `config` folder in the package's `extdata` directory,
#' verifies its existence, and copies it to the specified destination directory.
#' If the `config` folder already exists in the destination directory, the
#' function does not overwrite it. This is useful for setting up configuration
#' files during the initialization or setup phase.
#'
#' @return
#' Invisibly returns \code{TRUE} if the folder was successfully copied, or
#' \code{FALSE} if the destination already contained the folder.
#'
#' @seealso
#' \code{\link[base]{dir.create}} for directory creation,
#' \code{\link[base]{file.copy}} for file copying.
#'
#' @keywords internal
.copy_config_folder <- function(destination_dir) {

  # Define the source folder (config folder in extdata)
  # source_dir <- system.file("extdata", "config", package = "padt")
  source_dir <- file.path("C:/RW/EAISI-Padt", "inst", "extdata", "config")

  # Ensure the source directory exists in the package
  if (!dir.exists(source_dir)) {
    stop(
      red(paste0("Source config folder not found in the package.",
                 "Ensure it exists in: ", source_dir )))
  }

  # Define the destination folder path
  destination_config_dir <-
    normalizePath(file.path(destination_dir, "config"),
                  winslash = "/", mustWork = FALSE)

  # Check if the destination config folder already exists
  if (dir.exists(destination_config_dir)) {
    # message("The config folder already exists at: ", destination_config_dir)
    return(invisible(FALSE))  # Indicate that nothing was copied
  }

  # Copy the entire config folder to the destination
  dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(source_dir, destination_dir, recursive = TRUE)

  message(green(paste0("Config folder copied to: ", destination_config_dir)))

  return(invisible(TRUE))  # Indicate that the copy operation was successful
}


#### External Function ####

#' Set Up a Project Directory Structure
#'
#' Creates a directory structure for different DTAP environments, BSGP stages,
#' and functional areas, and saves the root directory location in `.config.yaml`.
#'
#' @param root_dir A character string specifying the top-level directory under
#'   which all subdirectories will be created. Defaults to `"."`.
#' @param dtap_levels A character vector defining the DTAP levels.
#'   Defaults to `DTAP` = c("Development", "Test", "Acceptance", "Production").
#' @param bsgp_levels A character vector defining the BSGP stages.
#'   Defaults to `BSGP` = c("Bronze", "Silver", "Gold", "Platinum").
#' @param functional_areas A character vector defining functional areas within
#'   each BSGP stage. Defaults to
#'   `AREA` = c("sales", "stock", "promotions", "master_data").
#' @param project_dir A character string specifying the directory where the
#'   `.config.yaml` file will be saved or updated. Defaults to `"."`.
#'
#' @return Invisibly returns the normalized `root_dir`.
#'
#' @details
#' This function constructs a multi-level directory hierarchy reflecting
#' different DTAP (Development, Test, Acceptance, Production) environments,
#' each with BSGP (Bronze, Silver, Gold, Platinum) stages and designated
#' functional areas (e.g., "sales", "stock"). If the specified `root_dir` is
#' within the user's OneDrive path (either consumer or commercial), the path
#' stored in the YAML file will be replaced with a placeholder
#' ("OneDriveConsumer" or "OneDriveBusiness") and the relative subdirectory
#' path.
#'
#' @seealso
#'   [fs::dir_create()] for directory creation,
#'   [fs::path_abs()] and [fs::path_rel()] for path manipulations,
#'   [yaml::write_yaml()] for saving configurations.
#'
#' @export
setup_project_structure <- function(
    root_dir         = ".",
    dtap_levels      = DTAP,
    bsgp_levels      = BSGP,
    functional_areas = AREA,
    project_dir      = "."
) {
  # Get OneDrive paths from environment variables
  onedrive_consumer   <- path_abs(Sys.getenv("OneDriveConsumer"))
  onedrive_commercial <- path_abs(Sys.getenv("OneDriveCommercial"))

  # Normalize and validate root_dir
  root_dir <- tryCatch({
    path_abs(root_dir)
  }, error = function(e) {
    stop("Invalid root directory path: ", root_dir, "\n", e$message)
  })

  # Determine the value to store for root_dir in the YAML
  yaml_root_dir <- if (path_has_parent(root_dir, onedrive_consumer)) {
    rel_path <- path_rel(root_dir, start = onedrive_consumer)
    path("OneDriveConsumer", rel_path)
  } else if (path_has_parent(root_dir, onedrive_commercial)) {
    rel_path <- path_rel(root_dir, start = onedrive_commercial)
    path("OneDriveBusiness", rel_path)
  } else {
    root_dir
  }

  # Attempt to create the root directory
  tryCatch({
    dir_create(root_dir)
  }, error = function(e) {
    stop("Failed to create the root directory: ", root_dir, "\n", e$message)
  })

  # Create the DTAP, BSGP, and functional area directory structure
  for (dtap in dtap_levels) {
    for (bsgp in bsgp_levels) {
      for (area in functional_areas) {
        dir_create(path(root_dir, dtap, bsgp, area))
      }
    }

  }

  # Save the root_dir to the YAML file
  .save_config_to_yaml(
    config_list = list(root_dir = yaml_root_dir),
    project_dir = project_dir
  )

  message(
    green(paste0("Project structure created successfully at: ", root_dir)))

  return(invisible(root_dir))
}

#' Set the Current DTAP Environment
#'
#' Updates the `.config.yaml` file to indicate which DTAP environment
#' (Development, Test, Acceptance, Production) is currently active.
#'
#' @param .project_dir A character string specifying the project directory
#'   where the `.config.yaml` file is located. Defaults to `"."`.
#' @param .environment A character string specifying the environment to set.
#'   Must be one of the values in `DTAP`.
#'
#' @details
#' If an invalid environment name is provided, the function will stop
#' execution and display a list of valid environments in green using the
#' **`crayon`** package.
#'
#' @return Invisibly returns `NULL` after updating the configuration.
#'
#' @export
set_current_environment <- function(
    .project_dir = ".",
    .environment = "Production"
) {
  if(!.environment %in% DTAP){
    message(green(paste0("Valid environments: ", paste(DTAP, collapse = ", "))))
    stop("Invalid environment: ", .environment)
  }

  .upsert_config_in_yaml(
    .key         = "environment",
    .value       = .environment,
    .project_dir = .project_dir
  )

  .copy_config_folder(get_environment_path())

}

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
get_environment_path <- function(
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

