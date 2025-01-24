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
pa_Setup_Project_Structure <- function(
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

  # check if root dir already exists
  if (dir_exists(root_dir)) {
    stop(silver("Root directory already exists!: "), green(root_dir))
  }

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
pa_Set_Current_Environment <- function(
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

  .copy_config_folder(pa_get_environment_path())

}

