#' Set Up a Project Directory Structure
#'
#' Creates a directory structure for different pa_DTAP environments, pa_BSGP stages,
#' and functional pa_AREAs, and saves the root directory location in `.config.yaml`.
#'
#' @param root_dir A character string specifying the top-level directory under
#'   which all subdirectories will be created. Defaults to `"."`.
#' @param pa_DTAP_levels A character vector defining the pa_DTAP levels.
#'   Defaults to `pa_DTAP` = c("Development", "Test", "Acceptance", "Production").
#' @param pa_BSGP_levels A character vector defining the pa_BSGP stages.
#'   Defaults to `pa_BSGP` = c("Bronze", "Silver", "Gold", "Platinum").
#' @param functional_pa_AREAs A character vector defining functional pa_AREAs within
#'   each pa_BSGP stage. Defaults to
#'   `pa_AREA` = c("sales", "stock", "promotions", "master_data").
#' @param project_dir A character string specifying the directory where the
#'   `.config.yaml` file will be saved or updated. Defaults to `"."`.
#'
#' @return Invisibly returns the normalized `root_dir`.
#'
#' @details
#' This function constructs a multi-level directory hierarchy reflecting
#' different pa_DTAP (Development, Test, Acceptance, Production) environments,
#' each with pa_BSGP (Bronze, Silver, Gold, Platinum) stages and designated
#' functional pa_AREAs (e.g., "sales", "stock"). If the specified `root_dir` is
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
    pa_DTAP_levels      = pa_DTAP,
    pa_BSGP_levels      = pa_BSGP,
    functional_pa_AREAs = pa_AREA,
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

  # Create the pa_DTAP, pa_BSGP, and functional pa_AREA directory structure
  for (pa_DTAP in pa_DTAP_levels) {
    for (pa_BSGP in pa_BSGP_levels) {
      for (pa_AREA in functional_pa_AREAs) {
        dir_create(path(root_dir, pa_DTAP, pa_BSGP, pa_AREA))
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

#' Set the Current pa_DTAP Environment
#'
#' Updates the `.config.yaml` file to indicate which pa_DTAP environment
#' (Development, Test, Acceptance, Production) is currently active.
#'
#' @param .project_dir A character string specifying the project directory
#'   where the `.config.yaml` file is located. Defaults to `"."`.
#' @param .environment A character string specifying the environment to set.
#'   Must be one of the values in `pa_DTAP`.
#'
#' @details
#' If an invalid environment name is provided, the function will stop
#' execution and display a list of valid environments in green using the
#' **`crayon`** package.
#'
#' @return Invisibly returns `NULL` after updating the configuration.
#'
#' @export
pa_set_environment <- function(
    .project_dir = ".",
    .environment = "Production"
) {
  if(!.environment %in% pa_DTAP){
    message(green(paste0("Valid environments: ", paste(pa_DTAP, collapse = ", "))))
    stop("Invalid environment: ", .environment)
  }

  .upsert_config_in_yaml(
    .key         = "environment",
    .value       = .environment,
    .project_dir = .project_dir
  )

  .copy_config_folder(pa_getwd())

}

