# prerequisite(s):
#  - execute yaml_int::.su_config_default_write()
#
# Set Up - after package installation
#  1. .su_package_folder_copy
#  2. .su_data_folders_create()
#  3. copy demo data to demo folder
#  4. sync files

#' Copy Package Folder to Target Directory (Internal)
#'
#' Copies a specified folder from the package's installation directory to a target directory.
#' The source folder defaults to `inst/extdata` within the package, and the target directory
#' defaults to the current working directory.
#'
#' @param folder_name Character. The folder within the package to copy. Defaults to `"extdata"`.
#' @param target_dir Character. The directory where the folder will be copied. Defaults to the current working directory `"."`.
#' @return NULL. The function copies the folder and provides a message upon success.
#' @keywords internal
.su_package_folder_copy <- function(
    folder_name,
    target_dir = ".",
    .overwrite = TRUE
) {
  # Define the full source path (e.g., inst/extdata within the package)
  full_source_path <- fs::path(
    system.file("extdata", folder_name, package = "padt")
  )

  # Validate the source path
  if (!fs::dir_exists(full_source_path)) {
    stop(paste0(
      "The folder '", folder_name,
      "' does not exist in the package directory: ", full_source_path
    ))
  }

  # Define the full target path (folder inside the target directory)
  full_target_path <- fs::path(target_dir, folder_name)

  # Handle overwriting behavior
  if (.overwrite && fs::dir_exists(full_target_path)) {
    fs::dir_delete(full_target_path)
  }

  # Copy the folder if it does not already exist or if overwritten
  if (!fs::dir_exists(full_target_path)) {
    fs::dir_copy(full_source_path, full_target_path)
    # Inform the user of the successful copy
    packageStartupMessage(crayon::green(
     "Folder: '", folder_name, "' successfully created at: ", full_target_path
    ))
  } else {
    # Inform the user that the folder already exists and was not overwritten
    packageStartupMessage(crayon::yellow(
     "Folder '", folder_name, "' already exists, No changes made: ", full_target_path
    ))
  }
}


#' Create Data Folder Structure (Internal)
#'
#' Internal helper function to create a folder structure based on the root directory,
#' BS(GP) tiers, functional areas, and DTAP stages.
#'
#' @param root_dir Character. The root directory where the folder structure will be created.
#' @param .bsgp Character vector. The Bronze, Silver, Gold, Platinum tiers, defaults to `.BGSP`.
#' @param .area Character vector. The functional areas, defaults to `.AREA`.
#' @param .dtap Character vector. The DTAP stages, defaults to `.DTAP`.
#' @return NULL. The function creates the directory structure and does not return anything.
#' @keywords internal
.su_data_folders_create <- function(
    root_dir,
    .bsgp = .BSGP,
    .area = .AREA,
    .dtap = .DTAP
) {

  # Ensure root directory exists, existing directories are left unchanged
  fs::dir_create(root_dir)

  # Create the directory structure, existing directories are left unchanged
  for (dtap in .dtap) {
    for (bsgp in .bsgp) {
      for (area in .area) {
        fs::dir_create(fs::path(root_dir, dtap, bsgp, area))
      }
    }
  }

  message(green("Data folder structure created successfully in: ", root_dir))
}

#' Initialize the padt environment
#'
#' This function initializes the `.padt_env` environment for development or testing purposes.
#' It mimics the `.onLoad` behavior to set up the `config.yaml` path, root directory, and any
#' other necessary package-specific settings. Use this function during development without needing
#' to reload the entire package.
#'
#' @details
#' - For testing or `devtools::check()` contexts, it sets the root directory to a temporary folder.
#' - For development within the package directory, it uses `inst/extdata` as the root directory.
#' - For regular use, it copies the configuration folder to the user's project directory (`"."`).
#'
#' @return NULL. This function is used for side effects.
#' @keywords internal
.su_padt_initialize <- function() {
  tryCatch(
    {
      prj_fldr <- "."

      # 1) Prioritize testthat and R CMD check contexts
      if (testthat::is_testing() || nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
        # Use a dedicated sub directory inside tempdir for testing or check
        temp_dir <- fs::path(tempdir(), .PACKAGE_NAME)
        if (!fs::dir_exists(temp_dir)) {
          fs::dir_create(temp_dir)
        }

        # copy the configuration and demo data to the tempdir
        .su_package_folder_copy(CONFIG_FLDR, temp_dir, TRUE)
        .su_package_folder_copy(PADEMO_FLDR, temp_dir, TRUE)

        # set the root directory
        .padt_env$root_dir  <- fs::path(temp_dir)
        # packageStartupMessage("Root directory set to tempdir: ", temp_dir)

        # 2) Check for a development context inside the package directory
      } else if (fs::dir_exists(fs::path("inst", "extdata"))) {

        # set the root directory
        .padt_env$root_dir <- fs::path("inst", "extdata")
        # packageStartupMessage("Root directory set to inst/extdata.")

        # 3) Otherwise, assume a normal user project directory
      } else {

        .su_package_folder_copy(CONFIG_FLDR, prj_fldr, FALSE)
        .su_package_folder_copy(PADEMO_FLDR, prj_fldr, FALSE)

        # set the root directory
        .padt_env$root_dir <- fs::path(prj_fldr)
        # packageStartupMessage("Root directory set to project folder: ", prj_fldr)
      }

      # load the configuration file
      .padt_env$cfg_path  <- fs::path(.padt_env$root_dir, CONFIG_FLDR, CONFIG_YAML)
      .padt_env$cfg       <- yaml::yaml.load_file(.padt_env$cfg_path)
      .padt_env$cfg_mtime <- file.info(.padt_env$cfg_path)$mtime

      # Debug messages
      # packageStartupMessage("Tempdir used: ", tempdir())
      # packageStartupMessage("Final root_dir: ", .padt_env$root_dir)


      # Inform the user
      packageStartupMessage(
        crayon::green(paste0("`", .PACKAGE_NAME, "` initialized successfully."))
      )
    },
    error = function(e) {
      warning("Initialization failed: ", conditionMessage(e))
    }
  )
}
