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


#' Create DTAP Folder Structure
#'
#' @description
#' An internal helper function that creates the required folder structure
#' for a given environment (e.g., production) based on the YAML configuration.
#'
#' @param config A nested list representing your YAML configuration
#'   (already read via \code{\link[yaml]{yaml.load_file}}).
#' @param environment A character string specifying the environment in DTAP
#'   (e.g., \code{"production"}, \code{"development"}, \code{"test"}, \code{"acceptance"}).
#' @param create_platinum Logical indicating whether to create platinum folders
#'   for new forecast outputs. Defaults to \code{TRUE}.
#' @param verbose Logical; if \code{TRUE}, messages about created folders
#'   are printed.
#'
#' @details
#' This function assumes the YAML has a structure similar to:
#' \preformatted{
#'   root: OneDriveConsumer/ET/pythia/data
#'   environment: production
#'   functional_areas:
#'     sales:
#'       datasets:
#'         rtp:
#'           directory: sales_rtp
#'           ...
#'           staging:
#'             bronze:
#'               pattern: ...
#'               extension: csv
#'             silver:
#'               pattern: ...
#'               extension: parquet
#'             platinum:
#'               ...
#' }
#'
#' The function iterates through each functional area, dataset, and staging level,
#' constructing folder paths of the form:
#' \code{file.path(root, <staging_level>, <directory>)}.
#'
#' @return
#' Returns \code{TRUE} (invisibly) if successful. Creates folders on disk as a side effect.
#'
#' @examples
#' \dontrun{
#'   # Suppose you have read your YAML config into 'cfg':
#'   # cfg <- yaml::yaml.load_file("path/to/your_config.yaml")
#'
#'   # Create folder structure in 'production'
#'   create_dtap_folders(cfg, environment = "production")
#' }
#'
#' @keywords internal
.si_create_dtap_folders <- function(config,
                                environment = "production",
                                create_platinum = TRUE,
                                verbose = TRUE) {
  # 1. Get root path for the specified environment:
  #    If your YAML is nested as "dtap: production: root: ...",
  #    adjust the extraction code accordingly.
  root_path <- config[["root"]]

  # 2. Access functional areas
  functional_areas <- config[["functional_areas"]]
  if (is.null(functional_areas)) {
    if (verbose) message("No functional areas found in config.")
    return(invisible(TRUE))
  }

  # 3. Loop over each functional area
  for (fa_name in names(functional_areas)) {
    area <- functional_areas[[fa_name]]

    # 3a. Loop over datasets
    datasets <- area[["datasets"]]
    if (is.null(datasets)) next

    for (ds_name in names(datasets)) {
      dataset <- datasets[[ds_name]]

      # Directory name (subfolder under staging)
      dir_name <- dataset[["directory"]]

      # 3b. Loop over staging levels
      staging_list <- dataset[["staging"]]
      if (is.null(staging_list)) next

      for (stg_name in names(staging_list)) {
        # Optionally skip platinum if create_platinum = FALSE
        if (stg_name == "platinum" && !create_platinum) next

        # Construct the full folder path
        # e.g., "<root>/<stg_name>/<dir_name>"
        folder_path <- file.path(root_path, stg_name, dir_name)

        # Create folders if they don't exist
        if (!dir.exists(folder_path)) {
          dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
          if (verbose) message("Created folder: ", folder_path)
        }
      }
    }
  }

  invisible(TRUE)
}


