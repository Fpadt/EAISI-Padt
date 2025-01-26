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
    system.file("extdata", folder_name, package = "padt"))

  # Validate the source path
  if (!fs::dir_exists(full_source_path)) {
    stop(paste0(
      "The folder '", folder_name,
      "' does not exist in the package directory: ", full_source_path))
  }

  # Define the absolute target path
  full_target_path <- fs::path_abs(target_dir, folder_name)

  if (.overwrite == TRUE && fs::dir_exists(full_target_path)) {
    fs::dir_delete(full_target_path)
  }

  if (!fs::dir_exists(full_target_path)) {

    # Copy the folder to the target directory
    fs::dir_copy(full_source_path, target_dir, overwrite = .overwrite )

    # Inform the user of the successful copy
    message(
      green("Folder '", folder_name, "' successfully copied to: ",
            target_dir))
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

#' Create Default Config YAML File (Internal)
#'
#' Internal helper function to create a default `config.yaml` file in the specified directory.
#'
#' @param config_dir Character. The directory where the `config.yaml` file will be created. Defaults to `"./inst/extdata"`.
#' @param key_value_list List. A named list of key-value pairs to set in the configuration file.
#' @keywords internal
.su_config_default_create <- function(
    config_dir = "./inst/extdata",
    key_value_list = NULL
) {
  # Ensure the directory exists
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
    message("Directory created: ", config_dir)
  }

  # Use default key-value pairs if none are provided
  if (is.null(key_value_list)) {
    key_value_list <- list(
      # Project settings
      "project.name"          = "Pythia's Advice",
      "project.department"    = "EAISI",

      # CSV file specifications
      "csv_file_spec.delim"     = ";",
      "csv_file_spec.date_format" = "%Y-%m-%d"
    )
  }

  # Set each key-value pair in the configuration file
  for (key in names(key_value_list)) {
    pa_config_set_value(
      .key       = key,
      .value     = key_value_list[[key]],
      config_dir = config_dir
    )
  }

  message("Default config.yaml created in: ", config_dir)
}

#' Create Default Config YAML File (Internal)
#'
#' Internal helper function to create a default `config.yaml` file in the specified directory.
#'
#' @param config_dir Character. The directory where the `config.yaml` file will be created. Defaults to `"./inst/extdata"`.
#' @param key_value_list List. A named list of key-value pairs to set in the configuration file.
#' @keywords internal
.su_config_default_write <- function(
    config_dir     = "./inst/extdata/config",
    key_value_list = NULL) {

  # Define the path to the YAML file
  config_file <- fs::path(config_dir, CONFIG_YAML)

  # Delete the existing .config.yaml file if it exists
  if (file.exists(config_file)) {
    file.remove(config_file)
    message("Existing .config.yaml file deleted: ", config_file)
  }

  # Use default key-value pairs if none are provided
  if (is.null(key_value_list)) {
    key_value_list <- list(
      # Project settings
      "project.name"               = "Pythia's Advice",
      "project.department"         = "EAISI",

      # Data location
      "data_dir"                   = "./data",
      "environment"                = "production",

      # CSV file specifications
      "csv_file_spec.delim"        = ";",
      "csv_file_spec.header"       = FALSE,
      "csv_file_spec.date_format"  = "%Y-%m-%d",

      # Sales pipeline settings
      "sales.details.file_pattern" = "^DD_SALES_QTY_202[12345].*\\.csv$"
    )
  }

  # Set each key-value pair in the configuration file
  for (key in names(key_value_list)) {
    pa_config_set_value(
      .key       = key,
      .value     = key_value_list[[key]],
      config_dir = config_dir
    )
  }

  message("Default config.yaml created in: ", config_dir)
}


