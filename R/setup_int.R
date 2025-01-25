# prerequisite(s):
#  - execute yaml_int::.config_default_write()
#
# Set Up - after package installation
#  1. .copy_package_folder
#  2. .create_data_folders()
#  3. copy demo data to demo folder
#  4. sync files

#' Copy Package Folder to Target Directory (Internal)
#'
#' Copies a specified folder from the package's installation directory to a target directory.
#' The source folder defaults to `inst/extdata` within the package, and the target directory
#' defaults to the current working directory.
#'
#' @param folder_name Character. The folder within the package to copy. Defaults to `"extdata"`.
#' @param source_dir Character. The base path within the package where the folder resides. Defaults to `"inst"`.
#' @param target_dir Character. The directory where the folder will be copied. Defaults to the current working directory `"."`.
#' @return NULL. The function copies the folder and provides a message upon success.
#' @examples
#' # Copy the extdata folder to the current project directory
#' .copy_package_folder(folder_name = "extdata", source_dir = "inst", target_dir = ".")
#'
#' # Copy a different folder from the package to a custom directory
#' .copy_package_folder(folder_name = "data", source_dir = "inst", target_dir = "./my_project")
#' @keywords internal
.copy_package_folder <- function(
    folder_name,
    source_dir = "extdata",
    target_dir = ".",
    .overwrite = TRUE
) {

  # Get the package's installation path
  package_path <- system.file(package = .PACKAGE_NAME, mustWork = TRUE)

  # Define the full source path (e.g., inst/extdata within the package)
  full_source_path <- fs::path(package_path, source_dir, folder_name)

  # Validate the source path
  if (!fs::dir_exists(full_source_path)) {
    stop(paste0(
      "The folder '", folder_name,
      "' does not exist in the package directory: ", full_source_path))
  }

  # Define the absolute target path
  full_target_path <- fs::path_abs(target_dir)

  # Ensure the target directory exists
  fs::dir_create(full_target_path)

  # Copy the folder to the target directory
  fs::dir_copy(full_source_path, full_target_path, overwrite = .overwrite)

  # Inform the user of the successful copy
  message(
    green("Folder '", folder_name, "' successfully copied to: ",
          full_target_path))
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
#' @examples
#' # Create the default folder structure
#' .create_data_folders(
#'   root_dir = "./data",
#'   .bsgp = .BGSP,
#'   .area = .AREA
#'   .dtap = .DTAP
#' )
#' @keywords internal
.create_data_folders <- function(
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
