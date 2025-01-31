#' List Files in Directory with Metadata (Internal)
#'
#' Internal function to create a data.table containing information about all files
#' in a given directory.
#'
#' @param dir Character. The directory to scan for files.
#' @return A data.table with the following columns:
#' \describe{
#'   \item{FFN}{Full file name (absolute path).}
#'   \item{BFN}{Base file name (without extension).}
#'   \item{EXT}{File extension.}
#'   \item{MOD}{Modification time.}
#' }
#' @keywords internal
.list_files_with_metadata <- function(dir) {
  # Load required packages
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("The 'fs' package is required but not installed. Please install it with install.packages('fs').")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required but not installed. Please install it with install.packages('data.table').")
  }

  # Validate the directory
  if (!fs::dir_exists(dir)) {
    stop("The specified directory does not exist: ", dir)
  }

  # List all files in the directory
  files <- fs::dir_ls(dir, recurse = FALSE, type = "file")

  # Create a data table with the required structure
  file_info <- data.table::data.table(
    FFN = files,                                       # Full file name (absolute path)
    BFN = fs::path_ext_remove(fs::path_file(files)),   # Base file name (without extension)
    EXT = fs::path_ext(files),                         # File extension
    MOD = fs::file_info(files)$modification_time       # Modification time
  )

  return(file_info)
}

#' Get New and Modified Files Between Two Directories (Internal)
#'
#' Internal function to compare two directories (source and target) and identify files
#' that are new or modified in the source directory compared to the target directory.
#'
#' @param dir_src Character. Path to the source directory.
#' @param dir_tgt Character. Path to the target directory.
#' @return A character vector of full file names (absolute paths) for files that are
#' new or modified in the source directory.
#' @keywords internal
.sync_get_new_and_modified_files <- function(dir_src, dir_tgt) {
  # Load required packages
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required but not installed. Please install it with install.packages('data.table').")
  }

  # Validate directories
  if (!fs::dir_exists(dir_src)) {
    stop("The source directory does not exist: ", dir_src)
  }
  if (!fs::dir_exists(dir_tgt)) {
    stop("The target directory does not exist: ", dir_tgt)
  }

  # Get metadata for both directories
  pa_src <- .list_files_with_metadata(dir_src) %>%
    data.table::setnames(paste0(names(.), "_src"))

  pa_tgt <- .list_files_with_metadata(dir_tgt) %>%
    data.table::setnames(paste0(names(.), "_tgt"))

  # Find new files (files in source but not in target)
  pa_new <- pa_src[!pa_tgt, on = .(BFN_src == BFN_tgt)]

  # Find modified files (files with the same name but newer modification time in source)
  pa_mod <- pa_src[pa_tgt,
                   on = .(BFN_src == BFN_tgt, MOD_src > MOD_tgt), nomatch = 0]

  # Combine new and modified files
  files_to_sync <- list(NEW = pa_new$FFN_src, MOD = pa_mod$FFN_src)

  return(files_to_sync)
}

#' Construct Staging Path with Environment
#'
#' @description
#' Creates a folder path of the form:
#'   \code{root / environment / staging / directory}.
#'
#' @param config A list representing your YAML configuration (already read via
#'   \code{\link[yaml]{yaml.load_file}}).
#' @param functional_area A character string (e.g., \code{"sales"}).
#' @param dataset A character string (e.g., \code{"rtp"}).
#' @param staging A character string for the staging level, commonly
#'   \code{"bronze"}, \code{"silver"}, or \code{"platinum"}.
#' @param environment A character string specifying the environment,
#'   e.g., \code{"Production"}, \code{"Test"}, \code{"Development"}.
#'   Defaults to \code{"Production"}.
#'
#' @return
#' A string with the full path to the specified staging folder (without file name).
#'
#' @details
#' This function assumes your YAML structure looks like this:
#' \preformatted{
#'   root: OneDriveConsumer/ET/pythia/data
#'   functional_areas:
#'     sales:
#'       datasets:
#'         rtp:
#'           directory: sales_rtp
#'           ...
#' }
#'
#' and appends \code{environment} and \code{staging} before the
#' \code{directory} name:
#' \code{file.path(root, environment, staging, directory)}.
#'
#'
#' @examples
#' \dontrun{
#'   # Suppose 'cfg' is your loaded YAML list
#'   # e.g. cfg <- yaml::yaml.load_file("path/to/config.yaml")
#'
#'   # Construct path for sales_rtp in 'bronze' under 'Production':
#'   bronze_path <- construct_staging_path(
#'     config = cfg,
#'     functional_area = "sales",
#'     dataset = "rtp",
#'     staging = "bronze",
#'     environment = "Production"
#'   )
#'   # "OneDriveConsumer/ET/pythia/data/Production/bronze/sales_rtp"
#' }
#'
#' @keywords internal
.fh_path_dataset_get <- function(
    environment = c("development", "test"  , "acceptance", "production"),
    staging     = c("bronze"     , "silver", "gold"      , "platinum"),
    functional_area,
    dataset,
    config      = .hl_config_get()
) {

  sales_datasets <- config$datasets$functional_areas[[functional_area]]$datasets

  for (ds in sales_datasets) {
    cat("Name:", ds$name, "\n")
    cat("Directory:", ds$directory, "\n")
    cat("Pattern:"  , ds$staging$bronze$pattern  , "\n")
    cat("Extension:", ds$staging$bronze$extension, "\n")
    # ...
  }

  # browser()
  # If environment is unspecified or partial, match with available choices
  environment <- match.arg(environment)

  # If staging is unspecified or partial, match with available choices
  staging <- match.arg(staging)

  # Extract the root from YAML
  # root_path <- config[["root"]]

  # If you have environment keys in the YAML, use that structure instead:
  root_path <- config[["dtap"]][[tolower(environment)]][["root"]]

  # Extract the specific directory for the dataset
  dir_name <-
    config                  %>%
    .[["datasets"]]         %>%
    .[["functional_areas"]] %>%
    .[[functional_area]]    %>%
    .[["datasets"]]         %>%
    .[[dataset]]            %>%
    .[["directory"]]

  # Build final path: root / environment / staging / directory
  full_path <- file.path(root_path, environment, staging, dir_name)

  return(full_path)
}

#' Replace Absolute Path with OneDrive Root Identifier
#'
#' This internal function replaces an absolute path with a OneDrive root identifier
#' (`OneDriveConsumer` or `OneDriveCommercial`) if the path belongs to the respective
#' OneDrive directory.
#' If the path does not belong to either directory, the original path is returned unchanged.
#'
#' @param abs_path Character. The absolute path provided by the user.
#' @return Character. The modified path with the OneDrive root identifier or the original path.
#' @keywords internal
.fh_onedrive_rel <- function(abs_path) {

  # Get OneDrive paths from environment variables
  onedrive_consumer   <- fs::path_abs(Sys.getenv("OneDriveConsumer"  , ""))
  onedrive_commercial <- fs::path_abs(Sys.getenv("OneDriveCommercial", ""))

  # Check if the absolute path belongs to OneDriveConsumer or OneDriveCommercial
  if (fs::path_has_parent(abs_path, onedrive_consumer)) {
    # Replace the consumer path
    rel_path <- fs::path_rel(abs_path, start = onedrive_consumer)
    fs::path("OneDriveConsumer", rel_path)
  } else if (fs::path_has_parent(abs_path, onedrive_commercial)) {
    # Replace the commercial path
    rel_path <- fs::path_rel(abs_path, start = onedrive_commercial)
    fs::path("OneDriveCommercial", rel_path)
  } else {
    # Return the original path if it does not belong to OneDrive
    abs_path
  }
}

#' Resolves relative Path with OneDrive Root Identifier to absolute path
#'
#' This internal function replaces an relative path with a OneDrive root identifier
#' (`OneDriveConsumer` or `OneDriveCommercial`)
#'
#' @param rel_path Character. The absolute path provided by the user.
#' @return Character. The modified path with the OneDrive root identifier.
#' @keywords internal
.fh_onedrive_abs <- function(rel_path) {

  # Get OneDrive paths from environment variables
  rel_path            <- fs::path_abs(rel_path)
  onedrive_consumer   <- fs::path_abs(Sys.getenv("OneDriveConsumer"  , ""))
  onedrive_commercial <- fs::path_abs(Sys.getenv("OneDriveCommercial", ""))

  if (path_has_parent(rel_path, "OneDriveConsumer")) {
    sub_path <- path_rel(rel_path, start = "OneDriveConsumer")
    rel_path <- path(onedrive_consumer, sub_path)
  } else if (path_has_parent(rel_path, "OneDriveBusiness")) {
    sub_path <- path_rel(rel_path, start = "OneDriveBusiness")
    rel_path <- path(onedrive_commercial, sub_path)
  }

  return(rel_path)
}

.fh_data_file_pattern_get <-
  function(.area, .set, .dir = c("i", "a")) {

    # basename full part ext
    # filepattern <- pa_config_value_get(paste("data", .area, .sect, "filepattern", sep = "."))

    dat <- .pi_onedrive_abs(pa_config_value_get("data.root"))
    env <- pa_config_value_get("environment")
    set <- pa_config_value_get(paste("data", .area, .sub, sep = "."))

    fs::path_abs(
      file.path(
        dat, env,
        setfld, bsn))

  }


