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


#' Get Full File Names as a Comma-Separated String
#'
#' Constructs full file paths for a given combination of parameters and returns them as a single string separated by commas.
#'
#' @param .bsgp Integer. The index corresponding to the BSGP (Bronze, Silver, Gold, Platinum) tier. Must align with the \code{BSGP} vector.
#' @param .area Integer. The index corresponding to the functional area. Must align with the \code{AREA} vector.
#' @param .vtype Character. The data type code (e.g., "010", "060").
#' @param .ftype Integer. The file type code used to identify the file in the configuration table.
#' @param .etype Character. The file extension (e.g., "parquet", "csv").
#'
#' @return A character string containing normalized full file paths, separated by commas.
#'
#' @details
#' This function retrieves file names from a predefined configuration table of file paths and constructs the full file paths based on the specified parameters. The resulting paths are returned as a single string for easy inclusion in SQL queries or other contexts.
#' @examples
#' # This function requires a properly configured environment with a config.yaml.
#' # No examples are provided to avoid errors during package checks.
#' @import data.table
#' @keywords internal
.get_data_full_file_names <- function(
    .bsgp,
    .area,
    .vtype,
    .ftype,
    .etype
) {
  # Validate input parameters
  if (missing(.bsgp) || missing(.area) || missing(.vtype) || missing(.ftype) || missing(.etype)) {
    stop("All arguments (.bsgp, .area, .vtype, .ftype, .etype) must be provided.")
  }

  # Load paths table (should ideally be moved to a configuration setup or loaded once for efficiency)
  paths_parquet_files <- data.table::fread("
    area, vtype, ftype, all, fname
    1   , 010  , 1,     *  , SDSFRPR1
    1   , 010  , 3,     *  , SDSFRLV1
    1   , 010  , 2,     *  , SDSFRPR3
    1   , 010  , 4,     *  , SDSFRLV3
    1   , 060  , 1,     *  , SDSFRPR2
    1   , 060  , 2,     *  , SDSFRPR4
    2   , 010  , 1,     *  , IMP03SM1
    3   , 010  , 1,     *  , PROMO
    4   , 010  , 1,        , MD_MATERIAL
    4   , 010  , 2,        , MD_MATERIAL_SALES_ORG
    4   , 010  , 3,        , MD_MATERIAL_PLANT
    4   , 010  , 4,        , MD_SOLD_TO_CUSTOMER
    4   , 010  , 5,        , MD_BOMX
    4   , 010  , 6,        ,
    4   , 010  , 7,        ,
  ", header = TRUE, sep = ",", colClasses = list(character = "vtype"))

  # Validate root directory
  root_dir <- tryCatch({
    get_environment_path()
  }, error = function(e) {
    stop("Error retrieving root directory: ", e$message)
  })

  # Validate if .bsgp and .area are within acceptable ranges
  if (.bsgp > length(BSGP) || .area > length(AREA)) {
    stop("Invalid .bsgp or .area value. Ensure it aligns with BSGP and AREA definitions.")
  }

  # Filter paths and construct full file paths
  filtered_files <- paths_parquet_files[
    area == .area & vtype %chin% .vtype & ftype %in% .ftype,
    file.path(
      root_dir, BSGP[.bsgp], AREA[.area],
      paste0(fname, all, ".", .etype)
    )
  ]

  # Check if any files were found
  if (length(filtered_files) == 0) {
    stop("No matching files found for the provided parameters.")
  }

  # Return as a comma-separated string
  return(filtered_files %>%
           normalizePath(mustWork = FALSE, winslash = "/") %>%
           paste0("'", ., "'", collapse = ", "))
}



#
#
# # Master Data
# FN_MATL <-
#   file.path(P_MD, "MD_MATERIAL.parquet")           %>%
#   normalizePath()
#
# FN_MATS <-
#   file.path(P_MD, "MD_MATERIAL_SALES_ORG.parquet") %>%
#   normalizePath()
#
# FN_MATP <-
#   file.path(P_MD, "MD_MATERIAL_PLANT.parquet")     %>%
#   normalizePath()
#
# FN_CUST <-
#   file.path(P_MD, "MD_SOLD_TO_CUSTOMER.parquet")   %>%
#   normalizePath()
#
# # Transaction Data
#
# ## Invoiced Sales for Dynasys Cloud RTP
# FN_IRTP <-                # pre-Demand review
#   file.path(PSLS, paste0("DD_SALES_QTY_202*"))
#
# ## Invoiced Sales for Dynasys on-premise 2018
# FN_IIPM <-                # pre-Demand review
#   file.path(PSLS, paste0("DD_HISTO_QTY_202*"))
#
# # Stock
# FN_STCK <-
#   file.path(PSTK, paste0("IMP03SM1*"))
#
# FN_FRPR1 <-               # pre-Demand review
#   file.path(PSLS, .get_parquet_path("010", 1))
#
# FN_FRPR5 <-               # pre-Demand review
#   file.path(PSLS, .get_parquet_path("010", 5))
#
# FN_FRPR3 <-               # pst-Demand review
#   file.path(PSLS, .get_parquet_path("010", 2))
#
#
# FN_FRPR6 <-               # pst-Demand review
#   file.path(PSLS, .get_parquet_path("010", 6))
#
# ## Forecasts
# FN_FRPR2 <-               # pre-Demand review
#   file.path(PSLS, .get_parquet_path("060", 1))
#
# FN_FRPR4 <-               # pst-Demand review
#   file.path(PSLS, .get_parquet_path("060", 2))
