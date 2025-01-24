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
    pa_get_environment_path()
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
