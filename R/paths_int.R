#' Get Full File Names as a Comma-Separated String
#'
#' Constructs full file paths for a given combination of parameters and returns them as a single string separated by commas.
#'
#' @param .pa_BSGP Integer. The index corresponding to the pa_BSGP (Bronze, Silver, Gold, Platinum) tier. Must align with the \code{pa_BSGP} vector.
#' @param .pa_AREA Integer. The index corresponding to the functional pa_AREA. Must align with the \code{pa_AREA} vector.
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
    .pa_BSGP,
    .pa_AREA,
    .vtype,
    .ftype,
    .etype
) {
  # Validate input parameters
  if (missing(.pa_BSGP) || missing(.pa_AREA) || missing(.vtype) || missing(.ftype) || missing(.etype)) {
    stop("All arguments (.pa_BSGP, .pa_AREA, .vtype, .ftype, .etype) must be provided.")
  }

  # Load paths table (should ideally be moved to a configuration setup or loaded once for efficiency)
  paths_parquet_files <- data.table::fread("
    pa_AREA, vtype, ftype, all, fname
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
    pa_get_wd()
  }, error = function(e) {
    stop("Error retrieving root directory: ", e$message)
  })

  # Validate if .pa_BSGP and .pa_AREA are within acceptable ranges
  if (.pa_BSGP > length(pa_BSGP) || .pa_AREA > length(pa_AREA)) {
    stop("Invalid .pa_BSGP or .pa_AREA value. Ensure it aligns with pa_BSGP and pa_AREA definitions.")
  }

  # Filter paths and construct full file paths
  filtered_files <- paths_parquet_files[
    pa_AREA == .pa_AREA & vtype %chin% .vtype & ftype %in% .ftype,
    file.path(
      root_dir, pa_BSGP[.pa_BSGP], pa_AREA[.pa_AREA],
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
