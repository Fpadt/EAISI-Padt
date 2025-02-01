# SCOPE_PLNT <-
#   "WITH SCOPE_PLNT AS (
#      SELECT DISTINCT
#        PLANT
#      FROM
#        read_parquet([{`FN_FRPR1`}, {`FN_FRPR3`}])
#      WHERE
#        SALESORG IN ({SCOPE_SORG*})
#      )
#   "

#' Display Verbose Output for Function Execution
#'
#' Helper function to print details about a function's name, arguments, and return value if verbose mode is enabled.
#'
#' @param function_name Character. The name of the function being executed.
#' @param function_args List. The arguments passed to the function. Defaults to \code{NULL}.
#' @param function_return Any. The return value of the function. Defaults to \code{NULL}.
#' @param verbose Logical. Whether to print the verbose output. Defaults to \code{FALSE}.
#'
#' @return This function does not return anything but prints details to the console if \code{verbose = TRUE}.
#'
#' @examples
#' # Example for internal testing
#' @keywords internal
.verbose <- function(
    function_name = NULL,
    function_args = NULL,
    function_return = NULL,
    verbose = FALSE
) {
  if (isTRUE(verbose)) {
    cat("Function name:\n")
    cat("  ", function_name, "\n\n")

    if (!is.null(function_args)) {
      cat("Function arguments:\n")
      print(function_args)
      cat("\n")
    }

    if (!is.null(function_return)) {
      cat("Function return value:\n")
      print(function_return)
      cat("\n")
    }
  }
}




#' Internal function to retrieve most recent config file
#'
#' This function checks if the configuration file should be reloaded based on
#' the file modification time or forced by the `.reload` flag.
#'
#' @param .reload Logical, whether to force reload the config file. Default is FALSE.
#' @return The most recent configuration as a list.
.hl_config_get <- function(.reload = FALSE) {

  # Ensure the required path is available
  if (is.null(.padt_env$cfg_path) || !file.exists(.padt_env$cfg_path)) {
    stop("Config file path is not set or does not exist.")
  }

  # Reload the config if .reload is TRUE or the file is newer than the cached version
  if (.reload ||
      file.info(.padt_env$cfg_path)$mtime > .padt_env$cfg_mtime) {

    # Reload the config file
    .padt_env$cfg       <- yaml::read_yaml(.padt_env$cfg_path)
    .padt_env$cfg_mtime <- file.info(.padt_env$cfg_path)$mtime
    # message("Configuration reloaded from ", .padt_env$cfg_path)
  }

  # Return the cached or reloaded config
  .padt_env$cfg
}



.hl_convert_type <- function(
    .ftype  = NULL,  # If NULL, return all
    .vtype  = NULL,  # If NULL, return all
    .config = NULL  # Reserved for future use (optional)
) {

  # Define the mapping as a data.table
  conversion_table <- fread(text = "
    ftype,vtype,output, description
    1    , 010, abr   , Actuals  for Accuracy PreDR from 202401
    2    , 010, aar   , Actuals  for Accuracy PstDR from 202401
    3    , 010, lbr   , Actuals  Last Version PreDR from 202101
    4    , 010, lar   , Actuals  Last Version PstDR from 202101
    1    , 060, fbr   , Forecast for Accuracy PreDR from 202401
    2    , 060, far   , Forecast for Accuracy PstDR from 202401
  ",colClasses = c("vtype" = "character"))

  # 1. Return all values if both .ftype and .vtype are NULL
  if (is.null(.ftype) & is.null(.vtype)) {
    return(unique(conversion_table$output))
  }

  # 2. Return all values for a given .vtype if .ftype is NULL
  if (is.null(.ftype)) {
    return(conversion_table[vtype %chin% .vtype, output])
  }

  # 3. Return all values for a given .ftype if .vtype is NULL
  if (is.null(.vtype)) {
    return(conversion_table[ftype %in% .ftype, output])
  }


  # Retrieve the corresponding value
  result <- conversion_table[ftype %in% .ftype & vtype %chin% .vtype, output]

  if (length(result) == 0) {
    stop("No matching entry found for the given FTYPE and VTYPE.")
  }

  return(result)
}

