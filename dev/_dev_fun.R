
#' Get All Objects in the Package Namespace (Internal)
#'
#' Returns a data.table listing all objects in the package namespace, including
#' whether they are internal or external and whether they are functions.
#'
#' @return A data.table with columns:
#' \describe{
#'   \item{OBJ}{Character. The name of the object.}
#'   \item{ACC}{Character. Access level: "E" for exported (external) or "I" for internal.}
#'   \item{FUN}{Logical. TRUE if the object is a function, FALSE otherwise.}
#' }
#' @examples
#' .get_functions()
#' @keywords internal
.get_functions <- function() {
  # Ensure required packages are loaded
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required but not installed. Please install it using install.packages('data.table').")
  }

  # Load data.table
  library(data.table)
  devtools::load_all()

  # Package name (change "padt" to your actual package name)
  package_name <- "padt"

  # Get all objects in the package namespace, including those starting with a dot
  all_objects <- ls(getNamespace(package_name), all.names = TRUE)

  # Create the data.table
  DT <- data.table(
    OBJ = all_objects,
    ACC = ifelse(all_objects %in% getNamespaceExports(package_name), "E", "I"),
    FUN = sapply(all_objects, function(obj) is.function(get(obj, envir = asNamespace(package_name))))
  )

  .open_as_xlsx(DT)
}

.open_as_xlsx <-
  function(pDT, pPath = tempdir(), pFN, pasTable = TRUE){

    library(lubridate)
    library(openxlsx)

    if (!dir.exists(pPath)) {
      dir.create(pPath)
    }

    if (missing(pFN) == TRUE) {
      pFN <- paste0("~", format(now(), "%Y%m%d-%H%M%S"), ".xlsx")
    }

    FFN <- file.path(pPath, pFN)
    write.xlsx(x = pDT, file = FFN, asTable = pasTable,
               tableStyle = "TableStyleMedium4")
    openXL(FFN)

  }

#' Create Default Config YAML File (Internal)
#'
#' Internal helper function to create a default `config.yaml` file in the specified directory.
#'
#' @param config_dir Character. The directory where the `config.yaml` file will be created. Defaults to `"./inst/extdata"`.
#' @param key_value_list List. A named list of key-value pairs to set in the configuration file.
#' @keywords internal
.su_config_default_write <- function() {

  # Define the path to the YAML file
  config <- .hl_config_get()

  # Delete the existing .config.yaml file if it exists
  if (file.exists(config_file)) {
    file.remove(config_file)
    message("Existing .config.yaml file deleted: ", config_file)
  }

  # Use default key-value pairs if none are provided
  key_value_list <- list(
    # Project settings
    "project.name"          = "Pythia's Advice",
    "project.department"    = "EAISI",

    # Data location
    "data_dir"              = ".",
    "environment"           = "sample_data",

    # CSV file specifications
    "csv_file_spec.delim"        = ";",
    "csv_file_spec.header"       = FALSE,
    "csv_file_spec.date_format"  = "%Y-%m-%d",

    # Sales transformation settings
    "sales.details.file_pattern" = "^DD_SALES_QTY_202[12345].*\\.csv$"
  )


  # Set each key-value pair in the configuration file
  for (key in names(key_value_list)) {
    pa_config_value_set(
      .key         = key,
      .value       = key_value_list[[key]],
      .config_path = .padt_env$cfg_path
    )
  }

  message("Default config.yaml created in: ", .padt_env$cfg_path)
}
