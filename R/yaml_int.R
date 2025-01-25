#' Create Default Config YAML File (Internal)
#'
#' Internal helper function to create a default `config.yaml` file in the specified directory.
#'
#' @param config_dir Character. The directory where the `config.yaml` file will be created. Defaults to `"./inst/extdata"`.
#' @param key_value_list List. A named list of key-value pairs to set in the configuration file.
#' @examples
#' .create_default_config_yaml(
#'   config_dir = "./inst/extdata",
#'   key_value_list = list(
#'     "project.name"        = "Pythia's Advice",
#'     "project.department"  = "EAISI",
#'     "csv_file_spec.delim" = ";"
#'     )
#'   )
#' @keywords internal
.create_default_config_yaml <- function(
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
.create_instance_of_default_config_yaml <- function(
    config_dir     = "./inst/extdata",
    key_value_list = NULL) {

  # Define the path to the YAML file
  config_file <- fs::path(config_dir, ".config.yaml")

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
