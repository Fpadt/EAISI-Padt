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
.fh_files_metadata_list_get <- function(dir) {

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
.fh_files_sync_list_get <- function(dir_src, dir_tgt) {

  # Validate directories
  if (!fs::dir_exists(dir_src)) {
    stop("The source directory does not exist: ", dir_src)
  }
  if (!fs::dir_exists(dir_tgt)) {
    stop("The target directory does not exist: ", dir_tgt)
  }

  # Get metadata for both directories
  pa_src <- .fh_files_metadata_list_get(dir_src) %>%
    data.table::setnames(paste0(names(.), "_src"))

  pa_tgt <- .fh_files_metadata_list_get(dir_tgt) %>%
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

#' Retrieve the List of Datasets for a Functional Area
#'
#' @description
#' Extracts the list of datasets under a specified functional area
#' from the configuration.
#'
#' @param .functional_area A character string representing the functional area
#'   (e.g., \code{"sales"}).
#' @param .config A list or environment containing the loaded configuration.
#'   Defaults to \code{.hl_config_get()}, an internal function providing
#'   the most recent configuration.
#'
#' @return
#' Returns a list of datasets.
#' If no datasets are found, the function raises an error.
#'
#' @examples
#' \dontrun{
#'   datasets_list <- .fh_datasets_list_get(.functional_area = "sales")
#' }
#'
#' @keywords internal
.fh_datasets_list_get <- function(
    .functional_area,
    .config = .hl_config_get()
) {

  datasets_list <- .config$datasets$functional_areas[[.functional_area]]$datasets

  if (is.null(datasets_list)) {
    stop(sprintf(
      "No datasets found under functional area '%s'.",
      .functional_area
    ))
  }

  return(datasets_list)
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


#' Construct a Comma-Separated String of File Paths for Specific Datasets
#'
#' @description
#' Builds a comma-separated string of file paths for a specified environment
#' and staging level (e.g., \code{"bronze"}, \code{"silver"}, \code{"gold"},
#' or \code{"platinum"}). You can optionally provide \code{.dataset_names}
#' to restrict the output to specific datasets only.
#'
#' @param .environment A character string specifying the environment. One of
#'   \code{"development"}, \code{"test"}, \code{"acceptance"}, or
#'   \code{"production"}. Defaults to all options, with partial matching allowed.
#' @param .staging A character string specifying the staging level (one of
#'   \code{"bronze"}, \code{"silver"}, \code{"gold"}, or \code{"platinum"}).
#'   Defaults to all, with partial matching allowed.
#' @param .functional_area A character string specifying the functional area
#'   (e.g., \code{"sales"}).
#' @param .dataset_names An optional character vector of dataset names to filter.
#'   If \code{NULL} (default), returns paths for all datasets.
#' @param .config A list or environment containing the loaded configuration.
#'   Defaults to \code{.hl_config_get()}, an internal function assumed to provide
#'   the configuration.
#'
#' @return
#' A **single string** containing the file paths from the specified datasets
#' in the chosen functional area. Each file path is:
#' \enumerate{
#'   \item Normalized via \code{\link[base]{normalizePath}} (with
#'         \code{mustWork = FALSE} and \code{winslash = \"/\"}).
#'   \item Surrounded by single quotes (e.g. \code{'/my/path/file.csv'}).
#'   \item Joined by \code{\", \"}.
#' }
#'
#' @details
#' This function first retrieves the datasets list from a functional area
#' using \code{\link{.fh_datasets_list_get}}, optionally filters by dataset names,
#' then iterates over each dataset to build the paths for the chosen staging level.
#' Finally, the paths are normalized, quoted, and collapsed into a single
#' comma-separated string.
#'
#' @examples
#' \dontrun{
#'   # Build a comma-separated string of file paths for 'sales' in 'production'
#'   # environment using 'bronze' staging, restricting to the 'rtp' dataset:
#'
#'   path_str <- .fh_dataset_paths_get(
#'     .environment      = "production",
#'     .staging          = "bronze",
#'     .functional_area  = "sales",
#'     .dataset_names    = "rtp"
#'   )
#'
#'   cat(path_str)
#'   # e.g. '/some/path/sales_rtp/DD_SALES_QTY_202[12345].csv'
#' }
#'
#' @keywords internal
.fh_dataset_paths_get <- function(
    .environment       = .fh_environment_value_get("name"),
    .staging           = .fh_staging_value_get(.key = "folder"),
    .functional_area,
    .dataset_names     = NULL,
    .config            = .hl_config_get()
) {

  # Retrieve datasets for the given functional area
  datasets_list <- .fh_datasets_list_get(
    .functional_area = .functional_area,
    .config          = .config
  )

  # Match environment & staging args with recognized values
  environment <- match.arg(.environment) %>% tolower()
  staging     <- match.arg(.staging)     %>% tolower()

  # get root directory for environment
  env_ <-
    .fh_environment_value_get(.key= "root", .envr = environment) %>%
    .fh_onedrive_abs()
  stg_ <- .fh_staging_value_get(.key = "folder", .envr = staging)


  # If .dataset_names is specified, filter the list to only those datasets
  if (!is.null(.dataset_names)) {

    # Extract actual names in the list
    all_names <- sapply(datasets_list, function(ds) ds$name)

    # Check if all requested dataset names exist
    missing <- setdiff(.dataset_names, all_names)
    if (length(missing) > 0) {
      stop("The following dataset names were not found: ",
           paste(missing, collapse = ", "))
    }

    # Filter the list to keep only requested names
    datasets_list <- Filter(
      function(ds) ds$name %in% .dataset_names,
      datasets_list
    )
  }

  # Build a character vector of file paths
  file_paths <- sapply(datasets_list, function(ds) {

    dir_ <- ds$directory
    stg  <- ds$staging[[staging]]
    if (is.null(stg)) {
      return(NA_character_)
    }
    pattern_ <- stg$pattern
    ext_     <- stg$extension

    # Construct file path: directory/pattern.extension
    file.path(env_, stg_, dir_, paste0(pattern_, ".", ext_))
  })

  # Remove any NA entries
  filtered_files <- file_paths[!is.na(file_paths)]

  # Normalize paths, quote each one, and collapse into a single comma-separated string
  #    e.g., "'/my/path/file1.csv', '/my/path/file2.csv'"
  result_str <- filtered_files %>%
    normalizePath(mustWork = FALSE, winslash = "/") %>%
    paste0("'", ., "'", collapse = ", ")

  return(result_str)
}


#' Retrieve a Specific Field from the Specified or All Environments
#'
#' @description
#' This internal helper function extracts a user-specified field (e.g.,
#' \code{"name"}, \code{"root"}) from environment entries in the configuration.
#' You can optionally provide an environment name (\code{.envr}) to retrieve
#' the field from only that environment.
#'
#' @param .key A character string specifying the name of the field to retrieve
#'   from each environment entry (e.g., \code{"name"} or \code{"root"}).
#' @param .envr An optional character string specifying a single environment name
#'   (e.g., \code{"production"}, \code{"development"}). If \code{NULL} (default),
#'   the function returns values for \emph{all} environments.
#' @param .config A list or environment containing the loaded configuration.
#'   Defaults to \code{.hl_config_get()}, an internal function assumed to provide
#'   the YAML or config object.
#'
#' @return
#' A character vector containing the requested field's value for the matching
#' environments. If \code{.envr} is \code{NULL}, this includes \emph{all}
#' environments. If no environments match \code{.envr}, the function returns
#' an empty character vector.
#'
#' @details
#' The function first accesses the top-level \code{environments} key in the
#' configuration, which should be a list of environment entries, each containing
#' at least one field named according to \code{.key}. It filters the list by
#' \code{.envr} (if specified), then uses \code{\link[base]{vapply}} to extract
#' that field from each matching environment, returning the results as a
#' character vector.
#'
#' If \code{environments} is missing or empty, the function raises an error.
#'
#' @examples
#' \dontrun{
#'   # Suppose your YAML has:
#'   # environments:
#'   #   - name: development
#'   #     root: 'OneDriveConsumer/.../development'
#'   #   - name: production
#'   #     root: 'OneDriveConsumer/.../production'
#'
#'   # retrieving environment names for all environments
#'   env_names <- .fh_environment_value_get(.key = "name")
#'   print(env_names)
#'   # e.g. c("development", "production")
#'
#'   # retrieving the root path for only the 'production' environment
#'   prod_root <- .fh_environment_value_get(.key = "root", .envr = "production")
#'   print(prod_root)
#'   # e.g. "OneDriveConsumer/.../production"
#' }
#'
#' @keywords internal
.fh_environment_value_get <- function(
    .key,
    .envr   = NULL,
    .config = .hl_config_get()
) {
  # Extract the list of environment entries from the config
  env_list <- .config$environments
  if (is.null(env_list) || length(env_list) == 0) {
    stop("No environments found under 'environments' in the configuration.")
  }

  # If .envr is specified, filter env_list for matching 'name'
  if (!is.null(.envr)) {
    env_list <- Filter(
      f = function(e) isTRUE(e[["name"]] == .envr),
      x = env_list
    )
    # Optionally raise an error if no matches found
    if (length(env_list) == 0) {
      warning(sprintf(
        "No environment entry found with name '%s'. Returning empty character vector.",
        .envr
      ))
      return(character(0))
    }
  }

  # Use vapply to extract the .key field (string) from each environment
  result_vector <- vapply(
    env_list,
    FUN = function(e) {
      if (is.null(e[[.key]])) {
        stop(sprintf(
          "Field '%s' not found in one of the environment entries.",
          .key
        ))
      }
      e[[.key]]
    },
    FUN.VALUE = character(1)
  )

  # Return the result as a character vector
  return(result_vector)
}


#' Retrieve a Specific Field from Specified or All Staging Levels
#'
#' @description
#' This internal helper function extracts a user-specified field (e.g.,
#' \code{"name"}, \code{"folder"}, \code{"description"}, \code{"compression"})
#' from each entry in \code{staging_levels}. You can optionally provide
#' \code{.envr} (the name of a single staging level) to retrieve the field
#' only from that level.
#'
#' @param .key A character string specifying the name of the field to retrieve
#'   from each staging level entry (e.g., \code{"name"} or \code{"folder"}).
#' @param .envr An optional character string specifying a single staging level
#'   name (e.g., \code{"bronze"}, \code{"silver"}). If \code{NULL} (default),
#'   the function returns values for \emph{all} staging levels.
#' @param .config A list or environment containing the loaded configuration.
#'   Defaults to \code{.hl_config_get()}, an internal function assumed to provide
#'   the YAML or config object.
#'
#' @return
#' A character vector containing the requested field's value for the matching
#' staging levels. If \code{.envr} is \code{NULL}, this includes \emph{all}
#' levels. If no staging level matches \code{.envr}, the function returns
#' an empty character vector.
#'
#' @details
#' The function first accesses \code{staging_levels} in the configuration, which
#' should be a list of entries, each containing at least one field named
#' according to \code{.key}. It filters the list by \code{.envr} (if specified),
#' then uses \code{\link[base]{sapply}} to extract that field from each matching
#' staging level, returning a character vector of results.
#'
#' \strong{Note:} This function assumes the field referenced by \code{.key} is
#' a single string. If \code{.key} corresponds to a field that is \emph{not}
#' a single string (e.g., \code{file_types} as a vector), you may need to adapt
#' the code to return a list or handle that field differently.
#'
#' If no staging levels exist, or if a requested field is missing, the function
#' raises an error. If a specified \code{.envr} is not found, it returns an empty
#' vector (with a warning).
#'
#' @examples
#' \dontrun{
#'   # Example YAML:
#'   # staging_levels:
#'   #   - name: bronze
#'   #     folder: bronze
#'   #   - name: silver
#'   #     folder: silver
#'
#'   # Get all staging level names:
#'   stg_names <- .fh_staging_value_get(.key = "name")
#'   print(stg_names)
#'   # c("bronze", "silver", "gold", "platinum", ...)
#'
#'   # Get only the folder for the 'silver' staging:
#'   silver_folder <- .fh_staging_value_get(.key = "folder", .envr = "silver")
#'   print(silver_folder)
#'   # e.g. "silver"
#' }
#'
#' @keywords internal
.fh_staging_value_get <- function(
    .key,
    .envr   = NULL,
    .config = .hl_config_get()
) {
  # Extract the list of staging levels from the config
  stg_list <- .config$staging_levels
  if (is.null(stg_list) || length(stg_list) == 0) {
    stop("No staging levels found under 'staging_levels' in the configuration.")
  }

  # If .envr is specified, filter by the 'name' field
  if (!is.null(.envr)) {
    stg_list <- Filter(
      f = function(x) isTRUE(x[["name"]] == .envr),
      x = stg_list
    )
    # Optionally raise a warning if no match found
    if (length(stg_list) == 0) {
      warning(sprintf(
        "No staging level found with name '%s'. Returning empty character vector.",
        .envr
      ))
      return(character(0))
    }
  }

  # Use sapply to extract the .key field from each staging level
  # This works best if the field is a single string.
  result_vector <- sapply(stg_list, function(entry) {
    val <- entry[[.key]]
    if (is.null(val)) {
      stop(sprintf(
        "Field '%s' not found in one of the staging level entries.",
        .key
      ))
    }
    if (!is.character(val) || length(val) != 1) {
      stop(sprintf(
        "Field '%s' in one of the staging levels is not a single string. Consider returning a list instead.",
        .key
      ))
    }
    val
  })

  return(result_vector)
}


#' Create datasets folder structure based on YAML
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
.fh_create_dataset_folders <- function(
    config
    ) {

  # Get root path for the specified environment:
  root_path <- config[["root"]]

  # Access functional areas
  functional_areas <- config[["functional_areas"]]
  if (is.null(functional_areas)) {
    if (verbose) message("No functional areas found in config.")
    return(invisible(TRUE))
  }

  # Loop over each functional area
  for (fa_name in names(functional_areas)) {
    area <- functional_areas[[fa_name]]

    # Loop over datasets
    datasets <- area[["datasets"]]
    if (is.null(datasets)) next

    for (ds_name in names(datasets)) {
      dataset <- datasets[[ds_name]]

      # Directory name (subfolder under staging)
      dir_name <- dataset[["directory"]]

      # Loop over staging levels
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

##' Retrieve a Field from One or All Datasets in a Functional Area
#'
#' @description
#' This internal helper function extracts a specific field (e.g., \code{"name"},
#' \code{"transformation"}, \code{"directory"}) from either:
#' \enumerate{
#'   \item A specified dataset by name, or
#'   \item All datasets in a given functional area (if \code{.dataset_name} is \code{NULL}).
#' }
#'
#' @param .functional_area A character string indicating the functional area
#'   (e.g., \code{"master_data"}, \code{"sales"}).
#' @param .dataset_name An optional character string specifying which dataset's
#'   field you want (e.g., \code{"material"}, \code{"mat_sales"}). If \code{NULL},
#'   the field is retrieved from all datasets in the functional area.
#' @param .field A character string indicating which field to retrieve
#'   (e.g., \code{"name"}, \code{"transformation"}, \code{"directory"}).
#' @param .config A list or environment containing the loaded configuration.
#'   Defaults to \code{.hl_config_get()}, an internal function assumed to
#'   provide the YAML or config object.
#'
#' @return
#' \enumerate{
#'   \item If \code{.dataset_name} is specified: a single value (character string)
#'     corresponding to that dataset's \code{.field}.
#'   \item If \code{.dataset_name} is \code{NULL}: a **named character vector**
#'     of all datasets' \code{.field} values, with names set to each dataset's
#'     \code{name}.
#' }
#'
#' @details
#' This function first calls \code{\link{.fh_datasets_list_get}} to retrieve
#' all datasets in the specified \code{.functional_area}. If \code{.dataset_name}
#' is provided, it filters down to that single dataset. It then extracts the
#' requested \code{.field} (e.g., \code{"directory"}) from each matched dataset.
#'
#' \strong{Behavior}:
#' \itemize{
#'   \item If no dataset matches \code{.dataset_name}, an error is raised.
#'   \item If the \code{.field} does not exist in a matched dataset, an error is raised.
#'   \item If multiple datasets match (in the case of \code{.dataset_name = NULL}),
#'     a named character vector is returned.
#' }
#'
#' @examples
#' \dontrun{
#'   # Suppose .fh_datasets_list_get("master_data") returns multiple datasets:
#'   #   name: material, transformation: DSCP_MATE, directory: master_data
#'   #   name: mat_sales, transformation: DSCP_MATS, directory: master_data
#'   #   etc...
#'
#'   # 1) Get 'directory' for ONLY the 'material' dataset
#'   one_dir <- .fh_dataset_field_get(
#'     .functional_area = "master_data",
#'     .dataset_name    = "material",
#'     .field           = "directory"
#'   )
#'   print(one_dir)  # 'master_data'
#'
#'   # 2) Get 'transformation' for ALL datasets in 'master_data'
#'   all_trans <- .fh_dataset_field_get(
#'     .functional_area = "master_data",
#'     .field           = "transformation"
#'   )
#'   print(all_trans)
#'   # named vector, e.g. c(material = "DSCP_MATE", mat_sales = "DSCP_MATS", ...)
#' }
#'
#' @keywords internal
.fh_dataset_field_get <- function(
    .functional_area,
    .dataset_name = NULL,
    .field,
    .config = .hl_config_get()
) {

  # 1. Retrieve all datasets for the functional area
  datasets_list <- .fh_datasets_list_get(
    .functional_area = .functional_area,
    .config          = .config
  )

  # 2. If a specific dataset name is provided, filter for it
  if (!is.null(.dataset_name)) {
    ds_filtered <- Filter(
      f = function(d) identical(d[["name"]], .dataset_name),
      x = datasets_list
    )
    if (length(ds_filtered) == 0) {
      stop(sprintf(
        "No dataset found with name '%s' in functional area '%s'.",
        .dataset_name, .functional_area
      ))
    }
    # We'll assume dataset names are unique, so ds_filtered has exactly 1 element
    ds <- ds_filtered[[1]]

    val <- ds[[.field]]
    if (is.null(val)) {
      stop(sprintf(
        "Field '%s' not found in dataset '%s' under functional area '%s'.",
        .field, .dataset_name, .functional_area
      ))
    }
    return(val)
  }

  # 3. If no dataset_name is given, return the field for all datasets
  #    We'll build a named vector, named by the dataset's 'name' attribute.
  result_values <- sapply(datasets_list, function(ds) {
    val <- ds[[.field]]
    if (is.null(val)) {
      stop(sprintf(
        "Field '%s' is missing in dataset '%s' under functional area '%s'.",
        .field, ds[["name"]], .functional_area
      ))
    }
    val
  })

  # 4. Name the vector by the dataset 'name'
  ds_names <- sapply(datasets_list, `[[`, "name")
  names(result_values) <- ds_names

  return(result_values)
}
