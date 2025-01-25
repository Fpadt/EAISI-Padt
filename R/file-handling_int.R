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

