
#' get all pipeline definitions
#'
#' @description
#' Reads two CSV files (\emph{B4_PIPELINE_ORG.csv} and \emph{B4_PIPELINE_MOD.csv}) from a predefined
#' directory structure, merges them, and sorts the resulting data.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Reads original pipeline definitions from \emph{B4_PIPELINE_ORG.csv}.
#'   \item Reads modified pipeline definitions from \emph{B4_PIPELINE_MOD.csv}.
#'   \item Binds them into a single \code{data.table}.
#'   \item Adds columns like \code{SRC} and \code{WHERE_CLAUSE} as needed.
#'   \item Sorts the final table by \code{SRC}, \code{OHDEST}, and \code{POSIT}.
#'   \item Returns the first row per (\code{OHDEST}, \code{POSIT}) group.
#' }
#'
#' @return
#' A \code{data.table} containing the merged pipeline definitions, with one row per
#' combination of (\code{OHDEST}, \code{POSIT}).
#'
#' @examples
#' \dontrun{
#'   # Fetch pipeline data
#'   dt_pipe <- pa_transformations_get()
#'   head(dt_pipe)
#' }
#'
#' @export
pa_transformations_get <- function() {

  rbind(
    fread(file = file.path(
      ".", CONFIG_FLDR, fn_TRFN_ORG)) %>%
      .[, `:=`(SRC = "O", WHERE_CLAUSE = "")],
    fread(file = file.path(
      ".", CONFIG_FLDR, fn_TRFN_MOD)) %>%
      .[, `:=`(SRC = "C")]
  ) %T>%
    setorder(SRC, OHDEST, POSIT) %>%
    .[, .SD[1], by = .(OHDEST, POSIT)]
}


#' transform CSV files to parquet
#'
#' @description
#' This function transforms one or more CSV files into Parquet format using a predefined
#' data transformation pipeline (the “pipeline for ohdest”).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Checks whether both \code{source_path} and \code{output_path} exist.
#'   \item Lists files matching \code{file_pattern} in \code{source_path}.
#'   \item Retrieves the pipeline definition for the specified \code{ohdest}.
#'   \item Invokes an internal function \code{.di_csv_to_parquet_transform} (called via \code{purrr::walk})
#'         on each file to do the actual transformation.
#' }
#'
#' @param source_path  A character string specifying the path to the source directory containing CSV files.
#' @param output_path  A character string specifying the path to the output directory where Parquet files will be saved.
#' @param file_pattern A character string with a regex pattern to filter which source CSV files to include.
#' @param file_spec    A list or object defining file specifications (e.g., delimiter, headers).
#' @param ohdest       A character string indicating which “ohdest” pipeline configuration to retrieve and use.
#' @param verbose      A logical indicating whether to print verbose messages (\code{TRUE}) or not (\code{FALSE}).
#'
#' @return
#' Returns \code{NULL} invisibly. The side-effect is that new Parquet files are written to \code{output_path}.
#'
#' @seealso
#' \code{\link{.di_transformation_rules_get}}, \code{\link{.di_csv_to_parquet_transform}}
#'
#' @export
pa_transform <- function(
    source_path,
    output_path,
    file_pattern,
    ohdest,
    verbose
) {
  #--- Check if the source path exists -----------------------------------------
  if (!dir.exists(source_path)) {
    stop(
      cat(
        crayon::white$bgRed$bold(
          glue::glue("Error: The source_path does not exist!: {source_path}")
        )
      )
    )
  }

  #--- Check if the output path exists -----------------------------------------
  if (!dir.exists(output_path)) {
    stop(
      cat(
        crayon::white$bgRed$bold(
          glue::glue("Error: The output_path does not exist!: {output_path}")
        )
      )
    )
  }

  #--- List all relevant files in source_path ----------------------------------
  fls <- list.files(
    path       = source_path,
    pattern    = file_pattern,
    full.names = TRUE
  )

  #--- Check if at least one source file exists --------------------------------
  if (length(fls) == 0) {
    stop(
      cat(
        crayon::white$bgRed$bold(
          glue::glue("Error: No source files!: {source_path} {file_pattern}")
        )
      )
    )
  }

  #--- Get Transformation Pipeline ---------------------------------------------
  TRFN_RULES <- .di_transformation_rules_get(ohdest = ohdest)

  #--- Check if the pipeline for the source exists -----------------------------
  if (nrow(TRFN_RULES) == 0) {
    stop(
      cat(
        crayon::white$bgRed$bold(
          glue::glue("Error: The pipeline does not exist!: {ohdest}")
        )
      )
    )
  }

  #--- Run the main function to transform data from CSV to Parquet -------------
  purrr::walk(
    .x          = fls,
    .f          = .di_csv_to_parquet_transform,
    output_path = output_path,
    transformations   = TRFN_RULES,
    verbose     = verbose
  )

  invisible(NULL)
}

#' Prompt User for Transformation Decision with Options
#'
#' Asks the user how to handle the files found (transform all, transform new, transform modified, or do nothing).
#'
#' @param pa_SYNC A list with two elements:
#'   \describe{
#'     \item{NEW}{Character vector of paths for new files.}
#'     \item{MOD}{Character vector of paths for modified files.}
#'   }
#' @return Character. One of the options: ".all", ".new", ".mod", or ".none".
#' @export
pa_ask_user_transform_files <- function(pa_SYNC) {

  # Calculate file counts
  .new <- length(pa_SYNC[["NEW"]])
  .mod <- length(pa_SYNC[["MOD"]])
  .all <- .new + .mod

  # Define options
  options <- c(
    paste0("Transform All (", .all, " files)"),
    paste0("Transform New (", .new, " files)"),
    paste0("Transform Modified (", .mod, " files)"),
    "Do Nothing"
  )

  # Display the question and options
  cat("How would you like to handle the files?\n")
  for (i in seq_along(options)) {
    cat(i, ":", options[i], "\n")
  }

  # Get user input and validate it
  repeat {
    choice <- readline(prompt = "Please choose 1, 2, 3, or 4: ")

    # Validate the input
    if (choice %in% c("1", "2", "3", "4")) {
      # Map user choice to return value
      return(switch(
        choice,
        ".all", # Option 1
        ".new", # Option 2
        ".mod", # Option 3
        ".none" # Option 4
      ))
    } else {
      cat("Invalid input. Please choose 1, 2, 3, or 4.\n")
    }
  }
}
