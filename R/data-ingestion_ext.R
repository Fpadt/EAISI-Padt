
#' Retrieve All Pipeline Definitions
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
#'   dt_pipe <- pa_get_pipelines()
#'   head(dt_pipe)
#' }
#'
#' @export
pa_get_pipelines <- function() {

  rbind(
    fread(file = file.path(
      pa_getwd(), "config", "B4_PIPELINE_ORG.csv")) %>%
      .[, `:=`(SRC = "O", WHERE_CLAUSE = "")],
    fread(file = file.path(
      pa_getwd(), "config", "B4_PIPELINE_MOD.csv")) %>%
      .[, `:=`(SRC = "C")]
  ) %T>%
    setorder(SRC, OHDEST, POSIT) %>%
    .[, .SD[1], by = .(OHDEST, POSIT)]
}


#' Transform CSV Files to Parquet
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
#'   \item Invokes an internal function \code{.transform_csv_to_parquet} (called via \code{purrr::walk})
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
#' \code{\link{.get_pipe_line}}, \code{\link{.transform_csv_to_parquet}}
#'
#' @export
pa_transform_csv_to_parquet <- function(
    source_path,
    output_path,
    file_pattern,
    file_spec,
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
  PIPE_LINE <- .get_pipe_line(ohdest = ohdest)

  #--- Check if the pipeline for the source exists -----------------------------
  if (nrow(PIPE_LINE) == 0) {
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
    .f          = .transform_csv_to_parquet,
    output_path = output_path,
    file_spec   = file_spec,
    pipe_line   = PIPE_LINE,
    verbose     = verbose
  )

  invisible(NULL)
}

