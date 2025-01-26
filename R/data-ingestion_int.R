# Description: This script contains the functions to transform the data from Bronze to Silver
# DuckDB ####

#' A Generic Function for Generating Formatted Field Strings
#'
#' @description
#' This internal function filters rows in a \code{data.table}, calculates spacing
#' for alignment, applies a \code{glue} template to generate field strings, and
#' finally collapses them into a single character string.
#'
#' @details
#' \enumerate{
#'   \item Filters out rows where \code{filter_col} is empty (\code{""}).
#'   \item Calculates spacing based on the maximum length of \code{alignment_col}.
#'   \item Applies \code{glue_data} to each row using \code{glue_template}.
#'   \item Collapses the results into a single newline-delimited string, removing
#'         the trailing character (often a comma).
#' }
#'
#' @param transformations A \code{data.table} (or similar) containing the fields for
#'   transformations. Must include at least the columns referenced by
#'   \code{filter_col} and \code{alignment_col}.
#' @param filter_col A string specifying the column used to filter out empty rows.
#' @param alignment_col A string specifying the column used to calculate spacing
#'   for alignment.
#' @param glue_template A string template passed to \code{glue_data}, referencing
#'   columns in \code{transformations}. For example, \code{"'{FLDNM_IN}' : '{FIELDTP}',"}.
#'
#' @return A single character string with the formatted field definitions.
#'
#' @keywords internal
.di_fields_generic_get <- function(
    transformations,
    filter_col,
    alignment_col,
    glue_template
) {
  transformations %>%
    # 1. Filter out empty rows based on filter_col
    .[get(filter_col) != ""] %>%

    # # 2. Calculate spacing for alignment
    .[, `:=`(no_spc = max(nchar(get(alignment_col))) - nchar(get(alignment_col)) + 3)] %>%
    # # 2. Calculate spacing for alignment
    # .[, .no_spc := max(nchar(get(alignment_col))) - nchar(get(alignment_col)) + 3] %>%

    # 3. Generate the formatted strings via glue_data
    .[, glue_data(.SD, glue_template)] %>%

    # 4. Collapse into a single string with newlines
    glue_collapse(sep = "\n") %>%

    # 5. Remove the trailing character (comma or otherwise)
    {\(txt) substr(txt, 1, nchar(txt) - 1)}()
}


#' Generate "Fields In" Using a Generic Template
#'
#' @description
#' Internal wrapper function around \code{.di_fields_generic_get} specifically for
#' fields that reference \code{FLDNM_IN} and \code{FIELDTP}.
#'
#' @param transformations A \code{data.table} (or similar) with columns
#'   \code{FLDNM_IN} and \code{FIELDTP} at least.
#'
#' @return A single character string with the formatted \code{FLDNM_IN} fields.
#'
#' @keywords internal
.di_fields_in_get <- function(transformations) {
  .di_fields_generic_get(
    transformations     = transformations,
    filter_col    = "FLDNM_IN",
    alignment_col = "FLDNM_IN",
    glue_template = "'{FLDNM_IN}'{strrep(' ', no_spc)}: '{FIELDTP}',"
  )
}


#' Generate "Fields Out" Using a Generic Template
#'
#' @description
#' Internal wrapper function around \code{.di_fields_generic_get} specifically for
#' fields that reference \code{FLDNM_OUT} and \code{TRNSFRM}.
#'
#' @param transformations A \code{data.table} (or similar) with columns
#'   \code{FLDNM_OUT} and \code{TRNSFRM} at least.
#'
#' @return A single character string with the formatted \code{FLDNM_OUT} fields.
#'
#' @keywords internal
.di_fields_out_get <- function(transformations) {
  .di_fields_generic_get(
    transformations     = transformations,
    filter_col    = "FLDNM_OUT",
    alignment_col = "FLDNM_OUT",
    glue_template = "{TRNSFRM}{strrep(' ', no_spc)}AS {FLDNM_OUT},"
  )
}

#' Generate a WHERE Clause Using the Generic Fields Function
#'
#' @description
#' A wrapper around \code{.di_fields_generic_get} to produce the same output as the
#' original \code{.di_where_clause_get()} function. Specifically, it filters out any
#' empty \code{WHERE_CLAUSE} rows, calculates alignment based on \code{WHERE_CLAUSE},
#' and applies a glue template of the form:
#'
#' \code{"\{FLDNM_OUT\}\{strrep(' ', no_spc)\} \{WHERE_CLAUSE\},"}
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Filters rows where \code{WHERE_CLAUSE} is non-empty.
#'   \item Aligns based on the length of \code{WHERE_CLAUSE}.
#'   \item Inserts the columns \code{FLDNM_OUT} and \code{WHERE_CLAUSE} into a glue
#'         template that ends with a comma.
#'   \item Collapses them into one string with newlines, then strips the final comma.
#' }
#'
#' @param transformations A \code{data.table} (or similar) that includes at least
#'   \code{FLDNM_OUT} and \code{WHERE_CLAUSE}.
#'
#' @return A single character string identical to what the original
#'   \code{.di_where_clause_get()} produced.
#'
#' @keywords internal
.di_where_clause_get <- function(transformations) {


  if(nrow(transformations[get("WHERE_CLAUSE") != ""]) == 0){
    return("TRUE")
  } else {
    .di_fields_generic_get(
      transformations     = transformations,
      filter_col    = "WHERE_CLAUSE",   # Filter out empty WHERE_CLAUSE rows
      alignment_col = "WHERE_CLAUSE",   # Align based on the length of WHERE_CLAUSE
      glue_template = "{FLDNM_OUT}{strrep(' ', no_spc)} {WHERE_CLAUSE},"
    )
  }
}


#' @title Transform a Single CSV File to Parquet (Internal)
#'
#' @description
#' Internal function that reads a single CSV file, applies transformations,
#' and writes the result to a Parquet file.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Ensures the file is indeed a CSV.
#'   \item Reads the CSV using \code{duckdb}, applies transformations.
#'   \item Writes the results to a Parquet file.
#' }
#'
#' @param full_file_name A character string with the full path to the CSV file.
#' @param output_path    A character string with the directory path where the
#'   Parquet file should be written.
#' @param file_spec      A list-like object with the CSV specifications (e.g.
#'   delim, header, date format).
#' @param transformations      A \code{data.table} (or similar) detailing how
#'   fields should be transformed.
#' @param verbose        A logical indicating whether to print additional
#'   information (\code{TRUE}) or not (\code{FALSE}).
#'
#' @return
#' Returns \code{NULL} invisibly. The side effect is creation of a Parquet
#' file in \code{output_path}.
#'
#' @keywords internal
.di_transform_csv_to_parquet <- function(
    full_file_name,
    output_path,
    file_spec,
    transformations,
    verbose
) {
    file_name       <- fs::path_file(full_file_name)
    input_csv_file  <- full_file_name
    output_pqt_file <- file.path(
      output_path,
      fs::path_ext_set(file_name, "parquet")
    )

    # Check if the file extension is "csv"
    if (tolower(fs::path_ext(input_csv_file)) != "csv") {
      stop(
        cat(white$bgRed$bold(
          glue("Error: The file is not a CSV file!: {input_csv_file}")
        ))
      )
    }

    # Establish a connection to DuckDB
    con <- .duckdb_open_conn()

    # Ensure the connection is closed when the function exits
    on.exit(.duckdb_close_conn(), add = TRUE)

    # Generate duckdb SQL to transform_csv_to_parquet
    sql_transform_csv_to_parquet <- glue("
      read_csv('{input_csv_file}',
        delim      = '{file_spec$DELIM}',
        header     =  {file_spec$HEADER},
        dateformat = '{file_spec$DATE_FORMAT}',
        columns = {{
          {.di_fields_in_get(transformations)}
        }}
      )
    ", .con = con ) %>% glue_sql("
      SELECT
        {DBI::SQL(.di_fields_out_get(transformations))}
      FROM
        {DBI::SQL(sql_read_csv)}
      WHERE
        {DBI::SQL(where_clause)} AND
        TRUE
      GROUP BY
        ALL
      ORDER BY
        ALL
      ",
        sql_read_csv = .,
        where_clause = .di_where_clause_get(transformations),
        .con = con ) %>% glue("
      COPY ({sql_transformation_rules})
       TO '{output_pqt_file}'
       (FORMAT 'parquet', CODEC 'uncompressed')
      ", sql_transformation_rules = ., .con = con
    )

    # Execute the SQL to transform_csv_to_parquet
    tryCatch({
      system.time({
        dbExecute(con, sql_transform_csv_to_parquet)
        if (verbose == TRUE){
          print(
            dbGetQuery(con, glue("DESCRIBE SELECT * FROM '{output_pqt_file}';"))
          )
        }
        message(
          cat(black$bgGreen$bold(glue("Data successfully written to {output_pqt_file}")))
        )
      })
    }, error = function(e) {
      message(
        cat(white$bgRed$bold(glue("An error occurred: {e$message}")))
      )
    })
  }

#' @title Internal Helper: Retrieve a Pipeline by ohdest
#'
#' @description
#' Internal helper function that queries all available pipelines (via \code{pa_transformations_get()})
#' and returns only those relevant for a given \code{ohdest}. The returned data.table is then sorted.
#'
#' @param ohdest A character string indicating which pipeline to filter on.
#'
#' @return
#' Returns a \code{data.table} with pipeline steps (sorted by \code{OHDEST}, \code{POSIT}) for the specified \code{ohdest}.
#'
#' @details
#' This is an internal function (\emph{not exported}) and is intended to be used by other functions
#' within this package. It relies on \code{pa_transformations_get()} to load the master pipeline data first.
#'
#' @keywords internal
.di_transformation_rules_get <- function(ohdest) {
  pa_transformations_get() %>%
    .[OHDEST == ohdest] %T>%
    setorder(OHDEST, POSIT)
}

#' Get File Specification from YAML Configuration (Internal)
#'
#' Internal function to retrieve the file specification from the YAML configuration file
#' stored in the specified project directory.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{DELIM}{Character. The delimiter used in the files (e.g., `";"`).}
#'   \item{HEADER}{Logical. Whether the files have a header (`TRUE` or `FALSE`).}
#'   \item{DATE_FORMAT}{Character. The date format used in the files (e.g., `\"\%Y-\%m-\%d\"`).}
#' }
#' @keywords internal
.di_csv_file_spec_get <- function() {

  # Retrieve file specification from the YAML file
  FILE_SPEC <- list(
    DELIM       = pa_config_get_value(.key = DELIM),      # Delimiter
    HEADER      = pa_config_get_value(.key = HEADER),     # Header
    DATE_FORMAT = pa_config_get_value(.key = DATE_FORMAT) # Date format
  )

  # Validate the retrieved values
  if (is.null(FILE_SPEC$DELIM) || FILE_SPEC$DELIM == "") {
    stop("The `DELIM` key is missing or empty in the configuration file.")
  }
  if (is.null(FILE_SPEC$HEADER)) {
    stop("The `HEADER` key is missing in the configuration file.")
  }
  if (is.null(FILE_SPEC$DATE_FORMAT) || FILE_SPEC$DATE_FORMAT == "") {
    stop("The `DATE_FORMAT` key is missing or empty in the configuration file.")
  }

  return(FILE_SPEC)
}
