#' left pad material numbers
#'
#' Pads material numbers with leading zeros to match the standard
#' 18-character SAP material number format.
#'
#' @param x A character vector of material numbers.
#' Only numeric-like strings will be padded.
#'
#' @return A character vector where numeric strings are left-padded with
#' zeros to a width of 18. Non-numeric strings remain unchanged.
#'
#' @details
#' This function mimics the behavior of SAP's \code{CONVERSION_EXIT_MATN1_INPUT},
#' which ensures material numbers are standardized to 18 characters by
#' adding leading zeros.
#'
#' @examples
#' pa_matn1_input(c("123", "456789", "A123"))
#' # Returns: c("000000000000000123", "000000000000456789", "A123")
#'
#' @export
pa_matn1_input <- function(x) {
  # like CONVERSION_EXIT_MATN1_INPUT
  # only leftpad leading zero's in case it is a number
  .LP0(x, 18)
}

#' remove leading zeros
#'
#' This function removes leading zeros from a string, but only if the string is purely numeric.
#' If the string contains non-numeric characters, it will be returned unchanged.
#'
#' @param x A character vector. Each element will be checked to see if it is purely numeric.
#'
#' @return A character vector with leading zeros removed for numeric strings.
#'         Non-numeric strings will be returned unchanged.
#'
#' @details
#' This function mimics the behavior of SAP's \code{CONVERSION_EXIT_MATN1_OUTPUT}
#' which removes leading zeros from material numbers if these only contain numbers.
#'
#' @examples
#' pa_matn1_output("00123")     # Returns "123"
#' pa_matn1_output("00000A123") # Returns "00000A123"
#' pa_matn1_output(c("00123", "00000A123", "A000045")) # Returns c("123", "abc123", "A000045")
#'
#' @export
pa_matn1_output <-
  function(x){
    # like CONVERSION_EXIT_MATN1_OUTPUT
    # only remove leading zero's in case it is a number
    is_num <- grepl("^[0-9]+$", x)
    ifelse(
      is_num,
      sub("^0*", "", x, perl = TRUE),
      x
    )
  }

#' preview the header of a parquet file
#'
#' Retrieves the first \code{n} rows of a Parquet file as a \code{data.table}.
#'
#' @param .fn Character. The file path to the Parquet file.
#' @param .n Numeric. The number of rows to retrieve. Defaults to 1000.
#'
#' @return A \code{data.table} containing the first \code{n} rows of the Parquet file.
#'
#' @details
#' This function connects to DuckDB to query the specified Parquet file and fetch the first \code{n} rows.
#'
#' @export
pa_parquet_head <-
  function(.fn, .n = 1000){

    con <- .duckdb_open_conn()

    on.exit( .duckdb_close_conn())

    query <- glue_sql("
    SELECT
      *
    FROM
      read_parquet([{`.fn`}])
    LIMIT {.n}
   ", .con = con)

    dt <-
      dbGetQuery(con, query) %>%
      setDT()

    return(dt)

  }

#' Describe the Structure of a Parquet File
#'
#' Retrieves metadata information about a Parquet file by describing its structure using DuckDB.
#'
#' @param .fn Character. The file path to the Parquet file.
#'
#' @return Prints the metadata information of the Parquet file to the console.
#'
#' @details
#' This function uses DuckDB to describe the structure of a Parquet file. The results are printed to the console.
#'
#'
#' @export
.pa_parquet_describe <-
  function(
    .fn = NULL
  ){

    # Establish a connection to DuckDB
    con <- .duckdb_open_conn()

    # construct Query
    query <-
      glue_sql("
        DESCRIBE
        SELECT
          *
        FROM
          read_parquet({`.fn`})
      ", .con = con)

    # Execute and fetch results
    dbGetQuery(con, query) %>%
      print()

  }

#' Open a Folder in the File Explorer
#'
#' Opens a specified folder in the system's default file explorer (e.g., Windows Explorer on Windows).
#'
#' @param path Character. The path to the folder to open.
#'
#' @return This function does not return anything. It opens the folder in the system's file explorer.
#'
#' @export
pa_folder_open <-
  function(path){
    # Open the folder in Windows Explorer
    shell.exec(normalizePath(path))
  }

