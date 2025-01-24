#' Left Pad Material Numbers
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
#' pa_MATN1_I(c("123", "456789", "A123"))
#' # Returns: c("000000000000000123", "000000000000456789", "A123")
#'
#' @export
pa_MATN1_I <- function(x) {
  # like CONVERSION_EXIT_MATN1_INPUT
  # only leftpad leading zero's in case it is a number
  .LP0(x, 18)
}

#' Remove Leading Zeros
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
#' pa_MATN1_O("00123")     # Returns "123"
#' pa_MATN1_O("00000A123") # Returns "00000A123"
#' pa_MATN1_O(c("00123", "00000A123", "A000045")) # Returns c("123", "abc123", "A000045")
#'
#' @export
pa_MATN1_O <-
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

#' Preview the Header of a Parquet File
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
pa_Head_PQT <-
  function(.fn, .n = 1000){

    con <- .get_duckdb_conn()

    on.exit( .close_duckdb_conn())

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

