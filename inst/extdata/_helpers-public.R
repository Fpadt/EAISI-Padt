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
#' fVerbose("example_function", list(arg1 = 10, arg2 = "test"), "result", TRUE)
#'
#' @export
fVerbose <- function(
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
fDescribe_Parquet <-
  function(
    .fn = NULL
  ){

    # Establish a connection to DuckDB
    con <- .get_duckdb_conn()

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
#' @examples
#' fOpen_Folder("C:/Users/YourUserName/Documents")
#'
#' @export
fOpen_Folder <-
  function(path){
    # Open the folder in Windows Explorer
    shell.exec(normalizePath(path))
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
#' @examples
#' dt <- fHeader_PQT("path/to/your/file.parquet", .n = 10)
#' print(dt)
#'
#' @export
fHeader_PQT <-
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

