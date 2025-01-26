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
pa_parquet_describe <-
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

