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

#' Generate Variables for Colors in pa_ET_COLS
#'
#' Dynamically creates variables named "col_<name>" in the specified environment.
#'
#' @param colors A named vector or list of colors.
#' @param env The environment where the variables should be created. Defaults to the global environment.
#' @return NULL. The function creates variables dynamically.
#' @export
generate_color_variables <- function(colors, env = globalenv()) {
  if (!is.vector(colors) && !is.list(colors)) {
    stop("The 'colors' argument must be a named vector or list.")
  }

  for (name in names(colors)) {
    # Construct the variable name
    var_name <- paste0("col_", name)
    # Assign the color value to the variable in the specified environment
    assign(var_name, colors[[name]], envir = env)
  }

  message("Color variables generated: ", paste(paste0("col_", names(colors)), collapse = ", "))
}


