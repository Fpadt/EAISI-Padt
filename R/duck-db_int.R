#' Get (or create) a DuckDB connection
#'
#' Internal function that returns a `DBIConnection` to DuckDB. If no connection
#' exists in the `.padt_env`, a new one is created using \code{\link[DBI]{dbConnect}}.
#'
#' @param dbdir Character. Location of the DuckDB database file. Use \code{":memory:"}
#'   for an in-memory database.
#'
#' @return A `DBIConnection` object to DuckDB.
#'
#' @details
#' This internal helper function checks if there is an existing DuckDB connection in
#' the environment `.padt_env$conn`. If none is found, it initializes a new one and
#' caches it. Subsequent calls will reuse the same connection.
#'
#' @keywords internal
.dd_duckdb_open_conn <- function(dbdir = ":memory:") {
  if (!exists("conn", envir = .padt_env)) {
    message("Initializing DuckDB connection...")
    .padt_env$conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir)
  }
  .padt_env$conn
}

#' Close and Remove the DuckDB Connection
#'
#' This internal helper function checks whether a DuckDB connection (stored in
#' \code{.padt_env$conn}) exists. If found, it closes (disconnects) the
#' DuckDB connection using \code{\link[DBI]{dbDisconnect}} and removes the
#' \code{conn} object from \code{.padt_env}. If no connection is present,
#' a message is displayed indicating that nothing is closed.
#'
#' @details
#' This function is called to cleanly release resources associated with a
#' DuckDB connection in the internal environment. It can be invoked at the end
#' of your workflow or whenever you wish to reset the internal DuckDB state.
#'
#' @return
#' This function is called for its side effects (closing and removing the
#' connection). It returns \code{NULL} invisibly.
#'
#' @keywords internal
.dd_duckdb_close_conn <- function() {
  if (exists("conn", envir = .padt_env)) {
    message("Closing DuckDB connection...")
    DBI::dbDisconnect(.padt_env$conn, shutdown = TRUE)
    rm("conn", envir = .padt_env)
  } else {
    message("No DuckDB connection found to close.")
  }
}




#' Generate SQL for Scope Materials CTE
#'
#' Constructs a Common Table Expression (CTE) SQL string for scope materials.
#' This CTE selects distinct materials from a parquet file filtered by a condition
#' on the \code{PRDH1} column.
#'
#' @param .scope_prdh Character vector. The values to filter the \code{PRDH1} column.
#'   Defaults to the global constant \code{SCOPE_PRDH}.
#' @param .con A database connection object. Used to safely parameterize SQL queries
#'   via \code{glue::glue_sql}.
#'
#' @return A character string containing the SQL statement for the scope materials CTE.
#'
#' @details
#' This function dynamically constructs a SQL query using \code{glue::glue_sql},
#' ensuring that inputs are safely parameterized. The parquet file path for material
#' data is determined internally via \code{.fh_datasets_path_get}.
#'
#' The returned SQL defines a Common Table Expression (CTE) named \code{SCOPE_MATL},
#' which filters materials from a parquet file based on the specified \code{PRDH1} values.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' sql <- .da_matl_scope_get(
#'   .scope_prdh = c("PRDH001", "PRDH002"),
#'   .con = my_db_connection
#' )
#' print(sql)
#' }
#'
#' @import glue
#' @importFrom DBI SQL
#' @keywords internal
.da_matl_scope_get <- function(
    .scope_prdh = SCOPE_PRDH,
    .con
) {

  # Validate input parameters
  if (missing(.con)) {
    stop("DuckDB connection .con must be provided.")
  }

  # get file pattern to get master data
  FN_MATL <- .fh_dataset_paths_get(
    .environment     = .hl_config_get()$project$active_environment,
    .staging         = "silver",
    .functional_area = "master_data",
    .dataset_names   = "material"
  )

  # Construct the SQL string using glue_sql
  sql_query <- glue::glue_sql("
    WITH SCOPE_MATL AS (
      SELECT DISTINCT MATERIAL
      FROM read_parquet([{DBI::SQL(FN_MATL)}])
      WHERE PRDH1 IN ({.scope_prdh*})
    )
  ", .scope_prdh = .scope_prdh, .con = .con)

  return(sql_query)
}
