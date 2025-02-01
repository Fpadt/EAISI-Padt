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

