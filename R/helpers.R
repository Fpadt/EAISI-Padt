# SCOPE_PLNT <-
#   "WITH SCOPE_PLNT AS (
#      SELECT DISTINCT
#        PLANT
#      FROM
#        read_parquet([{`FN_FRPR1`}, {`FN_FRPR3`}])
#      WHERE
#        SALESORG IN ({SCOPE_SORG*})
#      )
#   "

#' Left Pad with Zeros
#'
#' Internal helper function to left-pad numeric strings with leading zeros to a specified width.
#'
#' @param x A character vector. Only numeric-like strings will be padded.
#' @param width An integer specifying the total width of the output strings. Non-numeric strings remain unchanged.
#'
#' @return A character vector where numeric strings are left-padded with zeros to the specified width.
#'
#' @keywords internal
.LP0 <- function(x, width) {
  # Only add leading zeros if it is a number
  is_num <- grepl("^[0-9]+$", x)
  ifelse(
    is_num,
    stringr::str_pad(string = x, width = width, side = "left", pad = "0"),
    x
  )
}

#' Retrieve DuckDB Config Parts
#'
#' This internal helper function returns a list of three elements:
#' \enumerate{
#'   \item \code{duckdb_con} - The DuckDB connection.
#'   \item \code{cte_scope_materials} - The CTE (common table expression) snippet
#'         for scope materials, constructed if needed.
#'   \item \code{where_clause} - A character vector of one or more WHERE clauses,
#'         built according to the provided parameters.
#' }
#'
#' @param .material A character vector of materials to filter on. If \code{NULL},
#'   no material-based filter is applied (beyond scope constraints).
#' @param .salesorg A character vector of sales organizations to filter on. If \code{NULL},
#'   no salesorg-based filter is applied (beyond scope constraints).
#' @param .scope_matl Logical. If \code{TRUE}, the returned \code{cte_scope_materials}
#'   (and any relevant WHERE clauses) will restrict to materials in the scope definition.
#' @param .scope_sorg Logical. If \code{TRUE}, where clauses will restrict to
#'   the provided sales organization scope (if \code{.salesorg} is not \code{NULL}).
#' @param .cm_min Character. Minimum YYYYMM to apply in date-based filtering. Defaults to
#'   \code{NULL}.
#' @param .cm_max Character. Maximum YYYYMM to apply in date-based filtering. Defaults to
#'   \code{NULL}.
#' @param .step_min Numeric. Minimum step filter. Defaults to \code{NULL}.
#' @param .step_max Numeric. Maximum step filter. Defaults to \code{NULL}.
#' @param .lagg_min Numeric. Minimum lag filter. Defaults to \code{NULL}.
#' @param .lagg_max Numeric. Maximum lag filter. Defaults to \code{NULL}.
#'
#' @return A named \code{list} with three elements:
#'   \itemize{
#'     \item \code{duckdb_con} - The active DuckDB connection.
#'     \item \code{cte_scope_materials} - SQL snippet for scope materials (may be an
#'            empty string if \code{.scope_matl=FALSE}).
#'     \item \code{where_clause} - A character vector of WHERE clauses.
#'   }
#'
#' @details
#' This function centralizes the logic of creating a DuckDB connection,
#' constructing the \code{cte_scope_materials} snippet, and building the \code{where_clause}.
#' Other functions can call this to avoid repeating code.
#'
#' @keywords internal
.get_duckdb_parts <- function(
    .vtype       = NULL,
    .ftype       = NULL,
    .material    = NULL,
    .salesorg    = NULL,
    .scope_matl  = NULL,
    .scope_sorg  = FALSE,
    .cm_min      = NULL,
    .cm_max      = NULL,
    .step_min    = NULL,
    .step_max    = NULL,
    .lagg_min    = NULL,
    .lagg_max    = NULL
) {

  # -- 1) Get or create a DuckDB connection --
  con <- .get_duckdb_conn()

  # -- 2) Build the CTE snippet for scope materials --
  cte_scope_materials <- .get_cte_scope_materials(
    .scope_matl = .scope_matl,
    .con        = con
  )

  # -- 3) Build the WHERE clause according to the given parameters --
  where_clause <- .get_where_clause(
    .vtype      = .vtype,
    .ftype      = .ftype,
    .material   = .material,
    .salesorg   = .salesorg,
    .scope_matl = .scope_matl,
    .scope_sorg = .scope_sorg,
    .cm_min     = .cm_min,
    .cm_max     = .cm_max,
    .step_min   = .step_min,
    .step_max   = .step_max,
    .lagg_min   = .lagg_min,
    .lagg_max   = .lagg_max,
    .con        = con
  )

  # Return as a named list
  list(
    duckdb_con          = con,
    cte_scope_materials = cte_scope_materials,
    where_clause        = where_clause
  )
}


#' Construct a SQL CTE Clause for Scope Materials
#'
#' Internal helper function that generates a Common Table Expression (CTE) SQL snippet
#' for scope materials. If \code{.scope_matl} is \code{TRUE}, the function constructs
#' the CTE SQL snippet using the \code{.get_scope_matl} function. If \code{.scope_matl}
#' is \code{FALSE}, an empty string is returned.
#'
#' @param .scope_matl Logical. Determines whether the scope materials CTE should
#'   be included in the query. If \code{TRUE}, the function constructs the SQL snippet.
#' @param .con A database connection object. Required for safely constructing
#'   the SQL snippet using \code{glue::glue_sql}.
#'
#' @return A character string containing the SQL snippet for the scope materials CTE
#'   if \code{.scope_matl} is \code{TRUE}. Returns an empty string otherwise.
#'
#' @details
#' This function relies on the \code{.get_scope_matl} function to dynamically generate
#' the SQL snippet for the scope materials CTE. The snippet is constructed using
#' \code{glue::glue_sql} for safe parameterization of inputs. If the \code{.scope_matl}
#' parameter is \code{FALSE}, the function avoids constructing the CTE and instead
#' returns an empty string.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' cte_sql <- .get_cte_scope_materials(
#'   .scope_matl = TRUE,
#'   .con = my_db_connection
#' )
#' print(cte_sql)
#' }
#'
#' @import glue
#' @importFrom DBI SQL
#' @keywords internal
.get_cte_scope_materials <- function(.scope_matl, .con) {

  # Retrieve the scope materials CTE SQL
  matl_scope <- .get_scope_matl(.con = .con)

  # Initialize the CTE SQL as empty
  cte_scope_materials <- ""

  # Generate the CTE SQL if .scope_matl is TRUE
  if (isTRUE(.scope_matl)) {
    cte_scope_materials <- glue::glue_sql(matl_scope, .con = .con)
  }

  return(cte_scope_materials)
}



#' Get (or create) a DuckDB connection
#'
#' Internal function that returns a `DBIConnection` to DuckDB. If no connection
#' exists in the `.duckdb_env`, a new one is created using \code{\link[DBI]{dbConnect}}.
#'
#' @param dbdir Character. Location of the DuckDB database file. Use \code{":memory:"}
#'   for an in-memory database.
#'
#' @return A `DBIConnection` object to DuckDB.
#'
#' @details
#' This internal helper function checks if there is an existing DuckDB connection in
#' the environment `.duckdb_env$conn`. If none is found, it initializes a new one and
#' caches it. Subsequent calls will reuse the same connection.
#'
#' @keywords internal
.get_duckdb_conn <- function(dbdir = ":memory:") {
  if (!exists("conn", envir = .duckdb_env)) {
    message("Initializing DuckDB connection...")
    .duckdb_env$conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir)
  }
  .duckdb_env$conn
}


#' Close and Remove the DuckDB Connection
#'
#' This internal helper function checks whether a DuckDB connection (stored in
#' \code{.duckdb_env$conn}) exists. If found, it closes (disconnects) the
#' DuckDB connection using \code{\link[DBI]{dbDisconnect}} and removes the
#' \code{conn} object from \code{.duckdb_env}. If no connection is present,
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
.close_duckdb_conn <- function() {
  if (exists("conn", envir = .duckdb_env)) {
    message("Closing DuckDB connection...")
    DBI::dbDisconnect(.duckdb_env$conn, shutdown = TRUE)
    rm("conn", envir = .duckdb_env)
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
#' data is determined internally via \code{.get_data_full_file_names}.
#'
#' The returned SQL defines a Common Table Expression (CTE) named \code{SCOPE_MATL},
#' which filters materials from a parquet file based on the specified \code{PRDH1} values.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' sql <- .get_scope_matl(
#'   .scope_prdh = c("PRDH001", "PRDH002"),
#'   .con = my_db_connection
#' )
#' print(sql)
#' }
#'
#' @import glue
#' @importFrom DBI SQL
#' @keywords internal
.get_scope_matl <- function(.scope_prdh = SCOPE_PRDH, .con) {

  # Validate input parameters
  if (missing(.con)) {
    stop("DuckDB connection .con must be provided.")
  }

  # Get the list of files to query for the first file type in .ftype
  FN_MATL <- .get_data_full_file_names(
    .bsgp  = 2,
    .area  = 4,
    .vtype = '010',
    .ftype = 1,
    .etype = "parquet"
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



#' Build a SQL WHERE Clause with Scope Materials
#'
#' Constructs a SQL WHERE clause by appending conditions based on the provided parameters. Additionally, it can include a Common Table Expression (CTE) for scope materials if required.
#'
#' @param .clauses A list of existing WHERE clauses (character strings). If empty, "TRUE" will be added as the initial condition.
#' @param .material Character vector. A list of materials to filter on. If \code{NULL}, no material-based filter is applied.
#' @param .salesorg Character vector. A list of sales organizations to filter on. If \code{NULL}, no sales organization-based filter is applied.
#' @param .scope_matl Logical. If \code{TRUE}, restricts the query to materials within the scope definition using a CTE.
#' @param .scope_sorg Logical. If \code{TRUE}, restricts the query to sales organizations within the scope definition.
#' @param .cm_min Character. Minimum calendar month (format: \code{YYYYMM}). If \code{NULL}, no lower bound is applied.
#' @param .cm_max Character. Maximum calendar month (format: \code{YYYYMM}). If \code{NULL}, no upper bound is applied.
#' @param .step_min Numeric. Minimum step value to filter on. If \code{NULL}, uses a default package-defined constant \code{STEP_MIN}.
#' @param .step_max Numeric. Maximum step value to filter on. If \code{NULL}, uses a default package-defined constant \code{STEP_MAX}.
#' @param .lagg_min Numeric. Minimum lag value to filter on. If \code{NULL}, uses a default package-defined constant \code{LAGG_MIN}.
#' @param .lagg_max Numeric. Maximum lag value to filter on. If \code{NULL}, uses a default package-defined constant \code{LAGG_MAX}.
#' @param .con A database connection object. Required for safely parameterizing SQL queries using \code{glue::glue_sql}.
#'
#' @details
#' This function constructs a SQL WHERE clause by:
#' \enumerate{
#'   \item Ensuring that the initial condition is "TRUE" if no clauses exist in \code{.clauses}.
#'   \item Appending conditions for \code{.material}, \code{.salesorg}, and scope definitions (\code{.scope_matl}, \code{.scope_sorg}).
#'   \item Adding conditions for calendar month (\code{.cm_min}, \code{.cm_max}), step values (\code{.step_min}, \code{.step_max}), and lag values (\code{.lagg_min}, \code{.lagg_max}).
#'   \item Optionally constructing a Common Table Expression (CTE) for scope materials if \code{.scope_matl = TRUE}.
#' }
#'
#' Scope-based filters are applied using pre-defined Common Table Expressions (CTEs).
#'
#' @return A character string representing the SQL WHERE clause.
#'
#' @examples
#' \dontrun{
#' # Build a basic WHERE clause
#' where_clause <- .get_where_clause(
#'   .material = c("MAT001", "MAT002"),
#'   .salesorg = c("SORG001"),
#'   .scope_matl = TRUE,
#'   .cm_min = "202101",
#'   .cm_max = "202312",
#'   .step_min = -100,
#'   .step_max = 100
#' )
#'
#' print(where_clause)
#' }
#'
#' @import glue
#' @importFrom DBI SQL
#' @importFrom data.table setDT
#' @keywords internal
.get_where_clause <- function(
    .clauses     = list(),  # Existing WHERE clauses
    .vtype       = NULL,    # VTYPE to filter on
    .ftype       = NULL,    # FTYPE to filter on
    .material    = NULL,    # Materials to filter on
    .salesorg    = NULL,    # Sales organizations to filter on
    .scope_matl  = FALSE,   # Scope-based material filter
    .scope_sorg  = FALSE,   # Scope-based salesorg filter
    .cm_min      = NULL,    # Minimum calendar month filter
    .cm_max      = NULL,    # Maximum calendar month filter
    .step_min    = NULL,    # Minimum step filter
    .step_max    = NULL,    # Maximum step filter
    .lagg_min    = NULL,    # Minimum lag filter
    .lagg_max    = NULL,    # Maximum lag filter
    .con         = NULL     # Database connection
) {

  # matl_scope <- .get_scope_matl(.con = .con)

  # Ensure "TRUE" is the initial condition
  if (!any(vapply(.clauses, function(x) identical(x, "TRUE"), logical(1)))) {
    .clauses <- c(.clauses, "TRUE")
  }

  # Add MATERIAL filter
  if (!is.null(.material) && length(.material) > 0) {
    .clauses <- c(
      .clauses,
      glue_sql("MATERIAL IN ({vals*})", vals = MATN1(.material), .con = .con)
    )
  }

  # Add SALESORG filter
  if (!is.null(.salesorg) && length(.salesorg) > 0) {
    .clauses <- c(
      .clauses,
      glue_sql("SALESORG IN ({vals*})", vals = .salesorg, .con = .con)
    )
  }

  # Add scope-based filters
  if (isTRUE(.scope_matl)) {
    .clauses <- c(
      .clauses,
      glue_sql("MATERIAL IN (SELECT MATERIAL FROM SCOPE_MATL)", .con = .con)
    )
  }

  if (isTRUE(.scope_sorg)) {
    .clauses <- c(
      .clauses,
      glue_sql("SALESORG IN ({vals*})", vals = SCOPE_SORG, .con = .con)
    )
  }

  # Add CALMONTH filter
  if (!is.null(.cm_min) || !is.null(.cm_max)) {
    .cm_min <- .cm_min %||% CM_MIN
    .cm_max <- .cm_max %||% CM_MAX
    .clauses <- c(
      .clauses,
      glue_sql("CALMONTH BETWEEN {.cm_min} AND {.cm_max}", .con = .con)
    )
  }

  # Initialize STEP and LAG clauses
  .step_cls <- NULL
  .lagg_cls <- NULL

  # Construct STEP filter clause for Forecasts
  if(!is.null(.vtype) && any(.vtype %in% '060')){
    if (!is.null(.step_min) || !is.null(.step_max)) {
      .step_min <- .step_min %||% STEP_MIN
      .step_max <- .step_max %||% STEP_MAX
      .step_cls <- glue_sql(
        "VTYPE = {.vtype} AND STEP BETWEEN {.step_min} AND {.step_max}",
        .con = .con
      )
    }
  }

  # Construct LAG filter clause for Actuals
  if(!is.null(.vtype) && any(.vtype %in% '060')){
    if (!is.null(.lagg_min) || !is.null(.lagg_max)) {
      .lagg_min <- .lagg_min %||% LAGG_MIN
      .lagg_max <- .lagg_max %||% LAGG_MAX
      .lagg_cls <- glue_sql(
        "VTYPE = {.vtype} AND STEP BETWEEN {.lagg_min} AND {.lagg_max}",
        .con = .con
      )
    }
  }

  # Combine STEP and LAG clauses with OR
  .comb_cls <- NULL
  if (!is.null(.step_cls) && !is.null(.lagg_cls)) {
    .comb_cls <- glue_sql("{.lagg_cls} OR {.step_cls}", .con = .con)
  } else if (!is.null(.step_cls)) {
    .comb_cls <- .step_cls
  } else if (!is.null(.lagg_cls)) {
    .comb_cls <- .lagg_cls
  }

  # Add combined clause to the existing clauses
  if (!is.null(.comb_cls)) {
    .clauses <- c(.clauses, .comb_cls)
  }

  # Combine clauses into a single WHERE clause
  paste(.clauses, collapse = " AND ")
}



.make_sql_query_dyn <-
  function(
    .vtype       = NULL    , # NULL will get both 010 and 060
    .ftype       = NULL    , # NULL will get all ftypes
    .material    = NULL    , # NULL wont apply any filter
    .salesorg    = NULL    , # NULL wont apply any filter
    .scope_matl  = FALSE   , # FALSE wont apply any filter
    .scope_sorg  = FALSE   , # FALSE wont apply any filter
    .cm_min      = '202101', # minimal Cal Month
    .cm_max      = '202506', # maximal Cal Month
    .step_min    = NULL    , # minimal forecast step ahead
    .step_max    = NULL    , # maximal forecast step ahead
    .lagg_min    = NULL    , # minimal diff. between VERSMON & MONTH
    .lagg_max    = NULL      # maximal diff. between VERSMON & MONTH
  ) {

    # Get Centralized config
    config <- .get_duckdb_parts(
      .vtype       = .vtype,
      .ftype       = .ftype,
      .material    = .material,
      .salesorg    = .salesorg,
      .scope_matl  = .scope_matl,
      .scope_sorg  = .scope_sorg,
      .cm_min      = .cm_min,
      .cm_max      = .cm_max,
      .step_min    = .step_min,
      .step_max    = .step_max,
      .lagg_min    = .lagg_min,
      .lagg_max    = .lagg_max
    )

    # Determine Files to read
    file_list <- .get_data_full_file_names(
      .bsgp  = 2, .area = 1, .vtype = .vtype, .ftype = .ftype,
      .etype = "parquet"
    )

    # Construct the query using glue_sql()
    query <- glue::glue_sql("
    {DBI::SQL(config$cte_scope_materials)}

    SELECT
      SALESORG,
      PLANT,
      MATERIAL,
      STEP,
      CALMONTH,
      VERSMON,
      FTYPE,
      VTYPE,
      BASE_UOM,
      SUM(DEMND_QTY) AS Q
    FROM
      read_parquet([{DBI::SQL(file_list)}])
    WHERE
      {DBI::SQL(config$where_clause)}
    GROUP BY
      ALL
    ORDER BY
      ALL
  ", .con = config$duckdb_con)

    return(query)
  }


#' Save a Configuration to YAML
#'
#' Internal function to save configuration settings to a `.config.yaml` file.
#'
#' @param config_list A named list of configuration items to write to the YAML file.
#' @param project_dir A character string specifying the project directory where
#'   the `.config.yaml` file is (or will be) located. Defaults to `"."`.
#'
#' @details
#' This function is used internally by the package to store key-value pairs
#' into a YAML file for persistent configuration management.
#'
#' @keywords internal
.save_config_to_yaml <- function(
    config_list,
    project_dir = ".") {

  # Normalize project directory
  project_dir <- normalizePath(project_dir, mustWork = TRUE)

  # Define the path for the .config file
  config_file <- file.path(project_dir, ".config.yaml")

  # Write the configuration data to the YAML file
  write_yaml(config_list, config_file)

  message(
    green(paste0("Configuration saved to YAML file: ", config_file)))
}

#' Upsert (Update/Insert) a Key-Value Pair in Configuration
#'
#' Internal function to add or update a single key-value pair in `.config.yaml`.
#'
#' @param .key A character string specifying the key to be updated or inserted.
#' @param .value The value to assign to the key.
#' @param .project_dir A character string specifying the project directory where
#'   the `.config.yaml` file is (or will be) located. Defaults to `"."`.
#'
#' @details
#' If the `.config.yaml` file already exists, this function updates or inserts
#' the key-value pair in that file. If the file does not exist, it is created
#' along with a minimal configuration list.
#'
#' @keywords internal
.upsert_config_in_yaml <- function(
    .key,
    .value,
    .project_dir = ".") {

  # Normalize project directory
  project_dir <- normalizePath(.project_dir, mustWork = TRUE)

  # Define the path for the .config file
  config_file <- file.path(project_dir, ".config.yaml")

  # Check if the config file exists
  if (file.exists(config_file)) {
    # Read the existing configuration
    config_data <- read_yaml(config_file)
  } else {
    # Initialize an empty configuration if the file does not exist
    config_data <- list()
  }

  # Check if the key exists and update or insert
  if (!.key %in% names(config_data)) {
    message(
      silver(paste0("Key '", .key, "' does not exist. ",
                    "Adding it to the configuration.")))
  }
  config_data[[.key]] <- .value

  # Write the updated configuration back to the YAML file
  write_yaml(config_data, config_file)

  message(
    green(paste0("Key '", .key,
                 "' updated/added in the configuration file: ", config_file)))
}

#' Copy the Config Folder from Package to Destination
#'
#' Internal function to copy the `config` folder located in the package's `extdata`
#' directory to a specified destination. If the destination already contains a
#' `config` folder, the function does nothing.
#'
#' @param destination_dir A character string specifying the directory where the
#'   `config` folder should be copied. If the folder already exists at the
#'   destination, no action is taken.
#'
#' @details
#' This function locates the `config` folder in the package's `extdata` directory,
#' verifies its existence, and copies it to the specified destination directory.
#' If the `config` folder already exists in the destination directory, the
#' function does not overwrite it. This is useful for setting up configuration
#' files during the initialization or setup phase.
#'
#' @return
#' Invisibly returns \code{TRUE} if the folder was successfully copied, or
#' \code{FALSE} if the destination already contained the folder.
#'
#' @seealso
#' \code{\link[base]{dir.create}} for directory creation,
#' \code{\link[base]{file.copy}} for file copying.
#'
#' @keywords internal
.copy_config_folder <- function(destination_dir) {

  # Define the source folder (config folder in extdata)
  # source_dir <- system.file("extdata", "config", package = "padt")
  source_dir <- file.path("C:/RW/EAISI-Padt", "inst", "extdata", "config")

  # Ensure the source directory exists in the package
  if (!dir.exists(source_dir)) {
    stop(
      red(paste0("Source config folder not found in the package.",
                 "Ensure it exists in: ", source_dir )))
  }

  # Define the destination folder path
  destination_config_dir <-
    normalizePath(file.path(destination_dir, "config"),
                  winslash = "/", mustWork = FALSE)

  # Check if the destination config folder already exists
  if (dir.exists(destination_config_dir)) {
    # message("The config folder already exists at: ", destination_config_dir)
    return(invisible(FALSE))  # Indicate that nothing was copied
  }

  # Copy the entire config folder to the destination
  dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(source_dir, destination_dir, recursive = TRUE)

  message(green(paste0("Config folder copied to: ", destination_config_dir)))

  return(invisible(TRUE))  # Indicate that the copy operation was successful
}

