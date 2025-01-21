# General ####

##  ToDO
# %>%
#   .[, .(
#     unique_id = paste(SALESORG, MATERIAL, sep = "_"),
#     ds        = CALMONTH,
#     y         = DEMND_QTY
#   )
#   ]

CM_MIN   <- '2021.01'
CM_MAX   <- '2025.06'
STEP_MIN <- 1
STEP_MAX <- 24
LAGG_MIN <- -999
LAGG_MAX <- 999

# duckdb environment
.duckdb_env <- new.env(parent = emptyenv())

# SCOPE_MATL <- MATN1('10023')
SCOPE_SORG <- c('FR30', 'NL10')
SCOPE_PRDH <- c(
  '07',  # ALTER ECO
  '08',  # BJORG
  '10',  # CLIPPER (CUPPER)
  '15',  # ZONNATURA
  '53',  # TANOSHI
  '65'   # NATURELA
)
SCOPE_MATL <-
  "WITH SCOPE_MATL AS (
     SELECT DISTINCT
       MATERIAL
     FROM
       read_parquet({`FN_MATL`})
     WHERE
       PRDH1 IN ({SCOPE_PRDH*})
     )
  "
SCOPE_PLNT <-
  "WITH SCOPE_PLNT AS (
     SELECT DISTINCT
       PLANT
     FROM
       read_parquet([{`FN_FRPR1`}, {`FN_FRPR3`}])
     WHERE
       SALESORG IN ({SCOPE_SORG*})
     )
  "

fVerbose <- function(
    function_name   = NULL,
    function_args   = NULL,
    function_return = NULL,
    verbose         = FALSE
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


#' Construct a Scope Materials CTE Clause
#'
#' Internal helper function that returns a SQL snippet (CTE) for scope materials
#' if \code{.scope_matl} is \code{TRUE}, otherwise returns an empty string.
#'
#' @param .scope_matl Logical. If \code{TRUE}, the function constructs the scope
#'   materials snippet; otherwise, it returns an empty string.
#' @param .con A database connection object. Used for safely constructing the
#'   SQL snippet using \code{glue::glue_sql}.
#'
#' @return A character string containing the SQL snippet for scope materials if
#'   \code{.scope_matl} is \code{TRUE}, otherwise an empty string.
#'
#' @keywords internal
#' @noMd
.get_cte_scope_materials <-
  function(.scope_matl, .con) {
    cte_scope_materials <- ""
    if (isTRUE(.scope_matl)) {
      cte_scope_materials <-
        glue::glue_sql(SCOPE_MATL, .con = .con)
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

#' Build a WHERE Clause
#'
#' This internal helper function builds a SQL WHERE clause by appending
#' conditions based on the provided parameters. It ensures that the final
#' WHERE clause starts with "TRUE" (a no-op condition) to simplify concatenation
#' with other clauses.
#'
#' @param .clauses A list of existing WHERE clauses (character strings). If no
#'   clauses exist, "TRUE" will be added as the initial condition.
#' @param .material A character vector of materials to filter on. If \code{NULL},
#'   no material-based filter is applied.
#' @param .salesorg A character vector of sales organizations to filter on. If
#'   \code{NULL}, no salesorg-based filter is applied.
#' @param .scope_matl Logical. If \code{TRUE}, filters on materials within the
#'   scope definition by appending a condition on the \code{MATERIAL} column.
#' @param .scope_sorg Logical. If \code{TRUE}, filters on sales organizations
#'   within the scope definition by appending a condition on the \code{SALESORG} column.
#' @param .cm_min Character. Minimum calendar month (format: \code{YYYYMM}) to
#'   include in the filter. If \code{NULL}, no lower bound is applied.
#' @param .cm_max Character. Maximum calendar month (format: \code{YYYYMM}) to
#'   include in the filter. If \code{NULL}, no upper bound is applied.
#' @param .step_min Numeric. Minimum step value to include in the filter. If
#'   \code{NULL}, defaults to a package-defined constant \code{STEP_MIN}.
#' @param .step_max Numeric. Maximum step value to include in the filter. If
#'   \code{NULL}, defaults to a package-defined constant \code{STEP_MAX}.
#' @param .lagg_min Numeric. Minimum lag value to include in the filter. If
#'   \code{NULL}, defaults to a package-defined constant \code{LAGG_MIN}.
#' @param .lagg_max Numeric. Maximum lag value to include in the filter. If
#'   \code{NULL}, defaults to a package-defined constant \code{LAGG_MAX}.
#' @param .con A database connection object. Required for safely parameterizing
#'   SQL queries with \code{glue::glue_sql}.
#'
#' @details
#' The function constructs a SQL WHERE clause based on the following logic:
#' \enumerate{
#'   \item Ensures the initial condition is "TRUE" if no clauses exist in
#'         \code{.clauses}.
#'   \item Appends conditions for \code{MATERIAL}, \code{SALESORG}, and scope
#'         definitions (\code{.scope_matl}, \code{.scope_sorg}) if specified.
#'   \item Appends calendar month conditions using \code{.cm_min} and \code{.cm_max}.
#'   \item Adds step and lag filters using \code{.step_min}, \code{.step_max},
#'         \code{.lagg_min}, and \code{.lagg_max}.
#' }
#'
#' Missing parameters default to pre-defined constants (e.g., \code{STEP_MIN}
#' or \code{LAGG_MIN}) when appropriate.
#'
#' @return A single character string containing the complete WHERE clause.
#'
#' @examples
#' # Minimal example with pseudo-code (no real DB connection):
#'
#' @keywords internal
.get_where_clause <- function(
    .clauses     = list(),  # Existing WHERE clauses
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

  # Ensure the list has "TRUE" in case no other where clauses exist.
  if (!any(
    vapply(.clauses, function(x) identical(x, "TRUE"), logical(1))
  )
  ){
    .clauses <- c(.clauses, "TRUE")
  }

  # If .material is given, filter on MATERIAL
  # leading zero's are added by function MATN1
  if (!is.null(.material) && length(.material) > 0) {
    .clauses <- c(
      .clauses,
      glue_sql(
        "MATERIAL IN ({vals*})", vals = MATN1(.material),
        .con = .con)
    )
  }

  # If .salesorg is given, filter on SALESORG
  if (!is.null(.salesorg) && length(.salesorg) > 0) {
    .clauses <- c(
      .clauses,
      glue_sql(
        "SALESORG IN ({vals*})", vals = .salesorg,
        .con = .con)
    )
  }

  # If .scope_matl is given, filter on MATERIALs in Scope
  if (isTRUE(.scope_matl)) {
    .clauses <- c(
      .clauses,
      glue::glue_sql(
        "MATERIAL IN (SELECT MATERIAL FROM SCOPE_MATL)",
        .con = .con)
    )
  }

  # If .scope_sorg is given, filter on SALESORGs in Scope
  if (isTRUE(.scope_sorg)) {
    .clauses <- c(
      .clauses,
      glue::glue_sql(
        "SALESORG IN ({vals*})", vals = SCOPE_SORG,
        .con = .con)
    )
  }

  # If .cm_min or .cm_max are given, filter on CALMONTH
  if(!is.null(.cm_min) || !is.null(.cm_max)){

    # If only .cm_max is given, set .cm_min
    if (is.null(.cm_min) || length(.cm_min) == 0) {
      .cm_min <- CM_MIN
    }

    # If only .cm_min is given, set .cm_max
    if (is.null(.cm_max) || length(.cm_max) == 0) {
      .cm_max <- CM_MAX
    }

    .clauses <- c(
      .clauses,
      glue::glue_sql(
        "CALMONTH BETWEEN {.cm_min} AND {.cm_max}",
        .con = .con)
    )
  }

  # If  one of the variables is not NULL add a filter
  if(!all(
    vapply(
      list(.step_min, .step_min, .lagg_min, .lagg_max),
      is.null, logical(1)))){

    # set .step_min if NULL
    if (is.null(.step_min) || length(.step_min) == 0) {
      .step_min <- STEP_MIN
    }

    # set .step_max if NULL
    if (is.null(.step_max) || length(.step_max) == 0) {
      .step_max <- STEP_MAX
    }

    # set .lagg_min if NULL
    if (is.null(.lagg_min) || length(.lagg_min) == 0) {
      .lagg_min <- LAGG_MIN
    }

    # set .lagg_max if NULL
    if (is.null(.lagg_max) || length(.lagg_max) == 0) {
      .lagg_max <- LAGG_MAX
    }

    .clauses <- c(
      .clauses,
      glue::glue_sql(
        "((VTYPE = '060' AND
            STEP BETWEEN {.step_min} AND {.step_max}
           ) OR
           (VTYPE = '010' AND
            STEP BETWEEN {.lagg_min} AND {.lagg_max}
           ))",
        .con = .con)
    )
  }

  # collapse list of where_clasues to 1 clause with AND
  where_clause <- paste(.clauses, collapse = " AND ")

  # Return the updated list
  return(where_clause)
}

#' Return Parquet Paths by VTYPE and FTYPE
#'
#' An internal helper function that uses a data.table lookup for valid vtype-ftype-path
#' combinations. By default, it returns all available paths if no arguments are supplied.
#'
#' @param .vtype A character vector of valid vtype codes. Defaults to \code{c("010", "060")}.
#' @param .ftype A numeric (or integer) vector of valid ftype codes. Defaults to \code{c(1,2)}.
#'
#' @return A character vector of parquet paths corresponding to all
#'   \code{(.vtype, .ftype)} pairs in the lookup.
#' @keywords internal
.get_parquet_paths <-
  function(
    .vtype = c("010", "060"),
    .ftype = c(1, 2)) {

    if(is.null(.vtype)){ .vtype <- c("010", "060")}
    if(is.null(.ftype)){ .ftype <- c(1    , 2    )}

    # Filter using data.table syntax
    # %chin% is for character matching, %in% for numeric
    result <- paths_parquet_files[
      vtype %chin% .vtype &
        ftype %in% .ftype,
      path
    ]

    # Return the matching paths
    return(result)
  }


.make_sql_query_dyn <-
  function(
    .vtype       = NULL    , # NULL will get both 010 and 060
    .ftype       = NULL    , # NULL will get all ftypes
    .material    = NULL    , # NULL wont apply any filter
    .salesorg    = NULL    , # NULL wont apply any filter
    .scope_matl  = FALSE   , # FALSE wont apply any filter
    .scope_sorg  = FALSE   ,  # FALSE wont apply any filter
    .cm_min      = '202101', # minimal Cal Month
    .cm_max      = '202506', # maximal Cal Month
    .step_min    = NULL    , # minimal forecast step ahead
    .step_max    = NULL    , # maximal forecast step ahead
    .lagg_min    = NULL    , # minimal diff. between VERSMON & MONTH
    .lagg_max    = NULL      # maximal diff. between VERSMON & MONTH
  ) {

    # Get Centralized config
    config <- .get_duckdb_parts(
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
    file_list <-
      paste0(
        "'", .get_parquet_paths(.vtype = .vtype, .ftype = .ftype), "'",
        collapse = ", "
      )

    # Construct the query using glue_sql()
    query <- glue::glue_sql("
    {DBI::SQL(config$cte_scope_materials)}

    SELECT
      SALESORG,
      PLANT,
      MATERIAL,
      date_diff(
        'month',
        -- Parse VERSMON as YYYYMM + '01' into a date
        strptime(VERSMON  || '01', '%Y%m%d'),
        -- Parse CALMONTH as YYYYMM + '01' into a date
        strptime(CALMONTH || '01', '%Y%m%d')
      ) AS STEP,
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

# Master data Functions ####
fGet_MATL <-
  function(
    .material    = NULL, # Optional user-supplied material
    .scope_matl  = TRUE, # restrict to Pythia Scope
    .n           = Inf   # number of materials to return
  ){

    # Get Centralized config
    config <- .get_duckdb_parts(
      .material   = .material,
      .scope_matl = .scope_matl
    )

    # construct Query using glue package
    query <-
      glue_sql("
          {DBI::SQL(config$cte_scope_materials)}
          SELECT
            *
          FROM
            read_parquet({`FN_MATL`})
                  WHERE
          {DBI::SQL(config$where_clause)}
        ", .con = config$duckdb_con)

    # fetch .n results and return as data.table
    dbGetQuery(config$duckdb_con, query, n = .n) %>%
      setDT()

  }

fGet_MATS <-
  function(
    .material    = NULL, # Optional user-supplied material
    .salesorg    = NULL, # Optional user-supplied salesorg
    .scope_matl  = TRUE, # restrict to Pythia Scope
    .scope_sorg  = TRUE, # restrict to Pythia Scope
    .n           = Inf   # number of materials to return
  ){

    # Get Centralized config
    config <- .get_duckdb_parts(
      .material    = .material,
      .salesorg    = .salesorg,
      .scope_matl  = .scope_matl,
      .scope_sorg  = .scope_sorg
    )

    # construct Query using glue package
    query <-
      glue_sql("
          {DBI::SQL(config$cte_scope_materials)}
          SELECT
            *
          FROM
            read_parquet({`FN_MATS`})
          WHERE
            {DBI::SQL(config$where_clause)}
        ", .con = config$duckdb_con)

    # fetch .n results and return as data.table
    dbGetQuery(config$duckdb_con, query, n = .n) %>%
      setDT()

  }

fGet_MATP <-
  function(
    .material    = NULL, # Optional user-supplied material
    .scope_matl  = TRUE, # restrict to Pythia Scope
    .n           = Inf   # number of materials to return
  ){

    # Get Centralized config
    config <- .get_duckdb_parts(
      .material   = .material,
      .scope_matl = .scope_matl
    )

    # construct Query using glue package
    query <-
      glue_sql("
          {DBI::SQL(config$cte_scope_materials)}
          SELECT
            *
          FROM
            read_parquet({`FN_MATP`})
          WHERE
            {DBI::SQL(config$where_clause)}
        ", .con = config$duckdb_con)

    # fetch .n results and return as data.table
    dbGetQuery(config$duckdb_con, query, n = .n) %>%
      setDT()

  }



# Transaction Data Functions ####

## DYN from Dynasys ####
# fGet_DYN_Actuals <-
#   function(
    #     .material    = NULL    , # Optional user-supplied material
#     .salesorg    = NULL    , # Optional user-supplied salesorg
#     .scope_matl  = TRUE    , # restrict to Pythia Scope
#     .scope_sorg  = TRUE    , # restrict to Pythia Scope
#     .cm_min      = '202101', # no data available before this date
#     .cm_max      = '202512', # no data available after this date
#     .step_min    = -999    , # no data available before this step
#     .step_max   = 999     , # no data available before this step
#     .n           = Inf       # number of materials to return
#   ){
#
#     # construct Query
#     query <- .make_sql_query_dyn(
#       .vtype       = '010',
#       .material    = .material,
#       .salesorg    = .salesorg,
#       .scope_matl  = .scope_matl,
#       .scope_sorg  = .scope_sorg
#     )
#
#     # fetch .n results and return as data.table
#     dbGetQuery(.get_duckdb_conn(), query, n = .n) %>%
#       setDT()
#
#   }

fGet_DYN <-
  function(
    .vtype       = NULL    , # NULL will get all vtypes
    .ftype       = NULL    , # NULL will get all ftypes
    .material    = NULL    , # Optional user-supplied material
    .salesorg    = NULL    , # Optional user-supplied salesorg
    .scope_matl  = TRUE    , # restrict to Pythia Scope
    .scope_sorg  = TRUE    , # restrict to Pythia Scope
    .cm_min      = NULL    , # minimal Cal Month
    .cm_max      = NULL    , # maximal Cal Month
    .step_min    = NULL    , # minimal forecast step ahead
    .step_max    = NULL    , # maximal forecast step ahead
    .lagg_min    = NULL    , # minimal diff. between VERSMON & MONTH
    .lagg_max    = NULL    , # maximal diff. between VERSMON & MONTH
    .n           = Inf       # number of materials to return
  ){

    # construct Query
    query <- .make_sql_query_dyn(
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

    # fetch .n results and return as data.table
    dbGetQuery(.get_duckdb_conn(), query, n = .n) %>%
      setDT()

  }

# fGet_DYN_Forecast <-
#   function(
    #     .material    = NULL    , # Optional user-supplied material
#     .salesorg    = NULL    , # Optional user-supplied salesorg
#     .scope_matl  = TRUE    , # restrict to Pythia Scope
#     .scope_sorg  = TRUE    , # restrict to Pythia Scope
#     .cm_min      = '202101', # no data available before this date
#     .cm_max      = '202506', # no data available after this date
#     .step_min    = -999    , # no data available before this step
#     .step_max   = 999     , # no data available before this step
#     .n           = Inf       # number of materials to return
#   ){
#
#     # construct Query
#     query <- .make_sql_query_dyn(
#       .vtype       = '060',
#       .material    = .material,
#       .salesorg    = .salesorg,
#       .scope_matl  = .scope_matl,
#       .scope_sorg  = .scope_sorg
#     )
#
#     # fetch .n results and return as data.table
#     dbGetQuery(.get_duckdb_conn(), query, n = .n) %>%
#       setDT()
#
#   }

## RTP to Dynasys  ####
fGet_RTP_Actuals <-
  function(
    .material    = NULL    , # Optional user-supplied material
    .salesorg    = NULL    , # Optional user-supplied salesorg
    .scope_matl  = TRUE    , # restrict to Pythia Scope
    .scope_sorg  = TRUE    , # restrict to Pythia Scope
    .cm_min      = '202101', # no data available before this date
    .cm_max      = '202506', # no data available after this date
    .step_min    = -999    , # no data available before this step
    .step_max   = 999     , # no data available before this step
    .n           = Inf       # number of materials to return
  ){

    # Get Centralized config
    config <- .get_duckdb_parts(
      .material   = .material,
      .scope_matl = .scope_matl
    )

    # construct Query
    query <-
      glue_sql("
        {DBI::SQL(config$cte_scope_materials)}

        SELECT
          SALESORG,
          PLANT,
          MATERIAL,
          0 AS STEP,
          CALMONTH,
          sum(SLS_QT_SO + SLS_QT_FOC) as Q
        FROM
          read_parquet([{`FN_IRTP`}])
        WHERE
          {DBI::SQL(config$where_clause)}
        GROUP BY
          ALL
        ORDER BY
          ALL
      ", .con = config$duckdb_con)

    # fetch .n results and return as data.table
    dbGetQuery(config$duckdb_con, query, n = .n) %>%
      setDT()

  }

# R Functions ####

# Open Folder ####
fOpen_Folder <-
  function(path){
    # Open the folder in Windows Explorer
    shell.exec(normalizePath(path))
  }

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
